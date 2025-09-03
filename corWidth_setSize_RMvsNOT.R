# Redundancy Masking Analysis: Quality vs Quantity Trade-offs
# Step-by-step analysis examining whether information loss in RM leads to accuracy gains

# Load required packages
library(tidyverse)
library(lme4)
library(emmeans)
library(patchwork)
library(broom.mixed)
library(viridis)

# Load the processed data
df <- read.csv("datasets/processed.csv")

# =============================================================================
# SOFT-CODED DEPENDENT VARIABLES - CHANGE THESE TO ANALYZE DIFFERENT MEASURES
# =============================================================================

# Define dependent variables to analyze (change these as needed)
dv_width <- "width_deviation"           # Options: "abs_width_deviation", "width_deviation", "width_deviation_ration"
dv_spacing <- "spacing_deviation"       # Options: "abs_spacing_deviation", "spacing_deviation", "spacing_deviation_ratio"  
dv_density <- "width_density_deviation" # Options: "abs_width_density_deviation", "width_density_deviation"

# Define labels for plots
dv_width_label <- "Width Deviation (°)"
dv_spacing_label <- "Spacing Deviation (°)"
dv_density_label <- "Density Deviation"

# Data preparation
df_filtered <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%  # Focus on RM vs no-RM trials
  mutate(
    RM_condition = ifelse(number_deviation == -1, "RM", "no-RM"),
    RM_condition = factor(RM_condition, levels = c("no-RM", "RM")),
    # FIXED: Convert correct_num to factor for proper plotting
    correct_num = factor(correct_num),
    # Add centered ratio variables
    width_deviation_ratio_centered = (as.numeric(response_width) / as.numeric(correct_width)),
    spacing_deviation_ratio_centered = (response_space / correct_space),
    correct_width = factor(correct_width),
    abs_width_deviation = abs(width_deviation),
    abs_spacing_deviation = abs(spacing_deviation), 
    abs_width_density_deviation = abs(width_density_deviation)
  ) %>%
  filter(as.numeric(response_space) > as.numeric(response_width))  %>% # Remove any rows with missing spacing values
  filter(as.numeric(response_width) >= 0.05)


# Custom theme for plots
theme_rm <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

# =============================================================================
# EXPERIMENT 1 ANALYSIS (Exp1A)
# =============================================================================

# Filter data for Experiment 1
exp1_data <- df_filtered %>% filter(exp_version == "Exp1A")

# Data overview
cat("EXPERIMENT 1 DATA OVERVIEW\n")
cat("Total trials:", nrow(exp1_data), "\n")
cat("Participants:", length(unique(exp1_data$subID)), "\n")
cat("Trials per condition:\n")
print(table(exp1_data$RM_condition, exp1_data$correct_num))
cat("Width conditions:", paste(sort(unique(exp1_data$correct_width)), collapse = ", "), "\n")
cat("Spacing range:", sprintf("%.2f to %.2f", min(exp1_data$correct_space), max(exp1_data$correct_space)), "\n\n")

# 1. REDUNDANCY MASKING PROBABILITY ANALYSIS
cat("1. REDUNDANCY MASKING PROBABILITY ANALYSIS\n")
cat("==========================================\n")

# Fit GLMM for RM probability
model_rm_prob_exp1 <- glmer(
  RM_condition ~ correct_num + correct_width + correct_space + (1|subID),
  data = exp1_data, 
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

print(summary(model_rm_prob_exp1))

# Create RM probability data for plotting (pooled across spacings)
rm_prob_data_exp1 <- exp1_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

# RM probability plot
p1_exp1 <- ggplot(rm_prob_data_exp1, aes(x = correct_num, y = rm_rate, color = correct_width, group = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Set Size",
    y = "RM Probability"
  ) +
  theme_rm

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

# Fit LMM for width accuracy
model_width_exp1 <- lmer(
  as.formula(paste(dv_width, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp1_data
)

print(summary(model_width_exp1))

# Estimated marginal means and contrasts grouped by correct_width
emmeans_width_exp1 <- emmeans(model_width_exp1, ~ RM_condition * correct_num | correct_width)
contrast_width_exp1 <- contrast(emmeans_width_exp1, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_width_exp1)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_width_exp1)

# Width accuracy plot
width_plot_data_exp1 <- as.data.frame(emmeans_width_exp1)

p2_exp1 <- ggplot(width_plot_data_exp1, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Set Size", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

# Fit LMM for spacing accuracy
model_spacing_exp1 <- lmer(
  as.formula(paste(dv_spacing, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp1_data
)

print(summary(model_spacing_exp1))

# Estimated marginal means and contrasts grouped by correct_width
emmeans_spacing_exp1 <- emmeans(model_spacing_exp1, ~ RM_condition * correct_num | correct_width)
contrast_spacing_exp1 <- contrast(emmeans_spacing_exp1, "pairwise", by = c("correct_num", "correct_width"))

cat("\nSpacing accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_spacing_exp1)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_spacing_exp1)

# Spacing accuracy plot
spacing_plot_data_exp1 <- as.data.frame(emmeans_spacing_exp1)

p3_exp1 <- ggplot(spacing_plot_data_exp1, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Set Size",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

# Fit LMM for width density accuracy
model_density_exp1 <- lmer(
  as.formula(paste(dv_density, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp1_data
)

print(summary(model_density_exp1))

# Estimated marginal means and contrasts grouped by correct_width
emmeans_density_exp1 <- emmeans(model_density_exp1, ~ RM_condition * correct_num | correct_width)
contrast_density_exp1 <- contrast(emmeans_density_exp1, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth density accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_density_exp1)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_density_exp1)

# Width density accuracy plot
density_plot_data_exp1 <- as.data.frame(emmeans_density_exp1)

p4_exp1 <- ggplot(density_plot_data_exp1, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Set Size",
    y = dv_density_label
  ) +
  theme_rm

# Combine Experiment 1 plots
plot_exp1 <- (p1_exp1 | (p2_exp1 / p3_exp1 / p4_exp1)) +
  plot_annotation(
    title = "Experiment 1",
    subtitle = "Quality vs Quantity Trade-offs in Redundancy Masking",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
    )
  )

print(plot_exp1)

# Summary for Experiment 1
cat("\nEXPERIMENT 1 SUMMARY\n")
cat("====================\n")

width_contrast_exp1_df <- as.data.frame(contrast_width_exp1)
spacing_contrast_exp1_df <- as.data.frame(contrast_spacing_exp1)
density_contrast_exp1_df <- as.data.frame(contrast_density_exp1)

cat(sprintf("Width accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(width_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp1_df$estimate[1], width_contrast_exp1_df$p.value[1]))
cat(sprintf("Spacing accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(spacing_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp1_df$estimate[1], spacing_contrast_exp1_df$p.value[1]))
cat(sprintf("Density accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(density_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp1_df$estimate[1], density_contrast_exp1_df$p.value[1]))

# =============================================================================
# EXPERIMENT 2 ANALYSIS (Exp1B)
# =============================================================================

# Filter data for Experiment 2
exp2_data <- df_filtered %>% filter(exp_version == "Exp1B")

cat("\n\n")
cat("EXPERIMENT 2 DATA OVERVIEW\n")
cat("Total trials:", nrow(exp2_data), "\n")
cat("Participants:", length(unique(exp2_data$subID)), "\n")
cat("Trials per condition:\n")
print(table(exp2_data$RM_condition, exp2_data$correct_num))
cat("Width conditions:", paste(sort(unique(exp2_data$correct_width)), collapse = ", "), "\n")
cat("Spacing range:", sprintf("%.2f to %.2f", min(exp2_data$correct_space), max(exp2_data$correct_space)), "\n\n")

# 1. REDUNDANCY MASKING PROBABILITY ANALYSIS
cat("1. REDUNDANCY MASKING PROBABILITY ANALYSIS\n")
cat("==========================================\n")

model_rm_prob_exp2 <- glmer(
  RM_condition ~ correct_num + correct_width + correct_space + (1|subID),
  data = exp2_data, 
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

print(summary(model_rm_prob_exp2))

rm_prob_data_exp2 <- exp2_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

p1_exp2 <- ggplot(rm_prob_data_exp2, aes(x = correct_num, y = rm_rate, color = correct_width, group = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Set Size",
    y = "RM Probability"
  ) +
  theme_rm

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

model_width_exp2 <- lmer(
  as.formula(paste(dv_width, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp2_data
)

print(summary(model_width_exp2))

emmeans_width_exp2 <- emmeans(model_width_exp2, ~ RM_condition * correct_num | correct_width)
contrast_width_exp2 <- contrast(emmeans_width_exp2, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_width_exp2)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_width_exp2)

width_plot_data_exp2 <- as.data.frame(emmeans_width_exp2)

p2_exp2 <- ggplot(width_plot_data_exp2, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Set Size", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

model_spacing_exp2 <- lmer(
  as.formula(paste(dv_spacing, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp2_data
)

print(summary(model_spacing_exp2))

emmeans_spacing_exp2 <- emmeans(model_spacing_exp2, ~ RM_condition * correct_num | correct_width)
contrast_spacing_exp2 <- contrast(emmeans_spacing_exp2, "pairwise", by = c("correct_num", "correct_width"))

cat("\nSpacing accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_spacing_exp2)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_spacing_exp2)

spacing_plot_data_exp2 <- as.data.frame(emmeans_spacing_exp2)

p3_exp2 <- ggplot(spacing_plot_data_exp2, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Set Size",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

model_density_exp2 <- lmer(
  as.formula(paste(dv_density, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp2_data
)

print(summary(model_density_exp2))

emmeans_density_exp2 <- emmeans(model_density_exp2, ~ RM_condition * correct_num | correct_width)
contrast_density_exp2 <- contrast(emmeans_density_exp2, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth density accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_density_exp2)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_density_exp2)

density_plot_data_exp2 <- as.data.frame(emmeans_density_exp2)

p4_exp2 <- ggplot(density_plot_data_exp2, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Set Size",
    y = dv_density_label
  ) +
  theme_rm

# Combine Experiment 2 plots
plot_exp2 <- (p1_exp2 | (p2_exp2 / p3_exp2 / p4_exp2)) +
  plot_annotation(
    title = "Experiment 2",
    subtitle = "Quality vs Quantity Trade-offs in Redundancy Masking",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
    )
  )

print(plot_exp2)

# Summary for Experiment 2
cat("\nEXPERIMENT 2 SUMMARY\n")
cat("====================\n")

width_contrast_exp2_df <- as.data.frame(contrast_width_exp2)
spacing_contrast_exp2_df <- as.data.frame(contrast_spacing_exp2)
density_contrast_exp2_df <- as.data.frame(contrast_density_exp2)

cat(sprintf("Width accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(width_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp2_df$estimate[1], width_contrast_exp2_df$p.value[1]))
cat(sprintf("Spacing accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(spacing_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp2_df$estimate[1], spacing_contrast_exp2_df$p.value[1]))
cat(sprintf("Density accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(density_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp2_df$estimate[1], density_contrast_exp2_df$p.value[1]))

# =============================================================================
# EXPERIMENT 3 ANALYSIS (Exp1C)
# =============================================================================

# Filter data for Experiment 3
exp3_data <- df_filtered %>% filter(exp_version == "Exp1C")

cat("\n\n")
cat("EXPERIMENT 3 DATA OVERVIEW\n")
cat("Total trials:", nrow(exp3_data), "\n")
cat("Participants:", length(unique(exp3_data$subID)), "\n")
cat("Trials per condition:\n")
print(table(exp3_data$RM_condition, exp3_data$correct_num))
cat("Width conditions:", paste(sort(unique(exp3_data$correct_width)), collapse = ", "), "\n")
cat("Spacing range:", sprintf("%.2f to %.2f", min(exp3_data$correct_space), max(exp3_data$correct_space)), "\n\n")

# 1. REDUNDANCY MASKING PROBABILITY ANALYSIS
cat("1. REDUNDANCY MASKING PROBABILITY ANALYSIS\n")
cat("==========================================\n")

model_rm_prob_exp3 <- glmer(
  RM_condition ~ correct_num + correct_width + correct_space + (1|subID),
  data = exp3_data, 
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

print(summary(model_rm_prob_exp3))

rm_prob_data_exp3 <- exp3_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

p1_exp3 <- ggplot(rm_prob_data_exp3, aes(x = correct_num, y = rm_rate, color = correct_width, group = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Set Size",
    y = "RM Probability"
  ) +
  theme_rm

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

model_width_exp3 <- lmer(
  as.formula(paste(dv_width, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp3_data
)

print(summary(model_width_exp3))

emmeans_width_exp3 <- emmeans(model_width_exp3, ~ RM_condition * correct_num | correct_width)
contrast_width_exp3 <- contrast(emmeans_width_exp3, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_width_exp3)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_width_exp3)

width_plot_data_exp3 <- as.data.frame(emmeans_width_exp3)

p2_exp3 <- ggplot(width_plot_data_exp3, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Set Size", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

model_spacing_exp3 <- lmer(
  as.formula(paste(dv_spacing, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp3_data
)

print(summary(model_spacing_exp3))

emmeans_spacing_exp3 <- emmeans(model_spacing_exp3, ~ RM_condition * correct_num | correct_width)
contrast_spacing_exp3 <- contrast(emmeans_spacing_exp3, "pairwise", by = c("correct_num", "correct_width"))

cat("\nSpacing accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_spacing_exp3)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_spacing_exp3)

spacing_plot_data_exp3 <- as.data.frame(emmeans_spacing_exp3)

p3_exp3 <- ggplot(spacing_plot_data_exp3, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Set Size",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

model_density_exp3 <- lmer(
  as.formula(paste(dv_density, "~ RM_condition * correct_num * correct_width + correct_space + (1|subID)")),
  data = exp3_data
)

print(summary(model_density_exp3))

emmeans_density_exp3 <- emmeans(model_density_exp3, ~ RM_condition * correct_num | correct_width)
contrast_density_exp3 <- contrast(emmeans_density_exp3, "pairwise", by = c("correct_num", "correct_width"))

cat("\nWidth density accuracy comparison (RM vs no-RM) by correct_width:\n")
print(emmeans_density_exp3)
cat("\nContrasts (positive = RM less accurate):\n")
print(contrast_density_exp3)

density_plot_data_exp3 <- as.data.frame(emmeans_density_exp3)

p4_exp3 <- ggplot(density_plot_data_exp3, aes(x = correct_num, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_width, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Set Size",
    y = dv_density_label
  ) +
  theme_rm

# Combine Experiment 3 plots
plot_exp3 <- (p1_exp3 | (p2_exp3 / p3_exp3 / p4_exp3)) +
  plot_annotation(
    title = "Experiment 3",
    subtitle = "Quality vs Quantity Trade-offs in Redundancy Masking",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
    )
  )

print(plot_exp3)

# Summary for Experiment 3
cat("\nEXPERIMENT 3 SUMMARY\n")
cat("====================\n")

width_contrast_exp3_df <- as.data.frame(contrast_width_exp3)
spacing_contrast_exp3_df <- as.data.frame(contrast_spacing_exp3)
density_contrast_exp3_df <- as.data.frame(contrast_density_exp3)

cat(sprintf("Width accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(width_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp3_df$estimate[1], width_contrast_exp3_df$p.value[1]))
cat(sprintf("Spacing accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(spacing_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp3_df$estimate[1], spacing_contrast_exp3_df$p.value[1]))
cat(sprintf("Density accuracy: %s (estimate = %.4f, p = %.4f)\n", 
            ifelse(density_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp3_df$estimate[1], density_contrast_exp3_df$p.value[1]))

# =============================================================================
# OVERALL SUMMARY ACROSS ALL EXPERIMENTS
# =============================================================================

cat("\n\n")
cat("========================================================================\n")
cat("OVERALL SUMMARY: QUALITY VS QUANTITY TRADE-OFFS ACROSS ALL EXPERIMENTS\n")
cat("========================================================================\n")

cat("\nExperiment 1 (Exp1A):\n")
cat("---------------------\n")
cat(sprintf("Width: %s (p = %.4f)\n", 
            ifelse(width_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp1_df$p.value[1]))
cat(sprintf("Spacing: %s (p = %.4f)\n", 
            ifelse(spacing_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp1_df$p.value[1]))
cat(sprintf("Density: %s (p = %.4f)\n", 
            ifelse(density_contrast_exp1_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp1_df$p.value[1]))

cat("\nExperiment 2 (Exp1B):\n")
cat("---------------------\n")
cat(sprintf("Width: %s (p = %.4f)\n", 
            ifelse(width_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp2_df$p.value[1]))
cat(sprintf("Spacing: %s (p = %.4f)\n", 
            ifelse(spacing_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp2_df$p.value[1]))
cat(sprintf("Density: %s (p = %.4f)\n", 
            ifelse(density_contrast_exp2_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp2_df$p.value[1]))

cat("\nExperiment 3 (Exp1C):\n")
cat("---------------------\n")
cat(sprintf("Width: %s (p = %.4f)\n", 
            ifelse(width_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            width_contrast_exp3_df$p.value[1]))
cat(sprintf("Spacing: %s (p = %.4f)\n", 
            ifelse(spacing_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            spacing_contrast_exp3_df$p.value[1]))
cat(sprintf("Density: %s (p = %.4f)\n", 
            ifelse(density_contrast_exp3_df$estimate[1] > 0, "RM less accurate", "RM more accurate"),
            density_contrast_exp3_df$p.value[1]))

cat("\n========================================================================\n")
cat("INTERPRETATION:\n")
cat("• Negative estimates = RM trials MORE accurate than no-RM trials\n")
cat("• Positive estimates = RM trials LESS accurate than no-RM trials\n")
cat("• This tests the core hypothesis: Does quantity loss lead to quality gain?\n")
cat("========================================================================\n")