# Redundancy Masking Analysis: Quality vs Quantity Trade-offs
# Step-by-step analysis examining whether information loss in RM leads to accuracy gains

# Load required packages
library(tidyverse)
library(patchwork)
library(viridis)
library(dplyr)
library(magrittr)
library(rlang)

# Quiet NSE notes for linters
utils::globalVariables(c(
  "RM_condition", "correct_num", "correct_width",
  "n_valid", "emmean", "se", ".data"
))

# Load the processed data
df <- read.csv("datasets/processed.csv")

# =============================================================================
# SOFT-CODED DEPENDENT VARIABLES - CHANGE THESE TO ANALYZE DIFFERENT MEASURES
# =============================================================================

# Define dependent variables to analyze (change these as needed)
dv_width <- "width_deviation"           # Options: "abs_width_deviation", "width_deviation"
dv_spacing <- "spacing_deviation"       # Options: "abs_spacing_deviation", "spacing_deviation"  
dv_density <- "width_density_deviation" # Options: "abs_width_density_deviation", "width_density_deviation"

# Relative outcome variables (used for separate relative plots)
dv_width_rel <- "width_deviation_ratio_centered"
dv_spacing_rel <- "spacing_deviation_ratio_centered"
dv_density_rel <- "width_density_deviation_ratio_centered"

# Define labels for plots
dv_width_label <- "Width Deviation (°)"
dv_spacing_label <- "Spacing Deviation (°)"
dv_density_label <- "Density Deviation"

# Relative labels
dv_width_label_rel <- "Relative Width Deviation"
dv_spacing_label_rel <- "Relative Spacing Deviation"
dv_density_label_rel <- "Relative Density Deviation"

# Data preparation
df_filtered <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%  # Focus on RM vs no-RM trials
  mutate(
    RM_condition = ifelse(number_deviation == -1, "RM", "no-RM"),
    RM_condition = factor(RM_condition, levels = c("no-RM", "RM")),
    # FIXED: Convert correct_num to factor for proper plotting
    correct_num = factor(correct_num),
    # Add centered ratio variables
    width_deviation_ratio_centered = (as.numeric(response_width) - as.numeric(correct_width)) / as.numeric(correct_width),
    spacing_deviation_ratio_centered = (as.numeric(response_space) - as.numeric(correct_space)) / as.numeric(correct_space),
    width_density_deviation_ratio_centered = (
      (as.numeric(response_width) / as.numeric(response_space)) - (as.numeric(correct_width) / as.numeric(correct_space))
    ) / (as.numeric(correct_width) / as.numeric(correct_space)),
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

# Create RM probability data for plotting (pooled across spacings)
rm_prob_data_exp1 <- exp1_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

# RM probability plot
p1_exp1 <- ggplot(rm_prob_data_exp1, aes(x = correct_width, y = rm_rate, color = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Width (°)",
    y = "RM Probability"
  ) +
  theme_rm +
  facet_wrap(~correct_num, labeller = label_both)

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

width_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_width]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_width]])),
    se = sd(.data[[dv_width]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp1 <- ggplot(width_plot_data_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Bar Width (°)", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

spacing_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_spacing]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_spacing]])),
    se = sd(.data[[dv_spacing]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp1 <- ggplot(spacing_plot_data_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Bar Width (°)",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

density_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_density]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_density]])),
    se = sd(.data[[dv_density]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp1 <- ggplot(density_plot_data_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Bar Width (°)",
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

rm_prob_data_exp2 <- exp2_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

p1_exp2 <- ggplot(rm_prob_data_exp2, aes(x = correct_width, y = rm_rate, color = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Width (°)",
    y = "RM Probability"
  ) +
  theme_rm +
  facet_wrap(~correct_num, labeller = label_both)

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

width_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_width]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_width]])),
    se = sd(.data[[dv_width]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp2 <- ggplot(width_plot_data_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Bar Width (°)", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

spacing_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_spacing]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_spacing]])),
    se = sd(.data[[dv_spacing]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp2 <- ggplot(spacing_plot_data_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Bar Width (°)",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

density_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_density]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_density]])),
    se = sd(.data[[dv_density]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp2 <- ggplot(density_plot_data_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Bar Width (°)",
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

rm_prob_data_exp3 <- exp3_data %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    .groups = "drop"
  )

p1_exp3 <- ggplot(rm_prob_data_exp3, aes(x = correct_width, y = rm_rate, color = correct_width)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Width (°)", option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Redundancy Masking Probability",
    subtitle = "Proportion of trials with number underreporting",
    x = "Width (°)",
    y = "RM Probability"
  ) +
  theme_rm +
  facet_wrap(~correct_num, labeller = label_both)

# 2. WIDTH ACCURACY ANALYSIS
cat("\n2. WIDTH ACCURACY ANALYSIS\n")
cat("==========================\n")

width_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_width]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_width]])),
    se = sd(.data[[dv_width]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp3 <- ggplot(width_plot_data_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Accuracy",
     
    x = "Bar Width (°)", 
    y = dv_width_label
  ) +
  theme_rm

# 3. SPACING ACCURACY ANALYSIS
cat("\n3. SPACING ACCURACY ANALYSIS\n")
cat("============================\n")

spacing_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_spacing]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_spacing]])),
    se = sd(.data[[dv_spacing]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp3 <- ggplot(spacing_plot_data_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Spacing Accuracy", 
     
    x = "Bar Width (°)",
    y = dv_spacing_label
  ) +
  theme_rm

# 4. WIDTH DENSITY ACCURACY ANALYSIS
cat("\n4. WIDTH DENSITY ACCURACY ANALYSIS\n")
cat("==================================\n")

density_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    emmean = mean(.data[[dv_density]], na.rm = TRUE),
    n_valid = sum(!is.na(.data[[dv_density]])),
    se = sd(.data[[dv_density]], na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp3 <- ggplot(density_plot_data_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.3), 
                width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(
    title = "Width Density Accuracy",
      
    x = "Bar Width (°)",
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

# =============================================================================
# SAVE PLOTS WITH SHORT, DISTINCT NAMES
# =============================================================================

ggsave("figures/exp1_rm.png", plot_exp1,
       width = 14, height = 10, dpi = 300, bg = "white")
ggsave("figures/exp2_rm.png", plot_exp2,
       width = 14, height = 10, dpi = 300, bg = "white")
ggsave("figures/exp3_rm.png", plot_exp3,
       width = 14, height = 10, dpi = 300, bg = "white")

# =============================================================================
# RELATIVE OUTCOMES: PLOTS AND SAVES (distinct names)
# =============================================================================

# Helper to summarise relative data
summarise_rel <- function(data, dv) {
  data %>%
    group_by(RM_condition, correct_num, correct_width) %>%
    summarise(
      emmean = mean(.data[[dv]], na.rm = TRUE),
      n_valid = sum(!is.na(.data[[dv]])),
      se = sd(.data[[dv]], na.rm = TRUE) / sqrt(n_valid),
      asymp.LCL = emmean - 1.96 * se,
      asymp.UCL = emmean + 1.96 * se,
      .groups = "drop"
    )
}

# Experiment 1 relative plots
width_rel_exp1 <- summarise_rel(exp1_data, dv_width_rel)
spacing_rel_exp1 <- summarise_rel(exp1_data, dv_spacing_rel)
density_rel_exp1 <- summarise_rel(exp1_data, dv_density_rel)

p2_exp1_rel <- ggplot(width_rel_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Accuracy", x = "Bar Width (°)", y = dv_width_label_rel) +
  theme_rm

p3_exp1_rel <- ggplot(spacing_rel_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Spacing Accuracy", x = "Bar Width (°)", y = dv_spacing_label_rel) +
  theme_rm

p4_exp1_rel <- ggplot(density_rel_exp1, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Density Accuracy", x = "Bar Width (°)", y = dv_density_label_rel) +
  theme_rm

plot_exp1_rel <- p1_exp1 + (p2_exp1_rel / p3_exp1_rel / p4_exp1_rel) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(title = "Experiment 1: Relative Outcomes")

ggsave("figures/exp1_rm_rel.png", plot_exp1_rel, width = 14, height = 10, dpi = 300, bg = "white")

# Experiment 2 relative plots
width_rel_exp2 <- summarise_rel(exp2_data, dv_width_rel)
spacing_rel_exp2 <- summarise_rel(exp2_data, dv_spacing_rel)
density_rel_exp2 <- summarise_rel(exp2_data, dv_density_rel)

p2_exp2_rel <- ggplot(width_rel_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Accuracy", x = "Bar Width (°)", y = dv_width_label_rel) +
  theme_rm

p3_exp2_rel <- ggplot(spacing_rel_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Spacing Accuracy", x = "Bar Width (°)", y = dv_spacing_label_rel) +
  theme_rm

p4_exp2_rel <- ggplot(density_rel_exp2, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Density Accuracy", x = "Bar Width (°)", y = dv_density_label_rel) +
  theme_rm

plot_exp2_rel <- p1_exp2 + (p2_exp2_rel / p3_exp2_rel / p4_exp2_rel) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(title = "Experiment 2: Relative Outcomes")

ggsave("figures/exp2_rm_rel.png", plot_exp2_rel, width = 14, height = 10, dpi = 300, bg = "white")

# Experiment 3 relative plots
width_rel_exp3 <- summarise_rel(exp3_data, dv_width_rel)
spacing_rel_exp3 <- summarise_rel(exp3_data, dv_spacing_rel)
density_rel_exp3 <- summarise_rel(exp3_data, dv_density_rel)

p2_exp3_rel <- ggplot(width_rel_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Accuracy", x = "Bar Width (°)", y = dv_width_label_rel) +
  theme_rm

p3_exp3_rel <- ggplot(spacing_rel_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Spacing Accuracy", x = "Bar Width (°)", y = dv_spacing_label_rel) +
  theme_rm

p4_exp3_rel <- ggplot(density_rel_exp3, aes(x = correct_width, y = emmean, color = RM_condition, group = RM_condition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.8) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.7, size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.3), width = 0.1, size = 0.8, alpha = 0.8) +
  facet_wrap(~correct_num, labeller = label_both) +
  scale_color_manual(name = "Condition", values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
  labs(title = "Width Density Accuracy", x = "Bar Width (°)", y = dv_density_label_rel) +
  theme_rm

plot_exp3_rel <- p1_exp3 + (p2_exp3_rel / p3_exp3_rel / p4_exp3_rel) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(title = "Experiment 3: Relative Outcomes")

ggsave("figures/exp3_rm_rel.png", plot_exp3_rel, width = 14, height = 10, dpi = 300, bg = "white")