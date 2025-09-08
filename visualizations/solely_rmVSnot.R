# Enhanced Redundancy Masking Analysis: Quality vs Quantity Trade-offs
# Improved visualizations with elegant design and better aesthetics

# Load required packages
library(tidyverse)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(scales)

# Load the processed data
df <- read.csv("datasets/processed.csv")

# Data preparation
df_filtered <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%  # Focus on RM vs no-RM trials
  mutate(
    RM_condition = ifelse(number_deviation == -1, "RM", "no-RM"),
    RM_condition = factor(RM_condition, levels = c("no-RM", "RM")),
    correct_width_numeric = correct_width,  # Keep numeric version for calculations
    correct_width = factor(correct_width),  # Factor version for grouping
    set_size = factor(correct_num)
  ) %>%
  filter(!is.na(correct_space))  # Remove any rows with missing spacing values

# Define elegant color palettes
rm_colors <- c("no-RM" = "#2E8B57", "RM" = "#DC143C")  # Sea green and crimson
width_colors <- c("#1B4F72", "#7D3C98", "#B7950B")  # Deep blue, purple, gold
density_colors <- viridis(3, option = "plasma", begin = 0.2, end = 0.8)

# Enhanced theme for plots
theme_elegant <- theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey92", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey20", size = 0.5),
    axis.ticks = element_line(color = "grey20", size = 0.5),
    axis.title = element_text(size = 13, face = "bold", color = "grey10"),
    axis.text = element_text(size = 11, color = "grey20"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "grey10"),
    legend.title = element_text(size = 12, face = "bold", color = "grey10"),
    legend.text = element_text(size = 11, color = "grey20"),
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 12, face = "bold", color = "grey10"),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(20, 20, 20, 20)
  )

# =============================================================================
# EXPERIMENT 1 ANALYSIS (Exp1A) - ENHANCED VERSION
# =============================================================================

exp1_data <- df_filtered %>% filter(exp_version == "Exp1A")

cat("EXPERIMENT 1 DATA OVERVIEW\n")
cat("Total trials:", nrow(exp1_data), "\n")
cat("Participants:", length(unique(exp1_data$subID)), "\n")
cat("Trials per condition:\n")
print(table(exp1_data$RM_condition, exp1_data$set_size))
cat("Width conditions:", paste(sort(unique(exp1_data$correct_width)), collapse = ", "), "\n")
cat("Spacing range:", sprintf("%.2f to %.2f", min(exp1_data$correct_space), max(exp1_data$correct_space)), "\n\n")

# 1. ENHANCED RM PROBABILITY ANALYSIS
cat("1. REDUNDANCY MASKING PROBABILITY VISUALIZATION\n")
cat("=============================================\n")

# Create RM probability data pooled across spacings as requested
rm_prob_data_exp1 <- exp1_data %>%
  group_by(set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    se = sqrt(rm_rate * (1 - rm_rate) / n_trials),  # Standard error for binomial
    ci_lower = pmax(0, rm_rate - 1.96 * se),
    ci_upper = pmin(1, rm_rate + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

# Create individual participant data for RM probability
rm_prob_individual_exp1 <- exp1_data %>%
  group_by(subID, set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

# Enhanced RM probability plot with set size on x-axis
p1_exp1 <- ggplot(rm_prob_data_exp1, aes(x = set_size_num, y = rm_rate, color = width_label)) +
  geom_point(data = rm_prob_individual_exp1, 
             aes(x = set_size_num, y = rm_rate, color = width_label),
             size = 0.8, alpha = 0.3, position = position_jitter(width = 0.05, height = 0)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.1, size = 1, alpha = 0.8) +
  scale_color_manual(values = width_colors, name = "Bar Width") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("3", "4", "5")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2)) +
  labs(
    title = "RM Probability: Set Size × Width Interaction",
    x = "Set Size (Number of Bars)",
    y = "RM Probability"
  ) +
  theme_elegant +
  theme(legend.position = "right")

# 2. WIDTH ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
width_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_deviation)),
    se = sd(width_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp1 <- ggplot(width_plot_data_exp1, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(
    title = "RM Effects on Width Deviation: Main Effects & Interactions",
    x = "Condition", 
    y = "Width Deviation (°)"
  ) +
  theme_elegant +
  theme(legend.position = "none")

# 3. SPACING ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
spacing_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(spacing_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(spacing_deviation)),
    se = sd(spacing_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp1 <- ggplot(spacing_plot_data_exp1, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(
    title = "RM Effects on Spacing Deviation: Main Effects & Interactions", 
    x = "Condition",
    y = "Spacing Deviation (°)"
  ) +
  theme_elegant +
  theme(legend.position = "none")

# 4. WIDTH DENSITY ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
density_plot_data_exp1 <- exp1_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_density_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_density_deviation)),
    se = sd(width_density_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp1 <- ggplot(density_plot_data_exp1, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(
    title = "RM Effects on Width Density Deviation: Main Effects & Interactions",
    x = "Condition",
    y = "Width Density Deviation"
  ) +
  theme_elegant +
  theme(legend.position = "none")

# Combined plot for Experiment 1 (ABSOLUTE)
plot_exp1 <- p1_exp1 + (p2_exp1 / p3_exp1 / p4_exp1) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Experiment 1: RM Effects on Absolute Width, Spacing & Density Deviations",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "grey10")
    )
  )

print(plot_exp1)

# =============================================================================
# EXPERIMENT 2 ANALYSIS (Exp1B) - ENHANCED VERSION
# =============================================================================

exp2_data <- df_filtered %>% filter(exp_version == "Exp1B")

# Enhanced RM probability plot for Exp2
rm_prob_data_exp2 <- exp2_data %>%
  group_by(set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    se = sqrt(rm_rate * (1 - rm_rate) / n_trials),
    ci_lower = pmax(0, rm_rate - 1.96 * se),
    ci_upper = pmin(1, rm_rate + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

# Create individual participant data for RM probability
rm_prob_individual_exp2 <- exp2_data %>%
  group_by(subID, set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

p1_exp2 <- ggplot(rm_prob_data_exp2, aes(x = set_size_num, y = rm_rate, color = width_label)) +
  geom_point(data = rm_prob_individual_exp2, 
             aes(x = set_size_num, y = rm_rate, color = width_label),
             size = 0.8, alpha = 0.3, position = position_jitter(width = 0.05, height = 0)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.1, size = 1, alpha = 0.8) +
  scale_color_manual(values = width_colors, name = "Bar Width") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("3", "4", "5")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2)) +
  labs(
    title = "RM Probability: Set Size × Width Interaction",
    x = "Set Size (Number of Bars)",
    y = "RM Probability"
  ) +
  theme_elegant +
  theme(legend.position = "right")

# WIDTH ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
width_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_deviation)),
    se = sd(width_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp2 <- ggplot(width_plot_data_exp2, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Width Deviation: Main Effects & Interactions", x = "Condition", y = "Width Deviation (°)") +
  theme_elegant +
  theme(legend.position = "none")

# SPACING ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
spacing_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(spacing_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(spacing_deviation)),
    se = sd(spacing_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp2 <- ggplot(spacing_plot_data_exp2, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Spacing Deviation: Main Effects & Interactions", x = "Condition", y = "Spacing Deviation (°)") +
  theme_elegant +
  theme(legend.position = "none")

# DENSITY ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
density_plot_data_exp2 <- exp2_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_density_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_density_deviation)),
    se = sd(width_density_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp2 <- ggplot(density_plot_data_exp2, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Width Density Deviation: Main Effects & Interactions", x = "Condition", y = "Width Density Deviation") +
  theme_elegant +
  theme(legend.position = "none")

plot_exp2 <- p1_exp2 + (p2_exp2 / p3_exp2 / p4_exp2) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Experiment 2: RM Effects on Absolute Width, Spacing & Density Deviations",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "grey10")
    )
  )

print(plot_exp2)

# =============================================================================
# EXPERIMENT 3 ANALYSIS (Exp1C) - ENHANCED VERSION
# =============================================================================

exp3_data <- df_filtered %>% filter(exp_version == "Exp1C")

# Enhanced RM probability plot for Exp3
rm_prob_data_exp3 <- exp3_data %>%
  group_by(set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    se = sqrt(rm_rate * (1 - rm_rate) / n_trials),
    ci_lower = pmax(0, rm_rate - 1.96 * se),
    ci_upper = pmin(1, rm_rate + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

# Create individual participant data for RM probability
rm_prob_individual_exp3 <- exp3_data %>%
  group_by(subID, set_size, correct_width) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    .groups = "drop"
  ) %>%
  mutate(
    set_size_num = as.numeric(as.character(set_size)),
    width_label = paste0(correct_width, "°")
  )

p1_exp3 <- ggplot(rm_prob_data_exp3, aes(x = set_size_num, y = rm_rate, color = width_label)) +
  geom_point(data = rm_prob_individual_exp3, 
             aes(x = set_size_num, y = rm_rate, color = width_label),
             size = 0.8, alpha = 0.3, position = position_jitter(width = 0.05, height = 0)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.1, size = 1, alpha = 0.8) +
  scale_color_manual(values = width_colors, name = "Bar Width") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("3", "4", "5")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, 0.2)) +
  labs(
    title = "RM Probability: Set Size × Width Interaction",
    x = "Set Size (Number of Bars)",
    y = "RM Probability"
  ) +
  theme_elegant +
  theme(legend.position = "right")

# WIDTH ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
width_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_deviation)),
    se = sd(width_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p2_exp3 <- ggplot(width_plot_data_exp3, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Width Deviation: Main Effects & Interactions", x = "Condition", y = "Width Deviation (°)") +
  theme_elegant + theme(legend.position = "none")

# SPACING ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
spacing_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(spacing_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(spacing_deviation)),
    se = sd(spacing_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p3_exp3 <- ggplot(spacing_plot_data_exp3, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Spacing Deviation: Main Effects & Interactions", x = "Condition", y = "Spacing Deviation (°)") +
  theme_elegant + theme(legend.position = "none")

# DENSITY ACCURACY ANALYSIS - PLOTTING DATA (ABSOLUTE)
density_plot_data_exp3 <- exp3_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_density_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_density_deviation)),
    se = sd(width_density_deviation, na.rm = TRUE) / sqrt(n_valid),
    asymp.LCL = emmean - 1.96 * se,
    asymp.UCL = emmean + 1.96 * se,
    .groups = "drop"
  )

p4_exp3 <- ggplot(density_plot_data_exp3, aes(x = RM_condition, y = emmean, fill = RM_condition)) +
  geom_col(alpha = 0.85, color = "white", size = 1, width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, size = 1, color = "grey20") +
  scale_fill_manual(values = rm_colors) +
  labs(title = "RM Effects on Width Density Deviation: Main Effects & Interactions", x = "Condition", y = "Width Density Deviation") +
  theme_elegant + theme(legend.position = "none")

plot_exp3 <- p1_exp3 + (p2_exp3 / p3_exp3 / p4_exp3) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Experiment 3: RM Effects on Absolute Width, Spacing & Density Deviations",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "grey10")
    )
  )

print(plot_exp3)

# =============================================================================
# SAVE PLOTS (ABSOLUTE ONLY)
# =============================================================================

ggsave("figures/exp1_rmvsnot_absolute.png", plot_exp1, 
       width = 14, height = 10, dpi = 300, bg = "white")
ggsave("figures/exp2_rmvsnot_absolute.png", plot_exp2, 
       width = 14, height = 10, dpi = 300, bg = "white")
ggsave("figures/exp3_rmvsnot_absolute.png", plot_exp3, 
       width = 14, height = 10, dpi = 300, bg = "white")