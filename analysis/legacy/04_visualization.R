# ==============================================================================
# REDUNDANCY MASKING STUDY - PUBLICATION FIGURES
# ==============================================================================
# This script generates all publication-ready figures for exp1
# Input: Processed dataset from data/processed.csv
# Output: Publication figures saved to outputs/figures/
# ==============================================================================

library(tidyverse)
library(patchwork)
library(viridis)
library(scales)

cat("Generating publication figures for Exp1...\n\n")

# Load processed data
df <- read.csv("data/processed.csv")

# ==============================================================================
# THEME AND COLOR DEFINITIONS
# ==============================================================================

# Scientific publication theme
theme_publication <- theme_minimal(base_size = 12) +
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
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.title = element_text(size = 12, face = "bold", color = "grey10"),
    legend.text = element_text(size = 11, color = "grey20"),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "grey10"),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Color palettes
rm_colors <- c("NoRM" = "#2E8B57", "RM" = "#DC143C")  # Sea green and crimson
width_colors <- viridis(3, option = "plasma", begin = 0.2, end = 0.8)

# ==============================================================================
# DATA PREPARATION FOR VISUALIZATION
# ==============================================================================

cat("Preparing data for visualization...\n")

# Filter and prepare data for RM visualization
df_viz <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%  # Focus on RM vs NoRM trials
  mutate(
    RM_condition = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                         levels = c("NoRM", "RM")),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Small",
      correct_space <= 0.9 ~ "Medium",
      TRUE ~ "Large"
    ),
    spacing_category = factor(spacing_category, levels = c("Small", "Medium", "Large")),
    # Relative deviations for normalized comparisons
    width_deviation_relative = width_deviation / correct_width,
    spacing_deviation_relative = spacing_deviation / correct_space,
    density_deviation_relative = width_density_deviation / (correct_width / correct_space)
  )

cat("Visualization sample: ", nrow(df_viz), " trials from ", length(unique(df_viz$subID)), " participants\n\n")

# ==============================================================================
# FIGURE 1: RM PROBABILITY BY EXPERIMENTAL CONDITIONS
# ==============================================================================

cat("Creating Figure 1: RM Probability Analysis...\n")

# Calculate RM probability by conditions
rm_prob_data <- df_viz %>%
  group_by(correct_num, correct_width, spacing_category) %>%
  summarise(
    rm_rate = mean(RM_condition == "RM"),
    n_trials = n(),
    se = sqrt(rm_rate * (1 - rm_rate) / n_trials),
    ci_lower = pmax(0, rm_rate - 1.96 * se),
    ci_upper = pmin(1, rm_rate + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(set_size_num = as.numeric(as.character(correct_num)))

# Individual participant data for background points
rm_prob_individual <- df_viz %>%
  group_by(subID, correct_num, correct_width) %>%
  summarise(rm_rate = mean(RM_condition == "RM"), .groups = "drop") %>%
  mutate(set_size_num = as.numeric(as.character(correct_num)))

# Create RM probability plot
fig1 <- ggplot(rm_prob_data, aes(x = set_size_num, y = rm_rate, color = correct_width)) +
  geom_point(data = rm_prob_individual, 
             aes(x = set_size_num, y = rm_rate, color = correct_width),
             size = 0.8, alpha = 0.3, position = position_jitter(width = 0.05, height = 0)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.1, size = 1, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.9) +
  geom_line(size = 1.2, alpha = 0.8) +
  facet_wrap(~ spacing_category, labeller = label_both) +
  scale_color_manual(values = width_colors, name = "Bar Width (°)") +
  scale_x_continuous(breaks = c(3, 4, 5), labels = c("3", "4", "5")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Redundancy Masking Probability by Experimental Conditions",
    subtitle = "Individual participants (small points) and group means with 95% CI",
    x = "Set Size (Number of Bars)",
    y = "RM Probability"
  ) +
  theme_publication

# ==============================================================================
# FIGURE 2: PRIMARY RM EFFECTS ON WIDTH DEVIATION
# ==============================================================================

cat("Creating Figure 2: Primary RM Effects...\n")

# Calculate means and CIs for primary RM effect
primary_effects <- df_viz %>%
  group_by(RM_condition) %>%
  summarise(
    n = n(),
    mean_width = mean(width_deviation, na.rm = TRUE),
    se_width = sd(width_deviation, na.rm = TRUE) / sqrt(n),
    ci_lower = mean_width - 1.96 * se_width,
    ci_upper = mean_width + 1.96 * se_width,
    .groups = "drop"
  )

# Create primary effects plot
fig2a <- ggplot(primary_effects, aes(x = RM_condition, y = mean_width, fill = RM_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_col(alpha = 0.8, color = "white", size = 1, width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, color = "black", alpha = 0.8, size = 1) +
  scale_fill_manual(values = rm_colors) +
  labs(
    title = "Primary RM Effect on Width Deviation",
    subtitle = "Main effect comparison with 95% CI",
    x = "Condition",
    y = "Width Deviation (degrees)"
  ) +
  theme_publication +
  theme(legend.position = "none")

# RM effects by key factors
rm_by_factors <- df_viz %>%
  group_by(RM_condition, correct_num, correct_width) %>%
  summarise(
    mean_width = mean(width_deviation, na.rm = TRUE),
    se_width = sd(width_deviation, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean_width - 1.96 * se_width,
    ci_upper = mean_width + 1.96 * se_width,
    .groups = "drop"
  )

# RM effects by set size and width
fig2b <- ggplot(rm_by_factors, aes(x = correct_width, y = mean_width, 
                                   color = RM_condition, group = RM_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.1, size = 0.8, alpha = 0.8,
                position = position_dodge(width = 0.3)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.3)) +
  geom_line(size = 1, alpha = 0.8, position = position_dodge(width = 0.3)) +
  facet_wrap(~ correct_num, labeller = label_both) +
  scale_color_manual(values = rm_colors, name = "Condition") +
  labs(
    title = "RM Effects by Set Size and Bar Width",
    subtitle = "Interaction patterns with 95% CI",
    x = "Bar Width (degrees)",
    y = "Width Deviation (degrees)"
  ) +
  theme_publication

# Combine primary effects
fig2 <- (fig2a | fig2b) + 
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "Primary Analysis: Redundancy Masking Effects on Width Judgment",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# ==============================================================================
# FIGURE 3: MULTIPLE OUTCOME VARIABLES
# ==============================================================================

cat("Creating Figure 3: Multiple Outcome Variables...\n")

# Calculate means for multiple outcomes
outcomes_data <- df_viz %>%
  select(RM_condition, width_deviation, spacing_deviation, 
         pooled_width_deviation, width_density_deviation) %>%
  pivot_longer(cols = -RM_condition, names_to = "outcome", values_to = "value") %>%
  group_by(RM_condition, outcome) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se_value = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    ci_lower = mean_value - 1.96 * se_value,
    ci_upper = mean_value + 1.96 * se_value,
    .groups = "drop"
  ) %>%
  mutate(
    outcome_label = case_when(
      outcome == "width_deviation" ~ "Width\nDeviation",
      outcome == "spacing_deviation" ~ "Spacing\nDeviation", 
      outcome == "pooled_width_deviation" ~ "Pooled Width\nDeviation",
      outcome == "width_density_deviation" ~ "Width Density\nDeviation"
    ),
    outcome_label = factor(outcome_label, levels = c("Width\nDeviation", "Spacing\nDeviation", 
                                                   "Pooled Width\nDeviation", "Width Density\nDeviation"))
  )

# Multiple outcomes plot
fig3 <- ggplot(outcomes_data, aes(x = outcome_label, y = mean_value, fill = RM_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, color = "white", size = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.8), width = 0.3, 
                color = "black", alpha = 0.8, size = 0.8) +
  scale_fill_manual(values = rm_colors, name = "Condition") +
  labs(
    title = "RM Effects Across Multiple Outcome Variables",
    subtitle = "Comparison of RM vs NoRM effects with 95% CI",
    x = "Outcome Variable",
    y = "Deviation from Correct Value"
  ) +
  theme_publication +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ==============================================================================
# FIGURE 4: RELATIVE DEVIATIONS
# ==============================================================================

cat("Creating Figure 4: Relative Deviation Analysis...\n")

# Calculate relative deviations
relative_data <- df_viz %>%
  select(RM_condition, width_deviation_relative, spacing_deviation_relative) %>%
  pivot_longer(cols = -RM_condition, names_to = "outcome", values_to = "value") %>%
  filter(!is.na(value), is.finite(value)) %>%
  group_by(RM_condition, outcome) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se_value = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    ci_lower = mean_value - 1.96 * se_value,
    ci_upper = mean_value + 1.96 * se_value,
    .groups = "drop"
  ) %>%
  mutate(
    outcome_label = case_when(
      outcome == "width_deviation_relative" ~ "Relative Width\nDeviation",
      outcome == "spacing_deviation_relative" ~ "Relative Spacing\nDeviation"
    )
  )

# Relative deviations plot
fig4 <- ggplot(relative_data, aes(x = outcome_label, y = mean_value, fill = RM_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, color = "white", size = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.8), width = 0.3, 
                color = "black", alpha = 0.8, size = 0.8) +
  scale_fill_manual(values = rm_colors, name = "Condition") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Relative Deviation Analysis",
    subtitle = "Normalized deviations as proportion of correct values",
    x = "Relative Deviation Type",
    y = "Relative Deviation (%)"
  ) +
  theme_publication

# ==============================================================================
# SAVE ALL FIGURES
# ==============================================================================

cat("Saving publication figures...\n")

# Create output directory
if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

# Save individual figures
ggsave("outputs/figures/fig1_rm_probability.png", fig1, 
       width = 14, height = 8, dpi = 300, bg = "white")

ggsave("outputs/figures/fig2_primary_rm_effects.png", fig2, 
       width = 16, height = 8, dpi = 300, bg = "white")

ggsave("outputs/figures/fig3_multiple_outcomes.png", fig3, 
       width = 12, height = 8, dpi = 300, bg = "white")

ggsave("outputs/figures/fig4_relative_deviations.png", fig4, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Create combined manuscript figure
manuscript_figure <- (fig1 / fig2) | (fig3 / fig4)
ggsave("outputs/figures/manuscript_combined_figures.png", manuscript_figure,
       width = 20, height = 16, dpi = 300, bg = "white")

# Save high-resolution versions for publication
ggsave("outputs/figures/fig1_rm_probability_highres.pdf", fig1, 
       width = 14, height = 8, dpi = 600, bg = "white")
ggsave("outputs/figures/fig2_primary_rm_effects_highres.pdf", fig2, 
       width = 16, height = 8, dpi = 600, bg = "white")

cat("\nVisualization complete!\n")
cat("Publication figures saved to outputs/figures/:\n")
cat("- fig1_rm_probability.png - RM probability by conditions\n")
cat("- fig2_primary_rm_effects.png - Primary RM effects analysis\n")
cat("- fig3_multiple_outcomes.png - Multiple outcome variables\n")
cat("- fig4_relative_deviations.png - Relative deviation analysis\n")
cat("- manuscript_combined_figures.png - All figures combined\n")
cat("- High-resolution PDF versions for publication\n")
