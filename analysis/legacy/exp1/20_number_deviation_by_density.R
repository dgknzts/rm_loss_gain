# Number Deviation by Density Analysis
# Visualizes mean number deviation as a function of actual stimulus density
# Examines whether counting errors vary systematically with density

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

# Calculate density tertile cutoffs from all data
density_cuts <- quantile(df$actual_width_density, probs = c(1/3, 2/3), na.rm = TRUE)

cat("=== DENSITY TERTILE CUTOFFS ===\n")
cat(sprintf("Low: <= %.4f\n", density_cuts[1]))
cat(sprintf("Medium: %.4f - %.4f\n", density_cuts[1], density_cuts[2]))
cat(sprintf("High: > %.4f\n", density_cuts[2]))

# Prepare analysis data
analysis_data <- df %>%
  mutate(
    correct_num = factor(correct_num),
    density_category = case_when(
      actual_width_density <= density_cuts[1] ~ 'Low',
      actual_width_density <= density_cuts[2] ~ 'Medium',
      TRUE ~ 'High'
    ),
    density_category = factor(density_category, levels = c('Low', 'Medium', 'High'))
  )

cat(sprintf("\nTotal trials: %d\n", nrow(analysis_data)))
cat(sprintf("Participants: %d\n", length(unique(analysis_data$subID))))

# Print number deviation distribution
cat("\n=== NUMBER DEVIATION DISTRIBUTION ===\n")
print(table(analysis_data$number_deviation))

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

# Participant means by density category and set size
participant_means <- analysis_data %>%
  group_by(subID, correct_num, density_category) %>%
  summarise(
    mean_num_dev = mean(number_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Grand means with 95% CI (categorical)
grand_means_categorical <- participant_means %>%
  group_by(correct_num, density_category) %>%
  summarise(
    mean = mean(mean_num_dev, na.rm = TRUE),
    sd = sd(mean_num_dev, na.rm = TRUE),
    se = sd / sqrt(n()),
    ci_lower = mean - 1.96 * se,
    ci_upper = mean + 1.96 * se,
    n_participants = n(),
    .groups = "drop"
  )

cat("\n=== SUMMARY BY DENSITY CATEGORY AND SET SIZE ===\n")
print(grand_means_categorical)

# Save summary table
if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}
write.csv(grand_means_categorical, 'outputs/exp1/tables/number_deviation_by_density_summary.csv', row.names = FALSE)

# =============================================================================
# VISUALIZATION 1: CONTINUOUS DENSITY (FACETED BY SET SIZE)
# =============================================================================

cat("\n=== CREATING CONTINUOUS DENSITY PLOT ===\n")

p_continuous <- ggplot(analysis_data, aes(x = actual_width_density, y = number_deviation)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "#4169E1", fill = "#4169E1",
              alpha = 0.2, linewidth = 1.2) +
  facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
  scale_y_continuous(name = "Mean Number Deviation") +
  scale_x_continuous(name = "Actual Density") +
  labs(
    title = "Number Deviation by Stimulus Density",
    subtitle = "Smooth curves with SE bands (LOESS fit)"
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 10, face = "bold")
  )

print(p_continuous)

# =============================================================================
# VISUALIZATION 2: CATEGORICAL DENSITY (FACETED BY SET SIZE)
# =============================================================================

cat("\n=== CREATING CATEGORICAL DENSITY PLOT ===\n")

p_categorical <- ggplot(grand_means_categorical, aes(x = density_category, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_point(size = 3.5, color = "#4169E1") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15,
                color = "#4169E1", linewidth = 0.8) +
  facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
  scale_y_continuous(name = "Mean Number Deviation") +
  scale_x_discrete(name = "Density Category") +
  labs(
    title = "Number Deviation by Density Category",
    subtitle = "Point estimates with 95% CI (from participant means)"
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 10, face = "bold")
  )

print(p_categorical)

# =============================================================================
# COMBINED FIGURE
# =============================================================================

cat("\n=== CREATING COMBINED FIGURE ===\n")

combined_plot <- (p_continuous / p_categorical) +
  plot_annotation(
    title = "Number Deviation as a Function of Stimulus Density",
    subtitle = "Negative values = undercounting (RM), Positive = overcounting, Zero = accurate",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40")
    )
  )

print(combined_plot)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

# Continuous plot
ggsave("outputs/exp1/figures/number_deviation_by_density_continuous.png",
       p_continuous, width = 10, height = 4, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/number_deviation_by_density_continuous.pdf",
       p_continuous, width = 10, height = 4, bg = "white")

# Categorical plot
ggsave("outputs/exp1/figures/number_deviation_by_density_categorical.png",
       p_categorical, width = 10, height = 4, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/number_deviation_by_density_categorical.pdf",
       p_categorical, width = 10, height = 4, bg = "white")

# Combined plot
ggsave("outputs/exp1/figures/number_deviation_by_density_combined.png",
       combined_plot, width = 10, height = 8, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/number_deviation_by_density_combined.pdf",
       combined_plot, width = 10, height = 8, bg = "white")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("- number_deviation_by_density_summary.csv\n")
cat("\nFigures saved to outputs/exp1/figures/:\n")
cat("- number_deviation_by_density_continuous.png/pdf\n")
cat("- number_deviation_by_density_categorical.png/pdf\n")
cat("- number_deviation_by_density_combined.png/pdf\n")
