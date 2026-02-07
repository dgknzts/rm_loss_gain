# Performance Across Blocks Analysis
# Visualizes learning/time effects for width, spacing, and density deviation
# Compares RM vs Non-RM across experimental blocks

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

# Define block size (approximately 52 trials per block based on experiment structure)
TRIALS_PER_BLOCK <- 52

# Prepare analysis data
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  group_by(subID) %>%
  mutate(
    cumulative_trial = row_number(),
    block_number = ceiling(cumulative_trial / TRIALS_PER_BLOCK)
  ) %>%
  ungroup() %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num)
  )

# Print block structure info
cat("=== BLOCK STRUCTURE ===\n")
cat(sprintf("Trials per block: %d\n", TRIALS_PER_BLOCK))
cat(sprintf("Total trials: %d\n", nrow(analysis_data)))
cat(sprintf("Participants: %d\n", length(unique(analysis_data$subID))))
cat("\nBlocks per participant:\n")
print(analysis_data %>% group_by(subID) %>% summarise(max_block = max(block_number), .groups = "drop") %>% pull(max_block) %>% table())

# =============================================================================
# CALCULATE BLOCK-LEVEL MEANS
# =============================================================================

# Participant means per block (overall)
participant_block_means <- analysis_data %>%
  group_by(subID, block_number, rm_type) %>%
  summarise(
    width_dev = mean(width_deviation, na.rm = TRUE),
    spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    density_dev = mean(width_density_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Participant means per block by set size
participant_block_means_setsize <- analysis_data %>%
  group_by(subID, block_number, rm_type, correct_num) %>%
  summarise(
    width_dev = mean(width_deviation, na.rm = TRUE),
    spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    density_dev = mean(width_density_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Grand means with CI (overall)
grand_means <- participant_block_means %>%
  group_by(block_number, rm_type) %>%
  summarise(
    width_mean = mean(width_dev, na.rm = TRUE),
    width_se = sd(width_dev, na.rm = TRUE) / sqrt(n()),
    width_ci_lower = width_mean - 1.96 * width_se,
    width_ci_upper = width_mean + 1.96 * width_se,
    spacing_mean = mean(spacing_dev, na.rm = TRUE),
    spacing_se = sd(spacing_dev, na.rm = TRUE) / sqrt(n()),
    spacing_ci_lower = spacing_mean - 1.96 * spacing_se,
    spacing_ci_upper = spacing_mean + 1.96 * spacing_se,
    density_mean = mean(density_dev, na.rm = TRUE),
    density_se = sd(density_dev, na.rm = TRUE) / sqrt(n()),
    density_ci_lower = density_mean - 1.96 * density_se,
    density_ci_upper = density_mean + 1.96 * density_se,
    n_participants = n(),
    .groups = "drop"
  )

# Grand means with CI (by set size)
grand_means_setsize <- participant_block_means_setsize %>%
  group_by(block_number, rm_type, correct_num) %>%
  summarise(
    width_mean = mean(width_dev, na.rm = TRUE),
    width_se = sd(width_dev, na.rm = TRUE) / sqrt(n()),
    width_ci_lower = width_mean - 1.96 * width_se,
    width_ci_upper = width_mean + 1.96 * width_se,
    spacing_mean = mean(spacing_dev, na.rm = TRUE),
    spacing_se = sd(spacing_dev, na.rm = TRUE) / sqrt(n()),
    spacing_ci_lower = spacing_mean - 1.96 * spacing_se,
    spacing_ci_upper = spacing_mean + 1.96 * spacing_se,
    density_mean = mean(density_dev, na.rm = TRUE),
    density_se = sd(density_dev, na.rm = TRUE) / sqrt(n()),
    density_ci_lower = density_mean - 1.96 * density_se,
    density_ci_upper = density_mean + 1.96 * density_se,
    n_participants = n(),
    .groups = "drop"
  )

# Save summary table
if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}
write.csv(grand_means, 'outputs/exp1/tables/block_means_summary.csv', row.names = FALSE)

# =============================================================================
# PLOTTING FUNCTIONS
# =============================================================================

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

create_learning_plot <- function(data, y_mean, y_lower, y_upper, y_label, title) {
  ggplot(data, aes(x = block_number, color = rm_type, fill = rm_type)) +
    geom_ribbon(aes(ymin = .data[[y_lower]], ymax = .data[[y_upper]]),
                alpha = 0.2, color = NA) +
    geom_line(aes(y = .data[[y_mean]]), linewidth = 1.2) +
    geom_point(aes(y = .data[[y_mean]]), size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
    scale_color_manual(values = rm_colors, name = NULL) +
    scale_fill_manual(values = rm_colors, name = NULL) +
    scale_x_continuous(name = "Block", breaks = seq(1, max(data$block_number))) +
    scale_y_continuous(name = y_label) +
    labs(title = title) +
    theme_scientific(base_size = 11, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10, face = "bold")
    )
}

create_learning_plot_faceted <- function(data, y_mean, y_lower, y_upper, y_label, title) {
  ggplot(data, aes(x = block_number, color = rm_type, fill = rm_type)) +
    geom_ribbon(aes(ymin = .data[[y_lower]], ymax = .data[[y_upper]]),
                alpha = 0.2, color = NA) +
    geom_line(aes(y = .data[[y_mean]]), linewidth = 1) +
    geom_point(aes(y = .data[[y_mean]]), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
    facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
    scale_color_manual(values = rm_colors, name = NULL) +
    scale_fill_manual(values = rm_colors, name = NULL) +
    scale_x_continuous(name = "Block", breaks = seq(1, max(data$block_number))) +
    scale_y_continuous(name = y_label) +
    labs(title = title) +
    theme_scientific(base_size = 10, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 11, face = "bold"),
      strip.text = element_text(size = 9, face = "bold"),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# =============================================================================
# VISUALIZATION 1: OVERALL LEARNING CURVES
# =============================================================================

cat("\n=== CREATING OVERALL LEARNING CURVES ===\n")

# Individual plots
p_width <- create_learning_plot(
  grand_means, "width_mean", "width_ci_lower", "width_ci_upper",
  "Width Deviation (°)", "Width Deviation"
)

p_spacing <- create_learning_plot(
  grand_means, "spacing_mean", "spacing_ci_lower", "spacing_ci_upper",
  "Spacing Deviation (°)", "Spacing Deviation"
)

p_density <- create_learning_plot(
  grand_means, "density_mean", "density_ci_lower", "density_ci_upper",
  "Density Deviation", "Width Density Deviation"
)

# Combined plot
learning_overall <- (p_width | p_spacing | p_density) +
  plot_annotation(
    title = "Performance Across Experimental Blocks",
    subtitle = "Learning curves for RM vs Non-RM trials (shaded area = 95% CI)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(learning_overall)

# =============================================================================
# VISUALIZATION 2: LEARNING CURVES BY SET SIZE
# =============================================================================

cat("\n=== CREATING LEARNING CURVES BY SET SIZE ===\n")

# Faceted plots
p_width_setsize <- create_learning_plot_faceted(
  grand_means_setsize, "width_mean", "width_ci_lower", "width_ci_upper",
  "Width Deviation (°)", "Width Deviation"
)

p_spacing_setsize <- create_learning_plot_faceted(
  grand_means_setsize, "spacing_mean", "spacing_ci_lower", "spacing_ci_upper",
  "Spacing Deviation (°)", "Spacing Deviation"
)

p_density_setsize <- create_learning_plot_faceted(
  grand_means_setsize, "density_mean", "density_ci_lower", "density_ci_upper",
  "Density Deviation", "Width Density Deviation"
)

# Combined plot
learning_setsize <- (p_width_setsize / p_spacing_setsize / p_density_setsize) +
  plot_annotation(
    title = "Performance Across Blocks by Set Size",
    subtitle = "Learning curves for RM vs Non-RM trials (shaded area = 95% CI)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(learning_setsize)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

# Overall learning curves
ggsave("outputs/exp1/figures/learning_curves_overall.png",
       learning_overall, width = 12, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/learning_curves_overall.pdf",
       learning_overall, width = 12, height = 5, bg = "white")

# Learning curves by set size
ggsave("outputs/exp1/figures/learning_curves_by_setsize.png",
       learning_setsize, width = 10, height = 12, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/learning_curves_by_setsize.pdf",
       learning_setsize, width = 10, height = 12, bg = "white")

# Individual plots
ggsave("outputs/exp1/figures/learning_width_deviation.png",
       p_width, width = 6, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/learning_spacing_deviation.png",
       p_spacing, width = 6, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/learning_density_deviation.png",
       p_density, width = 6, height = 5, dpi = 300, bg = "white")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("- block_means_summary.csv\n")
cat("\nFigures saved to outputs/exp1/figures/:\n")
cat("- learning_curves_overall.png/pdf\n")
cat("- learning_curves_by_setsize.png/pdf\n")
cat("- learning_width_deviation.png\n")
cat("- learning_spacing_deviation.png\n")
cat("- learning_density_deviation.png\n")
