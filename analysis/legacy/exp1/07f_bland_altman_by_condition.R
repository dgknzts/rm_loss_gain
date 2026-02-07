# Bland-Altman Plots by Different Conditions
# Width Density Deviation: RM vs Non-RM Agreement

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(width_density_deviation)

if (!dir.exists('outputs/exp1/figures/density_alternatives')) {
  dir.create('outputs/exp1/figures/density_alternatives', recursive = TRUE)
}

# =============================================================================
# FIRST: Calculate global axis limits from all data
# =============================================================================

# Calculate BA data for all participants (overall)
ba_all_data <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density = mean(width_density_deviation, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = rm_type, values_from = mean_density) %>%
  filter(!is.na(`Non-RM`) & !is.na(RM)) %>%
  mutate(
    difference = `Non-RM` - RM,
    mean_value = (`Non-RM` + RM) / 2
  )

# Global axis limits (with some padding)
global_xlim <- c(

  min(ba_all_data$mean_value, na.rm = TRUE) - 0.02,
  max(ba_all_data$mean_value, na.rm = TRUE) + 0.02
)

global_ylim <- c(

  min(ba_all_data$difference, na.rm = TRUE) - 0.02,
  max(ba_all_data$difference, na.rm = TRUE) + 0.02
)

cat(sprintf("Global X-axis limits: [%.3f, %.3f]\n", global_xlim[1], global_xlim[2]))
cat(sprintf("Global Y-axis limits: [%.3f, %.3f]\n", global_ylim[1], global_ylim[2]))

# =============================================================================
# FUNCTION: Calculate Bland-Altman stats and create plot with fixed axes
# =============================================================================

create_ba_plot <- function(data, title_suffix = "", xlim = NULL, ylim = NULL) {

  # Calculate participant means by RM type
  ba_data <- data %>%
    group_by(subID, rm_type) %>%
    summarise(mean_density = mean(width_density_deviation, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = rm_type, values_from = mean_density) %>%
    filter(!is.na(`Non-RM`) & !is.na(RM)) %>%
    mutate(
      difference = `Non-RM` - RM,
      mean_value = (`Non-RM` + RM) / 2
    )

  if (nrow(ba_data) < 3) return(NULL)

  # Calculate statistics
  bias <- mean(ba_data$difference, na.rm = TRUE)
  sd_diff <- sd(ba_data$difference, na.rm = TRUE)
  upper_loa <- bias + 1.96 * sd_diff
  lower_loa <- bias - 1.96 * sd_diff

  # Create plot
  p <- ba_data %>%
    ggplot(aes(x = mean_value, y = difference)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = lower_loa, ymax = upper_loa,
             fill = "lightblue", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.5) +
    geom_hline(yintercept = bias, linetype = "solid", color = "#E74C3C", linewidth = 0.8) +
    geom_hline(yintercept = upper_loa, linetype = "dashed", color = "#3498DB", linewidth = 0.6) +
    geom_hline(yintercept = lower_loa, linetype = "dashed", color = "#3498DB", linewidth = 0.6) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = title_suffix,
      subtitle = sprintf("Bias = %.3f", bias),
      x = "Mean",
      y = "Difference"
    ) +
    theme_scientific(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "#E74C3C")
    )

  # Apply fixed axis limits if provided
  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim)
  }
  if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }

  return(list(plot = p, bias = bias, sd = sd_diff, upper = upper_loa, lower = lower_loa, n = nrow(ba_data)))
}

# =============================================================================
# 1. BLAND-ALTMAN BY WIDTH (with fixed axes)
# =============================================================================

ba_by_width <- map(levels(analysis_data$correct_width), function(w) {
  data_subset <- analysis_data %>% filter(correct_width == w)
  create_ba_plot(data_subset, paste0("Width: ", w, "°"), xlim = global_xlim, ylim = global_ylim)
})

ba_width_combined <- (ba_by_width[[1]]$plot | ba_by_width[[2]]$plot | ba_by_width[[3]]$plot) +
  plot_annotation(
    title = "Bland-Altman by True Width",
    subtitle = "Red line = bias | Blue lines = 95% Limits of Agreement",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_by_width.png",
       ba_width_combined, width = 12, height = 4, dpi = 300, bg = "white")

# =============================================================================
# 2. BLAND-ALTMAN BY SET SIZE (with fixed axes)
# =============================================================================

ba_by_setsize <- map(levels(analysis_data$correct_num), function(n) {
  data_subset <- analysis_data %>% filter(correct_num == n)
  create_ba_plot(data_subset, paste0("Set Size: ", n), xlim = global_xlim, ylim = global_ylim)
})

ba_setsize_combined <- (ba_by_setsize[[1]]$plot | ba_by_setsize[[2]]$plot | ba_by_setsize[[3]]$plot) +
  plot_annotation(
    title = "Bland-Altman by Set Size",
    subtitle = "Red line = bias | Blue lines = 95% Limits of Agreement",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_by_setsize.png",
       ba_setsize_combined, width = 12, height = 4, dpi = 300, bg = "white")

# =============================================================================
# 3. BLAND-ALTMAN BY SPACING (with fixed axes)
# =============================================================================

ba_by_spacing <- map(levels(analysis_data$spacing_category), function(s) {
  data_subset <- analysis_data %>% filter(spacing_category == s)
  create_ba_plot(data_subset, paste0("Spacing: ", s), xlim = global_xlim, ylim = global_ylim)
})

ba_spacing_combined <- (ba_by_spacing[[1]]$plot | ba_by_spacing[[2]]$plot | ba_by_spacing[[3]]$plot) +
  plot_annotation(
    title = "Bland-Altman by Spacing Category",
    subtitle = "Red line = bias | Blue lines = 95% Limits of Agreement",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_by_spacing.png",
       ba_spacing_combined, width = 12, height = 4, dpi = 300, bg = "white")

# =============================================================================
# 4. BLAND-ALTMAN GRID: WIDTH × SET SIZE (with fixed axes)
# =============================================================================

ba_grid_data <- expand.grid(
  width = levels(analysis_data$correct_width),
  setsize = levels(analysis_data$correct_num)
)

ba_grid_plots <- map2(ba_grid_data$width, ba_grid_data$setsize, function(w, n) {
  data_subset <- analysis_data %>% filter(correct_width == w, correct_num == n)
  create_ba_plot(data_subset, paste0("W:", w, "° N:", n), xlim = global_xlim, ylim = global_ylim)
})

ba_grid_combined <- wrap_plots(map(ba_grid_plots, ~.x$plot), ncol = 3) +
  plot_annotation(
    title = "Bland-Altman Grid: Width × Set Size",
    subtitle = "Red line = bias | Blue lines = 95% LoA | All biases near zero = no systematic difference",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_grid_width_setsize.png",
       ba_grid_combined, width = 12, height = 10, dpi = 300, bg = "white")

# =============================================================================
# 5. SUMMARY TABLE OF ALL BIASES
# =============================================================================

# Collect all bias values
summary_data <- tibble(
  Condition = c(
    paste0("Width: ", levels(analysis_data$correct_width), "°"),
    paste0("Set Size: ", levels(analysis_data$correct_num)),
    paste0("Spacing: ", levels(analysis_data$spacing_category))
  ),
  Bias = c(
    map_dbl(ba_by_width, ~.x$bias),
    map_dbl(ba_by_setsize, ~.x$bias),
    map_dbl(ba_by_spacing, ~.x$bias)
  ),
  SD = c(
    map_dbl(ba_by_width, ~.x$sd),
    map_dbl(ba_by_setsize, ~.x$sd),
    map_dbl(ba_by_spacing, ~.x$sd)
  ),
  Lower_LoA = c(
    map_dbl(ba_by_width, ~.x$lower),
    map_dbl(ba_by_setsize, ~.x$lower),
    map_dbl(ba_by_spacing, ~.x$lower)
  ),
  Upper_LoA = c(
    map_dbl(ba_by_width, ~.x$upper),
    map_dbl(ba_by_setsize, ~.x$upper),
    map_dbl(ba_by_spacing, ~.x$upper)
  )
)

cat("\n=== Bland-Altman Summary by Condition ===\n")
print(summary_data, n = Inf)

# Bias summary plot
bias_summary_plot <- summary_data %>%
  mutate(Condition = fct_reorder(Condition, Bias)) %>%
  ggplot(aes(x = Bias, y = Condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower_LoA, xmax = Upper_LoA), height = 0.3, color = "grey60") +
  geom_point(size = 4, color = "#E74C3C") +
  labs(
    title = "Summary: Bias Across All Conditions",
    subtitle = "All biases near zero with overlapping LoA = consistent agreement",
    x = "Bias (Non-RM − RM)",
    y = NULL
  ) +
  theme_scientific(base_size = 12) +
  theme(plot.subtitle = element_text(size = 10, color = "grey40"))

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_bias_summary.png",
       bias_summary_plot, width = 8, height = 5, dpi = 300, bg = "white")

# =============================================================================
# OUTPUT SUMMARY
# =============================================================================

cat("\n=== Bland-Altman Plots Generated ===\n")
cat("1. bland_altman_by_width.png - 3 panels by width\n")
cat("2. bland_altman_by_setsize.png - 3 panels by set size\n")
cat("3. bland_altman_by_spacing.png - 3 panels by spacing\n")
cat("4. bland_altman_grid_width_setsize.png - 9-panel grid\n")
cat("5. bland_altman_bias_summary.png - Summary of all biases\n")
