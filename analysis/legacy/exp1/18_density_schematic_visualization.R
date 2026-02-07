# Schematic Visualization of Density Conditions for Set Size 3
# Shows representative bar configurations for Low, Medium, and High density

library(tidyverse)
library(ggplot2)
library(patchwork)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

# Calculate density tertile cutoffs from ALL data
density_cuts <- quantile(df$actual_width_density, probs = c(1/3, 2/3), na.rm = TRUE)

cat("Density tertile cutoffs:\n")
cat(sprintf("  Low: <= %.4f\n", density_cuts[1]))
cat(sprintf("  Medium: %.4f - %.4f\n", density_cuts[1], density_cuts[2]))
cat(sprintf("  High: > %.4f\n", density_cuts[2]))

# Filter to set size 3 and create density categories
setsize3_data <- df %>%
  filter(correct_num == 3) %>%
  mutate(
    density_category = case_when(
      actual_width_density <= density_cuts[1] ~ 'Low',
      actual_width_density <= density_cuts[2] ~ 'Medium',
      TRUE ~ 'High'
    ),
    density_category = factor(density_category, levels = c('Low', 'Medium', 'High'))
  ) %>%
  select(correct_width, correct_space, actual_width_density, stim_length, density_category) %>%
  distinct()

cat(sprintf("\nUnique configurations for set size 3: %d\n", nrow(setsize3_data)))

# =============================================================================
# SELECT REPRESENTATIVE CONFIGURATIONS
# =============================================================================

# For each density category, select the configuration closest to median density
representative_configs <- setsize3_data %>%
  group_by(density_category) %>%
  mutate(
    median_density = median(actual_width_density),
    dist_to_median = abs(actual_width_density - median_density)
  ) %>%
  slice_min(dist_to_median, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(density_category, correct_width, correct_space, actual_width_density, stim_length)

cat("\nRepresentative configurations:\n")
print(representative_configs)

# =============================================================================
# BUILD BAR RECTANGLES DATA
# =============================================================================

# Function to create bar positions for 3 bars
create_bars <- function(width, spacing, category, density) {
  # Center the display around 0
  # For 3 bars: centers at -spacing, 0, +spacing
  centers <- c(-spacing, 0, spacing)

  tibble(
    density_category = category,
    bar_id = 1:3,
    x_left = centers - width/2,
    x_right = centers + width/2,
    y_bottom = 0,
    y_top = 1,
    width_val = width,
    spacing_val = spacing,
    density_val = density
  )
}

# Create bar data for all three density levels
bar_data <- representative_configs %>%
  pmap_dfr(function(density_category, correct_width, correct_space, actual_width_density, stim_length) {
    create_bars(correct_width, correct_space, as.character(density_category), actual_width_density)
  }) %>%
  mutate(density_category = factor(density_category, levels = c('Low', 'Medium', 'High')))

# Create annotation labels
annotation_data <- representative_configs %>%
  mutate(
    label = sprintf("Width: %.2f°\nSpacing: %.2f°\nDensity: %.3f",
                    correct_width, correct_space, actual_width_density)
  )

# =============================================================================
# CREATE SCHEMATIC PLOT
# =============================================================================

# Calculate x-axis limits (same for all panels for fair comparison)
x_max <- max(abs(c(bar_data$x_left, bar_data$x_right))) * 1.1

schematic_plot <- ggplot(bar_data) +
  geom_rect(
    aes(xmin = x_left, xmax = x_right, ymin = y_bottom, ymax = y_top),
    fill = "gray30",
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(
    data = annotation_data,
    aes(x = 0, y = -0.25, label = label),
    size = 3,
    lineheight = 0.9,
    vjust = 1
  ) +
  facet_wrap(~density_category, ncol = 3) +
  coord_fixed(ratio = 1, xlim = c(-x_max, x_max), ylim = c(-0.6, 1.2), clip = "off") +
  labs(
    title = "Stimulus Configurations by Density Category (Set Size 3)",
    subtitle = "Representative examples from experimental parameters"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.margin = margin(20, 20, 20, 20),
    panel.spacing = unit(2, "lines")
  )

print(schematic_plot)

# =============================================================================
# ALTERNATIVE: SCALED TO ACTUAL STIMULUS LENGTH
# =============================================================================

# Create a version where each panel is scaled to its actual stimulus length
schematic_scaled <- ggplot(bar_data) +
  geom_rect(
    aes(xmin = x_left, xmax = x_right, ymin = y_bottom, ymax = y_top),
    fill = "gray30",
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(
    data = annotation_data,
    aes(x = 0, y = -0.25, label = label),
    size = 3,
    lineheight = 0.9,
    vjust = 1
  ) +
  facet_wrap(~density_category, ncol = 3, scales = "free_x") +
  coord_fixed(ratio = 1, ylim = c(-0.6, 1.2), clip = "off") +
  labs(
    title = "Stimulus Configurations by Density Category (Set Size 3)",
    subtitle = "Each panel scaled to its own stimulus extent"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.margin = margin(20, 20, 20, 20),
    panel.spacing = unit(2, "lines")
  )

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

# Save fixed-scale version (better for comparison)
ggsave("outputs/exp1/figures/density_schematic_setsize3.png",
       schematic_plot, width = 10, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_schematic_setsize3.pdf",
       schematic_plot, width = 10, height = 5, bg = "white")

# Save scaled version
ggsave("outputs/exp1/figures/density_schematic_setsize3_scaled.png",
       schematic_scaled, width = 10, height = 5, dpi = 300, bg = "white")

cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Saved to outputs/exp1/figures/:\n")
cat("- density_schematic_setsize3.png/pdf (fixed scale)\n")
cat("- density_schematic_setsize3_scaled.png (panel-scaled)\n")
