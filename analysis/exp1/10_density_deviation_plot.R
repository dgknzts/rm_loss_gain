# Density Deviation Plot - Experiment 1
# Showing density perception by density category (Low/Medium/High)

library(tidyverse)

source("analysis/functions/themes.R")

# RM colors
rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")
density_colors <- c("Low" = "#6BAED6", "Medium" = "#4292C6", "High" = "#2171B5")

# Load results
load("outputs/exp1/tables/density_deviation_results.RData")

# =============================================================================
# PLOT 1: DENSITY CATEGORY SPLIT (DESCRIPTIVE)
# =============================================================================

# Load raw data to show distribution
df <- read.csv("data/exp1/processed.csv") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  drop_na(actual_width_density)

# Get tertile cutoffs
tertiles <- density_results$density_tertiles

# Assign categories
df <- df %>%
  mutate(
    density_category = case_when(
      actual_width_density <= tertiles[1] ~ "Low",
      actual_width_density <= tertiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    density_category = factor(density_category, levels = c("Low", "Medium", "High"))
  )

# Create density curve data
dens <- density(df$actual_width_density, n = 512)
dens_df <- tibble(x = dens$x, y = dens$y) %>%
  mutate(
    density_category = case_when(
      x <= tertiles[1] ~ "Low",
      x <= tertiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    density_category = factor(density_category, levels = c("Low", "Medium", "High"))
  )

# Count trials per category
trial_counts <- df %>%
  count(density_category)

# Create legend labels with counts
legend_labels <- c(
  "Low" = sprintf("Low (n = %d)", trial_counts$n[trial_counts$density_category == "Low"]),
  "Medium" = sprintf("Medium (n = %d)", trial_counts$n[trial_counts$density_category == "Medium"]),
  "High" = sprintf("High (n = %d)", trial_counts$n[trial_counts$density_category == "High"])
)

# Create descriptive plot
p_desc <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_area(aes(fill = density_category), alpha = 0.7) +
  geom_histogram(linewidth = 0.5, color = "grey30") +
  geom_vline(xintercept = tertiles[1], linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = tertiles[2], linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = density_colors, labels = legend_labels, name = "Category") +
  labs(
    x = "Actual Density",
    y = NULL
  ) +
  theme_scientific() +
  theme(
    legend.position = c(0.85, 0.85),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

print(p_desc)

ggsave("outputs/exp1/figures/density_category_split.png", p_desc,
       width = 5, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_category_split.svg", p_desc,
       width = 5, height = 4, bg = "white")

# =============================================================================
# PLOT 2: DENSITY DEVIATION BY DENSITY CATEGORY
# =============================================================================

plot_data <- density_results$grand_means

# Create BF annotation data
bf_data <- data.frame(
  density_category = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
  bf_01 = c(
    density_results$bayesian_by_category$Low$bf_01,
    density_results$bayesian_by_category$Medium$bf_01,
    density_results$bayesian_by_category$High$bf_01
  )
) %>%
  left_join(
    plot_data %>%
      group_by(density_category) %>%
      summarise(y_pos = max(ci_upper) + 0.015, .groups = "drop"),
    by = "density_category"
  )

# Create plot
p <- ggplot(plot_data, aes(x = density_category, y = mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, linewidth = 0.5,
                position = position_dodge(width = 0.4)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.4)) +
  geom_text(data = bf_data,
            aes(x = density_category, y = y_pos,
                label = sprintf("BF01=%.1f", bf_01)),
            color = "black", size = 4, inherit.aes = FALSE) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(x = "Density Category", y = "Density Deviation") +
  theme_scientific() +
  theme(
    legend.position = c(0.15, 0.15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

print(p)

ggsave("outputs/exp1/figures/density_deviation.png", p,
       width = 5, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_deviation.svg", p,
       width = 5, height = 4, bg = "white")
