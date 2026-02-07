# Density Deviation Plot by Set Size and Density Category - Experiment 1
# Faceted plot: rows = density category, columns = set size

library(tidyverse)

source("analysis/functions/themes.R")

rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# Load results
load("outputs/exp1/tables/density_by_condition_results.RData")

# =============================================================================
# PREPARE PLOT DATA
# =============================================================================

plot_data <- density_by_condition_results$grand_means %>%
  mutate(
    correct_num = factor(correct_num),
    density_category = factor(density_category, levels = c("Low", "Medium", "High"))
  )

# =============================================================================
# CREATE PLOT
# =============================================================================

p <- ggplot(plot_data, aes(x = correct_num, y = mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.6,
                position = position_dodge(0.4)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  facet_wrap(~ density_category, ncol = 3) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    x = "Set Size",
    y = "Density Deviation"
  ) +
  theme_scientific() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = c(0.85, 0.85)
  )

print(p)

# Save plot
ggsave("outputs/exp1/figures/density_by_setsize_and_density.png", p,
       width = 9, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_by_setsize_and_density.svg", p,
       width = 9, height = 4, bg = "white")

cat("Plot saved to outputs/exp1/figures/density_by_condition.png\n")

# =============================================================================
# PLOT 2: BY SET SIZE ONLY (AVERAGED ACROSS DENSITY)
# =============================================================================

plot_data_setsize <- density_by_condition_results$grand_means_setsize %>%
  mutate(correct_num = factor(correct_num))

p2 <- ggplot(plot_data_setsize, aes(x = correct_num, y = mean, fill = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(0.8), alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 0.8, color = "black",
                position = position_dodge(0.8)) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  labs(
    x = "Set Size",
    y = "Density Deviation"
  ) +
  theme_scientific() +
  theme(
    legend.position = c(0.85, 0.85),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

print(p2)

ggsave("outputs/exp1/figures/density_by_setsize.png", p2,
       width = 5, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_by_setsize.svg", p2,
       width = 5, height = 4, bg = "white")

cat("Plot saved to outputs/exp1/figures/density_by_setsize.png\n")
