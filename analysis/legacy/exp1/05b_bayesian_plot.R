# Bayesian Analysis Plot - Exp1
# Standalone publication-ready figure with density and robustness panels

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

# Load Bayesian results
load("outputs/exp1/tables/bayesian_analysis_results.RData")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# =============================================================================
# PANEL A: BAYESIAN DENSITY ANALYSIS
# =============================================================================

bayesian_plot_data <- bayesian_results$plot_data %>%
  mutate(
    condition = recode(condition, "NoRM" = "Non-RM"),
    condition = factor(condition, levels = c("Non-RM", "RM"))
  )

panel_density <- ggplot(bayesian_plot_data, aes(x = delta)) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area",
    fill = "grey40",
    alpha = 0.3,
    n = 1000
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey30",
    linetype = "dashed",
    linewidth = 1,
    n = 1000
  ) +
  geom_density(
    aes(color = condition),
    linewidth = 1.2,
    adjust = 1.2
  ) +
  geom_point(
    data = tibble(x = 0, y = bayesian_results$rm_density_at_zero, condition = factor("RM")),
    aes(x = x, y = y, color = condition),
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = bayesian_results$norm_density_at_zero, condition = factor("Non-RM")),
    aes(x = x, y = y, color = condition),
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = bayesian_results$prior_density_at_zero),
    aes(x = x, y = y),
    size = 3,
    shape = 21,
    fill = "white",
    color = "grey40",
    stroke = 1.5
  ) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = bayesian_results$median_rm, color = rm_colors["RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  geom_vline(xintercept = bayesian_results$median_norm, color = rm_colors["Non-RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  annotate(
    "text",
    x = bayesian_results$median_rm + 0.15, y = 1.8,
    label = paste0("BF[0][1] == ", sprintf("%.3f", bayesian_results$bf_01_rm)),
    parse = TRUE,
    color = rm_colors["RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    family = "Arial"
  ) +
  annotate(
    "text",
    x = bayesian_results$median_norm - 0.05, y = 1.5,
    label = "BF[0][1] == 3.5 %*% 10^-4",
    parse = TRUE,
    color = rm_colors["Non-RM"],
    fontface = "bold",
    size = 4,
    hjust = 1,
    family = "Arial"
  ) +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Bayesian One-Sample T-Tests on Relative Width Deviation",
    x = "Effect size (δ)",
    y = "Density"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

# =============================================================================
# PANEL B: ROBUSTNESS ANALYSIS
# =============================================================================

rob <- bayesian_results$robustness

# RM robustness
panel_rob_rm <- ggplot(rob$rm_data, aes(x = prior_scale, y = bf01)) +
  geom_hline(yintercept = rob$rm_critical_bf, color = "grey50", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = rob$rm_critical_scale, color = "grey50", linetype = "dashed", linewidth = 0.6) +
  geom_line(color = rm_colors["RM"], linewidth = 1.3) +
  geom_point(data = rob$rm_points, color = rm_colors["RM"], size = 2.5,
             fill = "white", shape = 21, stroke = 1.2) +
  scale_x_continuous(breaks = c(0, 0.707, rob$rm_critical_scale),
                     labels = c("0.0", "0.707", sprintf("%.2f", rob$rm_critical_scale))) +
  scale_y_continuous(breaks = c(0, 1, 3, 5)) +
  labs(title = "RM Robustness", x = "Prior scale (r)", y = expression(BF[0][1])) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Non-RM robustness
norm_data_log <- rob$norm_data %>% mutate(log_bf01 = log(bf01, base = rob$log_base))
norm_points_log <- rob$norm_points %>% mutate(log_bf01 = log(bf01, base = rob$log_base))

panel_rob_norm <- ggplot(norm_data_log, aes(x = prior_scale, y = log_bf01)) +
  geom_hline(yintercept = log(rob$norm_critical_bf, base = rob$log_base),
             color = "grey50", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = rob$norm_critical_scale, color = "grey50", linetype = "dashed", linewidth = 0.6) +
  geom_line(color = rm_colors["Non-RM"], linewidth = 1.3) +
  geom_point(data = norm_points_log, aes(y = log_bf01), color = rm_colors["Non-RM"], size = 2.5,
             fill = "white", shape = 21, stroke = 1.2) +
  scale_x_continuous(breaks = c(0.707, rob$norm_critical_scale),
                     labels = c("0.707", sprintf("%.4f", rob$norm_critical_scale))) +
  labs(title = "Non-RM Robustness", x = "Prior scale (r)",
       y = expression(log[10](BF[0][1]))) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# =============================================================================
# COMBINE PANELS
# =============================================================================

bayesian_plot <- panel_density / (panel_rob_rm | panel_rob_norm) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(1.2, 0.8))

print(bayesian_plot)

# Save outputs
if (!dir.exists("outputs/exp1/figures")) {
  dir.create("outputs/exp1/figures", recursive = TRUE)
}

ggsave("outputs/exp1/figures/bayesian_analysis.png",
       bayesian_plot, width = 9, height = 8, dpi = 300, bg = "white")

cat("Saved: outputs/exp1/figures/bayesian_analysis.png\n")
