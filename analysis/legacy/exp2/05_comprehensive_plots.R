# Comprehensive Width Deviation Analysis Visualization - Exp2
# Three-panel publication-ready figure (no spacing in exp2)

library(tidyverse)
library(ggplot2)
library(patchwork)

# Source themes
source("analysis/functions/themes.R")

# Load data and results
df <- read.csv("data/exp2/processed.csv")
width_contrast_results <- read.csv("outputs/exp2/tables/width_deviation_rm_contrasts.csv")
load("outputs/exp2/tables/bayesian_analysis_results.RData")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Common data preparation
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

# =============================================================================
# PANEL A: WIDTH DEVIATION ANALYSIS
# =============================================================================

# Calculate participant means for width deviation
width_participant_means <- analysis_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_deviation, na.rm = TRUE), .groups = "drop")

plot_data_width <- width_participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Add significance stars for width
width_contrast_results <- width_contrast_results %>%
  mutate(
    correct_width = factor(correct_width),
    correct_num = factor(correct_num),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

width_star_data <- width_contrast_results %>%
  filter(significance != "") %>%
  left_join(
    plot_data_width %>%
      group_by(correct_width, correct_num) %>%
      summarise(star_y = max(ci_upper) + 0.01, .groups = "drop"),
    by = c("correct_width", "correct_num")
  )

# Panel A: Width deviation plot
panel_a <- plot_data_width %>%
  ggplot(aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    size = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.4)
  ) +
  geom_text(
    data = width_star_data,
    aes(x = correct_num, y = star_y, label = significance),
    color = "black",
    size = 5,
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Deviation (°)") +
  labs(title = "Width Deviation by True Width and Set Size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.95, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(0.8, "lines"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

# =============================================================================
# PANEL B: BAYESIAN DENSITY ANALYSIS
# =============================================================================

# Convert condition labels for consistency
bayesian_plot_data <- bayesian_results$plot_data %>%
  mutate(
    condition = recode(condition, "NoRM" = "Non-RM"),
    condition = factor(condition, levels = c("Non-RM", "RM"))
  )

panel_b <- ggplot(bayesian_plot_data, aes(x = delta)) +
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
    size = 1,
    n = 1000
  ) +
  geom_density(
    aes(color = condition),
    size = 1.2,
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
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = bayesian_results$median_rm, color = rm_colors["RM"], linetype = "dotted", size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = bayesian_results$median_norm, color = rm_colors["Non-RM"], linetype = "dotted", size = 1.2, alpha = 0.8) +
  annotate(
    "text",
    x = bayesian_results$median_rm + 0.15, y = 1.8,
    label = paste0("BF01 = ", sprintf("%.3f", bayesian_results$bf_01_rm)),
    color = rm_colors["RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    family = "Arial"
  ) +
  annotate(
    "text",
    x = bayesian_results$median_norm - 0.05, y = 1.5,
    label = paste0("BF01 = ", sprintf("%.4f", bayesian_results$bf_01_norm)),
    color = rm_colors["Non-RM"],
    fontface = "bold",
    size = 4,
    hjust = 1,
    family = "Arial"
  ) +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Bayesian One-Sample T-Tests on Width Deviation",
    x = "Effect size (delta)",
    y = "Density"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

# =============================================================================
# PANEL C: ROBUSTNESS ANALYSIS
# =============================================================================

rob <- bayesian_results$robustness

# Panel C1: RM robustness
panel_c1 <- ggplot(rob$rm_data, aes(x = prior_scale, y = bf01)) +
  geom_hline(yintercept = rob$rm_critical_bf, color = "grey50", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = rob$rm_critical_scale, color = "grey50", linetype = "dashed", size = 0.6) +
  geom_line(color = rm_colors["RM"], size = 1.3) +
  geom_point(data = rob$rm_points, color = rm_colors["RM"], size = 2.5,
             fill = "white", shape = 21, stroke = 1.2) +
  scale_x_continuous(breaks = c(0, 0.707, rob$rm_critical_scale),
                     labels = c("0.0", "0.707", sprintf("%.2f", rob$rm_critical_scale))) +
  scale_y_continuous(breaks = c(0, 1, 3, 5)) +
  labs(title = "RM Robustness", x = "Prior scale (r)", y = "BF01") +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

# Panel C2: Non-RM robustness
norm_data_log <- rob$norm_data %>% mutate(log_bf01 = log(bf01, base = rob$log_base))
norm_points_log <- rob$norm_points %>% mutate(log_bf01 = log(bf01, base = rob$log_base))

panel_c2 <- ggplot(norm_data_log, aes(x = prior_scale, y = log_bf01)) +
  geom_hline(yintercept = log(rob$norm_critical_bf, base = rob$log_base), color = "grey50", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = rob$norm_critical_scale, color = "grey50", linetype = "dashed", size = 0.6) +
  geom_line(color = rm_colors["Non-RM"], size = 1.3) +
  geom_point(data = norm_points_log, aes(y = log_bf01), color = rm_colors["Non-RM"], size = 2.5,
             fill = "white", shape = 21, stroke = 1.2) +
  scale_x_continuous(breaks = c(0.707, rob$norm_critical_scale),
                     labels = c("0.707", sprintf("%.4f", rob$norm_critical_scale))) +
  labs(title = "Non-RM Robustness", x = "Prior scale (r)", y = expression(bold(log[10]*"(BF"[0][1]*")"))) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

# =============================================================================
# COMBINE ALL PANELS
# =============================================================================

# Create 3-panel comprehensive figure (no spacing panel for exp2)
final_plot <- panel_a / panel_b / (panel_c1 | panel_c2)

final_plot <- final_plot +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(1, 1, 0.8))

# Save comprehensive figure
if (!dir.exists('outputs/exp2/figures')) {
  dir.create('outputs/exp2/figures', recursive = TRUE)
}

ggsave(
  filename = "outputs/exp2/figures/comprehensive_width_deviation_analysis.png",
  plot = final_plot,
  width = 9,
  height = 9,
  dpi = 300,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "outputs/exp2/figures/comprehensive_width_deviation_analysis.pdf",
  plot = final_plot,
  width = 9,
  height = 9,
  units = "in",
  bg = "white"
)

cat("Comprehensive figure saved to outputs/exp2/figures/\n")
