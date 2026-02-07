# Two-Panel Width Deviation Analysis Visualization - Exp2
# Panel A: Absolute Width Deviation (by true width)
# Panel B: Bayesian on Relative Width Deviation (pooled across widths)

library(tidyverse)
library(ggplot2)
library(patchwork)
library(BayesFactor)

source("analysis/functions/themes.R")

df <- read.csv("data/exp2/processed.csv")

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation, width_deviation_relative)

# =============================================================================
# PANEL A: WIDTH DEVIATION ANALYSIS (ABSOLUTE)
# =============================================================================

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
# PANEL B: BAYESIAN DENSITY ANALYSIS (RELATIVE WIDTH DEVIATION)
# =============================================================================

subject_means <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(
    mean_width_deviation_relative = mean(width_deviation_relative, na.rm = TRUE),
    .groups = "drop"
  )

rm_means <- subject_means %>%
  filter(rm_type == "RM") %>%
  pull(mean_width_deviation_relative)

norm_means <- subject_means %>%
  filter(rm_type == "Non-RM") %>%
  pull(mean_width_deviation_relative)

bf_rm <- ttestBF(x = rm_means, mu = 0)
bf_norm <- ttestBF(x = norm_means, mu = 0)

posterior_rm <- posterior(bf_rm, iterations = 10000)
posterior_norm <- posterior(bf_norm, iterations = 10000)

delta_rm <- as.numeric(posterior_rm[, "delta"])
delta_norm <- as.numeric(posterior_norm[, "delta"])

median_rm <- median(delta_rm)
median_norm <- median(delta_norm)
bf_01_rm <- 1 / exp(bf_rm@bayesFactor$bf)
bf_01_norm <- 1 / exp(bf_norm@bayesFactor$bf)

rm_density <- density(delta_rm, adjust = 1.2)
norm_density <- density(delta_norm, adjust = 1.2)
rm_density_at_zero <- rm_density$y[which.min(abs(rm_density$x - 0))]
norm_density_at_zero <- norm_density$y[which.min(abs(norm_density$x - 0))]
prior_density_at_zero <- dcauchy(0, location = 0, scale = 0.707)

bayesian_plot_data <- data.frame(
  delta = c(delta_rm, delta_norm),
  condition = factor(rep(c("RM", "Non-RM"), c(length(delta_rm), length(delta_norm))),
                     levels = c("Non-RM", "RM"))
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
    data = tibble(x = 0, y = rm_density_at_zero, condition = factor("RM")),
    aes(x = x, y = y, color = condition),
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = norm_density_at_zero, condition = factor("Non-RM")),
    aes(x = x, y = y, color = condition),
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = prior_density_at_zero),
    aes(x = x, y = y),
    size = 3,
    shape = 21,
    fill = "white",
    color = "grey40",
    stroke = 1.5
  ) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = median_rm, color = rm_colors["RM"], linetype = "dotted", size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = median_norm, color = rm_colors["Non-RM"], linetype = "dotted", size = 1.2, alpha = 0.8) +
  annotate(
    "text",
    x = median_rm + 0.15, y = 1.8,
    label = paste0("BF01 = ", sprintf("%.3f", bf_01_rm)),
    color = rm_colors["RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    family = "Arial"
  ) +
  annotate(
    "text",
    x = median_norm - 0.05, y = 1.5,
    label = paste0("BF01 = ", sprintf("%.4f", bf_01_norm)),
    color = rm_colors["Non-RM"],
    fontface = "bold",
    size = 4,
    hjust = 1,
    family = "Arial"
  ) +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Bayesian One-Sample T-Tests on Relative Width Deviation",
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
# COMBINE PANELS
# =============================================================================

final_plot <- panel_a / panel_b +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(1, 1))

if (!dir.exists('outputs/exp2/figures')) {
  dir.create('outputs/exp2/figures', recursive = TRUE)
}

ggsave(
  filename = "outputs/exp2/figures/two_panel_width_deviation_analysis.png",
  plot = final_plot,
  width = 9,
  height = 7,
  dpi = 300,
  units = "in",
  bg = "white"
)

cat("Two-panel figure saved to outputs/exp2/figures/\n")
cat("Panel A: Absolute width deviation (by true width)\n")
cat("Panel B: Bayesian analysis on relative width deviation (pooled)\n")
