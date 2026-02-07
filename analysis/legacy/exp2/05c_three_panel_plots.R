# Three-Panel Figure for Exp2 Poster
# Panel A: Number Deviation (RM check)
# Panel B: Width Deviation (by condition)
# Panel C: Bayesian Analysis

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
# PANEL A: NUMBER DEVIATION (RM MANIPULATION CHECK)
# =============================================================================

number_data <- df %>%
  filter(!is.na(number_deviation)) %>%
  group_by(subID, correct_num) %>%
  summarise(mean_dev = mean(number_deviation), .groups = "drop")

number_grand_avg <- number_data %>%
  group_by(correct_num) %>%
  summarise(
    grand_mean = mean(mean_dev),
    sd_dev = sd(mean_dev),
    n = n(),
    se = sd_dev / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

panel_a <- ggplot(number_grand_avg, aes(x = factor(correct_num), y = grand_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = -1, linetype = "dotted", color = "#DC143C", alpha = 0.6, linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, linewidth = 1, color = "black") +
  geom_point(size = 4, shape = 16, color = "black") +
  scale_y_continuous(limits = c(-0.8, 0.2)) +
  scale_x_discrete(name = "Set Size") +
  labs(
    title = "Number Deviation",
    y = "Number Deviation"
  ) +
  theme_scientific(base_size = 13, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    plot.margin = margin(t = 5, r = 10, b = 2, l = 10, unit = "pt")
  )

# =============================================================================
# PANEL B: WIDTH DEVIATION BY CONDITION
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

panel_b <- plot_data_width %>%
  ggplot(aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Deviation (°)") +
  labs(title = "Width Deviation by Condition") +
  theme_scientific(base_size = 13, base_family = "Arial") +
  theme(
    legend.position = c(0.92, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    plot.margin = margin(t = 2, r = 10, b = 2, l = 10, unit = "pt")
  )

# =============================================================================
# PANEL C: BAYESIAN ANALYSIS
# =============================================================================

subject_means <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(
    mean_width_deviation_relative = mean(width_deviation_relative, na.rm = TRUE),
    .groups = "drop"
  )

rm_means <- subject_means %>% filter(rm_type == "RM") %>% pull(mean_width_deviation_relative)
norm_means <- subject_means %>% filter(rm_type == "Non-RM") %>% pull(mean_width_deviation_relative)

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

panel_c <- ggplot(bayesian_plot_data, aes(x = delta)) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area", fill = "grey40", alpha = 0.3, n = 1000
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey30", linetype = "dashed", linewidth = 1, n = 1000
  ) +
  geom_density(aes(color = condition), linewidth = 1.2, adjust = 1.2) +
  geom_point(
    data = tibble(x = 0, y = rm_density_at_zero, condition = factor("RM")),
    aes(x = x, y = y, color = condition), size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = norm_density_at_zero, condition = factor("Non-RM")),
    aes(x = x, y = y, color = condition), size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = prior_density_at_zero),
    aes(x = x, y = y), size = 3, shape = 21, fill = "white", color = "grey40", stroke = 1.5
  ) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = median_rm, color = rm_colors["RM"], linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  geom_vline(xintercept = median_norm, color = rm_colors["Non-RM"], linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Bayesian One-Sample T-Tests",
    x = "Effect Size (delta)",
    y = "Density"
  ) +
  theme_scientific(base_size = 13, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    plot.margin = margin(t = 2, r = 10, b = 5, l = 10, unit = "pt")
  )

# =============================================================================
# COMBINE PANELS (VERTICAL STACK)
# =============================================================================

final_plot <- (panel_a / panel_b / panel_c) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 16, face = "bold", family = "Arial"))
  ) &
  theme(plot.margin = margin(t = 3, r = 10, b = 3, l = 10, unit = "pt"))

final_plot <- final_plot + plot_layout(heights = c(0.8, 1.2, 1))

if (!dir.exists('outputs/exp2/figures')) {
  dir.create('outputs/exp2/figures', recursive = TRUE)
}

ggsave(
  filename = "outputs/exp2/figures/three_panel_poster_figure.png",
  plot = final_plot,
  width = 6,
  height = 7,
  dpi = 300,
  units = "in",
  bg = "white"
)

cat("Three-panel figure saved to outputs/exp2/figures/three_panel_poster_figure.png\n")
cat(sprintf("Panel A: Number Deviation (RM verification)\n"))
cat(sprintf("Panel B: Width Deviation by condition\n"))
cat(sprintf("Panel C: Bayesian analysis (BF01_RM = %.3f, BF01_NonRM = %.4f)\n", bf_01_rm, bf_01_norm))
