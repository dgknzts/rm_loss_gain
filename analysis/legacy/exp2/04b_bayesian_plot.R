# Bayesian Analysis Plot - Exp2
# Standalone publication-ready figure (density only, computed fresh)

library(tidyverse)
library(ggplot2)
library(BayesFactor)

source("analysis/functions/themes.R")

# Load data
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
# COMPUTE BAYESIAN ANALYSIS (RELATIVE WIDTH DEVIATION)
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

# =============================================================================
# CREATE PLOT
# =============================================================================

bayesian_plot <- ggplot(bayesian_plot_data, aes(x = delta)) +
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
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = median_rm, color = rm_colors["RM"], linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  geom_vline(xintercept = median_norm, color = rm_colors["Non-RM"], linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
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
    x = "Effect size (δ)",
    y = "Density"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(bayesian_plot)

# Save outputs
if (!dir.exists("outputs/exp2/figures")) {
  dir.create("outputs/exp2/figures", recursive = TRUE)
}

ggsave("outputs/exp2/figures/bayesian_analysis.png",
       bayesian_plot, width = 9, height = 3, dpi = 300, bg = "white")

cat("Saved: outputs/exp2/figures/bayesian_analysis.png\n")
cat(sprintf("RM: BF01 = %.3f, median delta = %.3f\n", bf_01_rm, median_rm))
cat(sprintf("Non-RM: BF01 = %.4f, median delta = %.3f\n", bf_01_norm, median_norm))
