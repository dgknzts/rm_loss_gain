# Bayesian Posterior Density Plot - Experiment 1
# Relative Width Deviation: Effect size distributions for RM and Non-RM conditions

library(tidyverse)

source("analysis/functions/themes.R")

# Load Bayesian results
load("outputs/exp1/tables/bayesian_analysis_results.RData")

# RM colors
rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# Format small numbers as scientific notation for plot labels
format_scientific <- function(x) {
  if (abs(x) >= 0.01) {
    return(sprintf("%.2f", x))
  }
  exponent <- floor(log10(abs(x)))
  coef <- x / 10^exponent
  sprintf("%.1f ~ '×' ~ 10^{%d}", coef, exponent)
}

# Prepare plot data
plot_data <- data.frame(
  delta = c(results$rm$delta, results$norm$delta),
  condition = factor(rep(c("RM", "Non-RM"), c(length(results$rm$delta), length(results$norm$delta))),
                     levels = c("Non-RM", "RM"))
)

# Create plot
p <- ggplot(plot_data, aes(x = delta)) +
  # Prior distribution (grey filled area + dashed line)
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area", fill = "grey40", alpha = 0.2, n = 500
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey40", linetype = "dashed", linewidth = 0.8, n = 500
  ) +

  # Posterior densities (lines only, no fill)
  geom_density(aes(color = condition), linewidth = 1.2, adjust = 1.2) +

  # Reference line at 0
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.5) +

  # Median lines (dotted)
  geom_vline(xintercept = results$rm$median_delta, color = rm_colors["RM"],
             linetype = "dotted", linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = results$norm$median_delta, color = rm_colors["Non-RM"],
             linetype = "dotted", linewidth = 1, alpha = 0.8) +

  # Dots ON the curves at x=0 (showing density at zero)
  geom_point(
    data = tibble(x = 0, y = results$rm$density_at_zero),
    aes(x = x, y = y), color = rm_colors["RM"],
    size = 2, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = results$norm$density_at_zero),
    aes(x = x, y = y), color = rm_colors["Non-RM"],
    size = 2, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = results$prior_density_at_zero),
    aes(x = x, y = y), color = "grey40",
    size = 2, shape = 21, fill = "white", stroke = 1.5
  ) +

  # BF annotations near medians
  annotate("text", x = results$rm$median_delta + 0.2, y = 1.6,
           label = sprintf("BF[0][1] == %s", format_scientific(results$rm$bf01)),
           parse = TRUE, color = rm_colors["RM"], size = 4.5, hjust = 0) +
  annotate("text", x = results$norm$median_delta - 0.1, y = 1.3,
           label = sprintf("BF[0][1] == %s", format_scientific(results$norm$bf01)),
           parse = TRUE, color = rm_colors["Non-RM"], size = 4.5, hjust = 1) +

  # Scales
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_continuous(limits = c(-2.5, 1.5)) +

  # Labels
  labs(
    x = expression(paste("Effect Size (", delta, "): Relative Width Deviation")),
    y = NULL
  ) +

  theme_scientific() +
  theme(
    legend.position = c(0.9, 0.9),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

print(p)

# Save plot
ggsave("outputs/exp1/figures/bayesian_width_deviation.png", p,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/bayesian_width_deviation.svg", p,
       width = 6, height = 4, bg = "white")
