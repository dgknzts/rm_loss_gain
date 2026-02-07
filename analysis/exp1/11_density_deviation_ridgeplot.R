# Density Deviation Ridge Plot - Experiment 1
# Posterior distributions for RM vs Non-RM difference by density category

library(tidyverse)
library(ggridges)

source("analysis/functions/themes.R")

# Load results
load("outputs/exp1/tables/density_deviation_results.RData")

# =============================================================================
# PREPARE DATA
# =============================================================================

# Combine posterior samples from all density categories
plot_data <- bind_rows(
  tibble(
    delta = density_results$bayesian_by_category$Low$delta,
    density_category = "Low",
    bf_01 = density_results$bayesian_by_category$Low$bf_01
  ),
  tibble(
    delta = density_results$bayesian_by_category$Medium$delta,
    density_category = "Medium",
    bf_01 = density_results$bayesian_by_category$Medium$bf_01
  ),
  tibble(
    delta = density_results$bayesian_by_category$High$delta,
    density_category = "High",
    bf_01 = density_results$bayesian_by_category$High$bf_01
  )
) %>%
  mutate(density_category = factor(density_category, levels = c("High", "Medium", "Low")))

# BF labels for each category
bf_labels <- plot_data %>%
  group_by(density_category) %>%
  summarise(bf_01 = first(bf_01), .groups = "drop")

# =============================================================================
# RIDGE PLOT
# =============================================================================

p <- ggplot(plot_data, aes(x = delta, y = density_category)) +
  # Reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +

  # Ridge densities
  geom_density_ridges(
    aes(fill = density_category),
    alpha = 0.6,
    scale = 1.5,
    color = "grey30",
    linewidth = 0.5
  ) +

  # BF annotations
  geom_text(data = bf_labels,
            aes(x = 1.2, y = density_category,
                label = sprintf("BF01 = %.1f", bf_01)),
            size = 4.5, hjust = 0, vjust = -0.5) +

  # Scales
  scale_fill_manual(
    values = c("Low" = "#6BAED6", "Medium" = "#4292C6", "High" = "#2171B5"),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(-1.5, 2)) +

  # Labels
  labs(
    x = expression(paste("Effect Size (", delta, "): RM - Non-RM Difference")),
    y = "Density Category"
  ) +

  theme_scientific() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

print(p)

ggsave("outputs/exp1/figures/density_deviation_ridgeplot.png", p,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_deviation_ridgeplot.svg", p,
       width = 6, height = 4, bg = "white")

