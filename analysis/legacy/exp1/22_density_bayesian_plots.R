# Density Bayesian Analysis Plots
# Publication-ready figures for density preservation hypothesis

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

# Load Bayesian results
load("outputs/exp1/tables/density_bayesian_results.RData")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C", "Comparison" = "#4169E1")

# =============================================================================
# FIGURE 1: OVERALL DENSITY BAYESIAN ANALYSIS (3-panel)
# =============================================================================

# Prepare plot data
plot_data <- tibble(
  delta = c(density_bayesian_results$delta_rm,
            density_bayesian_results$delta_norm,
            density_bayesian_results$delta_comp),
  test = factor(rep(c("RM = 0", "Non-RM = 0", "RM vs Non-RM"),
                    c(length(density_bayesian_results$delta_rm),
                      length(density_bayesian_results$delta_norm),
                      length(density_bayesian_results$delta_comp))),
                levels = c("RM = 0", "Non-RM = 0", "RM vs Non-RM"))
)

overall <- density_bayesian_results$overall_results

# Panel A: Prior vs Posterior Densities (using filled areas with transparency)
panel_density <- ggplot(plot_data, aes(x = delta, fill = test, color = test)) +
  # Prior distribution (gray dashed)
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area",
    fill = "grey70",
    color = "grey30",
    alpha = 0.4,
    linetype = "dashed",
    linewidth = 1.2,
    n = 1000
  ) +
  # Posterior distributions with fill and thick borders
  geom_density(
    aes(fill = test, color = test),
    alpha = 0.25,
    linewidth = 1.8,
    adjust = 1.2
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.8) +
  # Points at zero showing posterior density
  geom_point(
    data = tibble(
      x = 0,
      y = c(density_bayesian_results$rm_density_at_zero,
            density_bayesian_results$norm_density_at_zero,
            density_bayesian_results$comp_density_at_zero),
      test = factor(c("RM = 0", "Non-RM = 0", "RM vs Non-RM"),
                    levels = c("RM = 0", "Non-RM = 0", "RM vs Non-RM"))
    ),
    aes(x = x, y = y, color = test),
    size = 4,
    shape = 21,
    fill = "white",
    stroke = 2
  ) +
  # Prior density at zero
  geom_point(
    data = tibble(x = 0, y = density_bayesian_results$prior_density_at_zero),
    aes(x = x, y = y),
    size = 4,
    shape = 21,
    fill = "white",
    color = "grey30",
    stroke = 2
  ) +
  # BF annotations with colored backgrounds for visibility
  annotate(
    "label",
    x = 0.8, y = 2.4,
    label = sprintf("RM = 0: BF01 = %.2f", overall$bf01[1]),
    fill = "white",
    color = rm_colors["RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    label.size = 0.5
  ) +
  annotate(
    "label",
    x = 0.8, y = 2.0,
    label = sprintf("Non-RM = 0: BF01 = %.2f", overall$bf01[2]),
    fill = "white",
    color = rm_colors["Non-RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    label.size = 0.5
  ) +
  annotate(
    "label",
    x = 0.8, y = 1.6,
    label = sprintf("RM vs Non-RM: BF01 = %.2f", overall$bf01[3]),
    fill = "white",
    color = rm_colors["Comparison"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    label.size = 0.5
  ) +
  scale_color_manual(
    values = c("RM = 0" = rm_colors["RM"],
               "Non-RM = 0" = rm_colors["Non-RM"],
               "RM vs Non-RM" = rm_colors["Comparison"]),
    name = "Test"
  ) +
  scale_fill_manual(
    values = c("RM = 0" = rm_colors["RM"],
               "Non-RM = 0" = rm_colors["Non-RM"],
               "RM vs Non-RM" = rm_colors["Comparison"]),
    name = "Test"
  ) +
  labs(
    title = "Bayesian Tests on Density Deviation",
    subtitle = "Gray dashed: Cauchy prior (r = 0.707) | Colored: Posterior distributions",
    x = "Effect size (δ)",
    y = "Density"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.key.size = unit(1.2, "lines"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.5, linewidth = 2)),
    color = guide_legend(override.aes = list(linewidth = 2))
  )

# Panel B: RM = 0 Robustness
rob <- density_bayesian_results$robustness
rm_rob_data <- rob$data %>% filter(test == "RM = 0")

panel_rob_rm <- ggplot(rm_rob_data, aes(x = prior_scale, y = bf01)) +
  # Evidence threshold bands
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = Inf,
           fill = "#2E8B57", alpha = 0.15) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1/3,
           fill = "#DC143C", alpha = 0.15) +
  geom_hline(yintercept = 3, color = "#2E8B57", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 1/3, color = "#DC143C", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 1, color = "grey50", linetype = "dotted", linewidth = 0.8) +
  geom_vline(xintercept = 0.707, color = "grey40", linetype = "dashed", linewidth = 0.8) +
  geom_line(color = rm_colors["RM"], linewidth = 2) +
  geom_point(
    data = tibble(prior_scale = 0.707,
                  bf01 = rm_rob_data$bf01[which.min(abs(rm_rob_data$prior_scale - 0.707))]),
    color = rm_colors["RM"],
    size = 4,
    fill = "white",
    shape = 21,
    stroke = 2
  ) +
  scale_y_continuous(breaks = c(0.33, 1, 3, 5, 10), limits = c(0, NA)) +
  annotate("text", x = 1.3, y = 4, label = "Evidence for H0", color = "#2E8B57",
           fontface = "italic", size = 3.5) +
  labs(
    title = "RM = 0 Robustness",
    x = "Prior scale (r)",
    y = expression(BF[0][1])
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Panel C: RM vs NoRM Robustness
comp_rob_data <- rob$data %>% filter(test == "RM vs Non-RM")

panel_rob_comp <- ggplot(comp_rob_data, aes(x = prior_scale, y = bf01)) +
  # Evidence threshold bands
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = Inf,
           fill = "#2E8B57", alpha = 0.15) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1/3,
           fill = "#DC143C", alpha = 0.15) +
  geom_hline(yintercept = 3, color = "#2E8B57", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 1/3, color = "#DC143C", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 1, color = "grey50", linetype = "dotted", linewidth = 0.8) +
  geom_vline(xintercept = 0.707, color = "grey40", linetype = "dashed", linewidth = 0.8) +
  geom_line(color = rm_colors["Comparison"], linewidth = 2) +
  geom_point(
    data = tibble(prior_scale = 0.707,
                  bf01 = comp_rob_data$bf01[which.min(abs(comp_rob_data$prior_scale - 0.707))]),
    color = rm_colors["Comparison"],
    size = 4,
    fill = "white",
    shape = 21,
    stroke = 2
  ) +
  scale_y_continuous(breaks = c(0.33, 1, 3, 5, 10), limits = c(0, NA)) +
  annotate("text", x = 1.3, y = 4, label = "Evidence for H0", color = "#2E8B57",
           fontface = "italic", size = 3.5) +
  labs(
    title = "RM vs Non-RM Robustness",
    x = "Prior scale (r)",
    y = expression(BF[0][1])
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Combine Figure 1
figure1 <- panel_density / (panel_rob_rm | panel_rob_comp) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(1.2, 0.8))

# =============================================================================
# FIGURE 2: BY SET SIZE (Heatmap + Forest Plot)
# =============================================================================

setsize_data <- density_bayesian_results$setsize_results

# Panel A: BF01 Heatmap by Set Size
heatmap_data <- setsize_data %>%
  mutate(
    setsize = factor(setsize),
    test = factor(test, levels = c("RM = 0", "Non-RM = 0", "RM vs Non-RM")),
    bf01_label = sprintf("%.2f", bf01),
    evidence = case_when(
      bf01 > 10 ~ "Strong null",
      bf01 > 3 ~ "Moderate null",
      bf01 > 1/3 ~ "Inconclusive",
      bf01 > 1/10 ~ "Moderate alt",
      TRUE ~ "Strong alt"
    )
  )

panel_heatmap <- ggplot(heatmap_data, aes(x = setsize, y = test, fill = log10(bf01))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = bf01_label), size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#DC143C",
    mid = "white",
    high = "#2E8B57",
    midpoint = 0,
    name = expression(log[10](BF[0][1])),
    limits = c(-1, 1.5)
  ) +
  labs(
    title = "Bayes Factors by Set Size",
    subtitle = "BF01 > 3: Evidence for null, BF01 < 0.33: Evidence for alternative",
    x = "Set Size",
    y = ""
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

# Panel B: Forest Plot of Effect Sizes
forest_data <- setsize_data %>%
  mutate(
    setsize = factor(setsize),
    test_color = case_when(
      test == "RM = 0" ~ rm_colors["RM"],
      test == "Non-RM = 0" ~ rm_colors["Non-RM"],
      TRUE ~ rm_colors["Comparison"]
    )
  )

panel_forest <- ggplot(forest_data, aes(x = median_delta, y = interaction(test, setsize))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = test),
                 height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = test), size = 3) +
  scale_color_manual(
    values = c("RM = 0" = rm_colors["RM"],
               "Non-RM = 0" = rm_colors["Non-RM"],
               "RM vs Non-RM" = rm_colors["Comparison"]),
    name = "Test"
  ) +
  scale_y_discrete(labels = function(x) {
    parts <- str_split(x, "\\.", simplify = TRUE)
    paste0("Set ", parts[,2], ": ", parts[,1])
  }) +
  labs(
    title = "Effect Sizes (δ) by Set Size",
    subtitle = "Points: Median, Bars: 95% Credible Intervals",
    x = "Effect size (δ)",
    y = ""
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Combine Figure 2
figure2 <- panel_heatmap / panel_forest +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(0.8, 1.2))

# =============================================================================
# FIGURE 3: BY WIDTH x SET SIZE (3x3 Heatmaps)
# =============================================================================

ws_data <- density_bayesian_results$width_setsize_results %>%
  mutate(
    width = factor(width, labels = c("0.25°", "0.40°", "0.55°")),
    setsize = factor(setsize),
    bf01_label = sprintf("%.2f", bf01)
  )

# Function to create heatmap for each test type
create_heatmap <- function(data, test_name, fill_color) {
  d <- data %>% filter(test == test_name)

  ggplot(d, aes(x = setsize, y = width, fill = log10(bf01))) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = bf01_label), size = 3.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "#DC143C",
      mid = "white",
      high = "#2E8B57",
      midpoint = 0,
      limits = c(-1, 1.5),
      guide = "none"
    ) +
    labs(title = test_name, x = "Set Size", y = "Bar Width") +
    theme_scientific(base_size = 11, base_family = "Arial") +
    theme(
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}

panel_rm_heatmap <- create_heatmap(ws_data, "RM = 0", rm_colors["RM"])
panel_norm_heatmap <- create_heatmap(ws_data, "Non-RM = 0", rm_colors["Non-RM"])
panel_comp_heatmap <- create_heatmap(ws_data, "RM vs Non-RM", rm_colors["Comparison"])

# Combine Figure 3
figure3 <- (panel_rm_heatmap | panel_norm_heatmap | panel_comp_heatmap) +
  plot_annotation(
    title = "Bayes Factors by Width and Set Size",
    subtitle = "BF01 > 3 (green): Evidence for null hypothesis | BF01 < 0.33 (red): Evidence for alternative",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Arial"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", family = "Arial"),
      plot.tag = element_text(size = 12, face = "bold", family = "Arial")
    )
  )

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists("outputs/exp1/figures")) {
  dir.create("outputs/exp1/figures", recursive = TRUE)
}

ggsave("outputs/exp1/figures/density_bayesian_overall.png",
       figure1, width = 10, height = 9, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_bayesian_by_setsize.png",
       figure2, width = 9, height = 10, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_bayesian_by_width_setsize.png",
       figure3, width = 12, height = 5, dpi = 300, bg = "white")

cat("=== PLOTS SAVED ===\n")
cat("outputs/exp1/figures/density_bayesian_overall.png\n")
cat("outputs/exp1/figures/density_bayesian_by_setsize.png\n")
cat("outputs/exp1/figures/density_bayesian_by_width_setsize.png\n")
