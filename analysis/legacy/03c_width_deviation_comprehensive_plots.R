# Comprehensive Width Deviation Visualization
# Three-panel publication-ready figure

library(tidyverse)
library(ggplot2)
library(patchwork)

# Source themes
source("analysis/functions/themes.R")

# Load data and results
df <- read.csv("data/processed.csv")
contrast_results <- read.csv("outputs/tables/width_deviation_rm_contrasts.csv")
load("outputs/tables/bayesian_analysis_results.RData")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Prepare data for Panel A
spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(width_deviation)

# Calculate participant means and grand means with CIs
participant_means <- analysis_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_deviation, na.rm = TRUE), .groups = "drop")

plot_data_panel_a <- participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Add significance stars
contrast_results <- contrast_results %>%
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

star_data <- contrast_results %>%
  filter(significance != "") %>%
  left_join(
    plot_data_panel_a %>% 
      group_by(correct_width, correct_num) %>% 
      summarise(star_y = max(ci_upper) + 0.01, .groups = "drop"),
    by = c("correct_width", "correct_num")
  )

# Panel A: Emmeans dot plot
panel_a <- plot_data_panel_a %>%
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
    data = star_data,
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
    legend.position = c(0.96, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(0.8, "lines"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Panel B: Bayesian density plot
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
    x = bayesian_results$median_rm + 0.1, y = 1.8,
    label = paste0("RM: BF₀₁ = ", sprintf("%.3f", bayesian_results$bf_01_rm)),
    color = rm_colors["RM"],
    fontface = "bold",
    size = 4,
    hjust = 0,
    family = "Arial"
  ) +
  annotate(
    "text", 
    x = bayesian_results$median_norm - 0.1, y = 1.5,
    label = "Non-RM: BF₀₁ = 3.5 × 10⁻⁴",
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

# Panel C: Robustness plots
rob <- bayesian_results$robustness

panel_c1 <- ggplot(rob$rm_data, aes(x = prior_scale, y = bf01)) +
  geom_hline(yintercept = rob$rm_critical_bf, color = "grey50", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = rob$rm_critical_scale, color = "grey50", linetype = "dashed", size = 0.6) +
  geom_line(color = rm_colors["RM"], size = 1.3) +
  geom_point(data = rob$rm_points, color = rm_colors["RM"], size = 2.5, 
             fill = "white", shape = 21, stroke = 1.2) +
  scale_x_continuous(breaks = c(0, 0.707, rob$rm_critical_scale), 
                     labels = c("0.0", "0.707", sprintf("%.2f", rob$rm_critical_scale))) +
  scale_y_continuous(breaks = c(0, 1, 3, 5)) +
  labs(title = "RM Robustness", x = "Prior scale (r)", y = "BF₀₁") +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9)
  )

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
    axis.text = element_text(size = 9)
  )

# Reduce margins for each panel
panel_a <- panel_a + 
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))

panel_b <- panel_b + 
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))

panel_c1 <- panel_c1 + 
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

panel_c2 <- panel_c2 + 
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine panels
final_plot <- panel_a / panel_b / (panel_c1 | panel_c2)
final_plot <- final_plot + 
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", family = "Arial")
    )
  ) +
  plot_layout(heights = c(1, 1, 1))

# Save figure
if (!dir.exists('outputs/figures')) {
  dir.create('outputs/figures', recursive = TRUE)
}

ggsave(
  filename = "outputs/figures/comprehensive_width_deviation_analysis.png",
  plot = final_plot,
  width = 8,
  height = 10,
  dpi = 300,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "outputs/figures/comprehensive_width_deviation_analysis.pdf",
  plot = final_plot,
  width = 8,
  height = 10,
  units = "in",
  bg = "white"
)

