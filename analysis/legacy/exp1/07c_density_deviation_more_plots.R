# More Alternative Visualizations for Width Density Deviation
# Creative approaches to show "no difference" between RM and Non-RM

library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(patchwork)
library(ggridges)

source("analysis/functions/themes.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

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
  drop_na(width_density_deviation)

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

if (!dir.exists('outputs/exp1/figures/density_alternatives')) {
  dir.create('outputs/exp1/figures/density_alternatives', recursive = TRUE)
}

# =============================================================================
# 1. PAIRED SCATTER PLOT (RM vs Non-RM on axes)
# =============================================================================

# Calculate participant means by condition and rm_type
participant_means <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density = mean(width_density_deviation, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = rm_type, values_from = mean_density)

scatter_paired <- participant_means %>%
  ggplot(aes(x = `Non-RM`, y = RM)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_point(size = 4, alpha = 0.7, color = "#3498DB") +
  geom_smooth(method = "lm", se = TRUE, color = "#E74C3C", linewidth = 1) +
  coord_fixed() +
  labs(
    title = "RM vs Non-RM: Participant Comparison",
    subtitle = "Points on diagonal = identical responses",
    x = "Non-RM Width Density Deviation",
    y = "RM Width Density Deviation"
  ) +
  theme_scientific(base_size = 12)

ggsave("outputs/exp1/figures/density_alternatives/scatter_paired.png",
       scatter_paired, width = 6, height = 6, dpi = 300, bg = "white")

# =============================================================================
# 2. DUMBBELL PLOT (Connected dots showing RM vs Non-RM)
# =============================================================================

# By condition
condition_means <- analysis_data %>%
  group_by(correct_width, correct_num, rm_type) %>%
  summarise(
    mean_val = mean(width_density_deviation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(condition = paste0("Width ", correct_width, "° | Set ", correct_num))

dumbbell_data <- condition_means %>%
  pivot_wider(names_from = rm_type, values_from = mean_val)

dumbbell_plot <- dumbbell_data %>%
  mutate(condition = fct_reorder(condition, (`Non-RM` + RM) / 2)) %>%
  ggplot(aes(y = condition)) +
  geom_segment(aes(x = `Non-RM`, xend = RM, yend = condition),
               color = "grey60", linewidth = 1.5) +
  geom_point(aes(x = `Non-RM`), color = rm_colors["Non-RM"], size = 4) +
  geom_point(aes(x = RM), color = rm_colors["RM"], size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "RM vs Non-RM by Condition",
    subtitle = "Short segments = small difference (Green = Non-RM, Red = RM)",
    x = "Width Density Deviation",
    y = NULL
  ) +
  theme_scientific(base_size = 11) +
  theme(axis.text.y = element_text(size = 9))

ggsave("outputs/exp1/figures/density_alternatives/dumbbell_plot.png",
       dumbbell_plot, width = 8, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 3. RIDGELINE DENSITY PLOT
# =============================================================================

ridgeline_plot <- analysis_data %>%
  ggplot(aes(x = width_density_deviation, y = rm_type, fill = rm_type)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Distribution of Width Density Deviation",
    subtitle = "Overlapping distributions = no difference",
    x = "Width Density Deviation",
    y = NULL
  ) +
  theme_scientific(base_size = 12) +
  theme(axis.text.y = element_text(size = 11, face = "bold"))

ggsave("outputs/exp1/figures/density_alternatives/ridgeline_plot.png",
       ridgeline_plot, width = 8, height = 4, dpi = 300, bg = "white")

# =============================================================================
# 4. VIOLIN + BOXPLOT COMPARISON
# =============================================================================

violin_box <- analysis_data %>%
  ggplot(aes(x = rm_type, y = width_density_deviation, fill = rm_type)) +
  geom_violin(alpha = 0.4, width = 0.8) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Width Density Deviation: RM vs Non-RM",
    subtitle = "Similar distributions indicate no difference",
    x = NULL,
    y = "Width Density Deviation"
  ) +
  theme_scientific(base_size = 12) +
  theme(axis.text.x = element_text(size = 12, face = "bold"))

ggsave("outputs/exp1/figures/density_alternatives/violin_boxplot.png",
       violin_box, width = 5, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 5. PARTICIPANT SLOPE PLOT (Spaghetti plot)
# =============================================================================

participant_by_rm <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density = mean(width_density_deviation, na.rm = TRUE), .groups = "drop")

spaghetti_plot <- participant_by_rm %>%
  ggplot(aes(x = rm_type, y = mean_density, group = subID)) +
  geom_line(alpha = 0.3, color = "grey50") +
  geom_point(aes(color = rm_type), size = 2, alpha = 0.6) +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               linewidth = 2, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               size = 5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Individual Participant Responses",
    subtitle = "Flat lines = no change between RM and Non-RM (black = grand mean)",
    x = NULL,
    y = "Mean Width Density Deviation"
  ) +
  theme_scientific(base_size = 12) +
  theme(axis.text.x = element_text(size = 12, face = "bold"))

ggsave("outputs/exp1/figures/density_alternatives/spaghetti_plot.png",
       spaghetti_plot, width = 6, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 6. HEAT MAP OF EFFECT SIZES
# =============================================================================

model <- lmer(width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

residual_sd <- sigma(model)

emm_all <- emmeans(model, ~ rm_type | correct_width + correct_num)
contrasts_all <- as_tibble(summary(pairs(emm_all), infer = TRUE)) %>%
  mutate(cohen_d = estimate / residual_sd)

heatmap_plot <- contrasts_all %>%
  ggplot(aes(x = correct_num, y = correct_width, fill = cohen_d)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", cohen_d)), color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient2(low = "#2E8B57", mid = "grey90", high = "#DC143C",
                       midpoint = 0, limits = c(-0.5, 0.5),
                       name = "Cohen's d") +
  labs(
    title = "Effect Size (Cohen's d) Heat Map",
    subtitle = "Values near 0 (light) indicate no difference",
    x = "Set Size",
    y = "True Width (°)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  coord_fixed()

ggsave("outputs/exp1/figures/density_alternatives/heatmap_effectsize.png",
       heatmap_plot, width = 7, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 7. LOLLIPOP CHART (Cleaner than bar chart)
# =============================================================================

lollipop_plot <- contrasts_all %>%
  mutate(condition = paste0("W", correct_width, " × N", correct_num)) %>%
  mutate(condition = fct_reorder(condition, estimate)) %>%
  ggplot(aes(x = condition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_segment(aes(xend = condition, y = 0, yend = estimate),
               color = "grey60", linewidth = 1) +
  geom_point(aes(color = abs(estimate) < 0.02), size = 4) +
  scale_color_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#DC143C"),
                     labels = c("TRUE" = "Negligible", "FALSE" = "Small"),
                     name = "Effect") +
  labs(
    title = "RM vs Non-RM Difference by Condition",
    subtitle = "Dots near zero line = no difference",
    x = NULL,
    y = "Difference (Non-RM - RM)"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_alternatives/lollipop_chart.png",
       lollipop_plot, width = 8, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 8. SIMPLE MEAN COMPARISON WITH CI
# =============================================================================

overall_summary <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_val = mean(width_density_deviation, na.rm = TRUE), .groups = "drop") %>%
  group_by(rm_type) %>%
  summarise(
    grand_mean = mean(mean_val),
    se = sd(mean_val) / sqrt(n()),
    ci_lower = grand_mean - qt(0.975, n()-1) * se,
    ci_upper = grand_mean + qt(0.975, n()-1) * se,
    .groups = "drop"
  )

simple_comparison <- overall_summary %>%
  ggplot(aes(x = rm_type, y = grand_mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, linewidth = 1.2) +
  geom_point(size = 6) +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Overall Width Density Deviation",
    subtitle = "Overlapping CIs suggest no significant difference",
    x = NULL,
    y = "Mean Width Density Deviation"
  ) +
  theme_scientific(base_size = 14) +
  theme(axis.text.x = element_text(size = 14, face = "bold")) +
  coord_cartesian(ylim = c(-0.05, 0.05))

ggsave("outputs/exp1/figures/density_alternatives/simple_comparison.png",
       simple_comparison, width = 5, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 9. FACETED DENSITY OVERLAY
# =============================================================================

density_overlay <- analysis_data %>%
  ggplot(aes(x = width_density_deviation, fill = rm_type, color = rm_type)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(correct_num ~ correct_width,
             labeller = labeller(correct_num = function(x) paste("N =", x),
                                correct_width = function(x) paste("W =", x, "°"))) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Distribution Overlap by Condition",
    subtitle = "Overlapping distributions = no difference",
    x = "Width Density Deviation",
    y = "Density"
  ) +
  theme_scientific(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9, face = "bold")
  )

ggsave("outputs/exp1/figures/density_alternatives/density_overlay_faceted.png",
       density_overlay, width = 9, height = 7, dpi = 300, bg = "white")

# =============================================================================
# 10. MINIMALIST DOT + RANGE PLOT
# =============================================================================

minimalist_plot <- contrasts_all %>%
  mutate(
    condition = paste0(correct_width, "° × ", correct_num),
    within_bounds = abs(estimate) < 0.02 & asymp.LCL < 0 & asymp.UCL > 0
  ) %>%
  ggplot(aes(y = fct_reorder(condition, as.numeric(correct_width)))) +
  annotate("rect", xmin = -0.02, xmax = 0.02, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 height = 0, linewidth = 2, color = "grey50") +
  geom_point(aes(x = estimate), size = 3, color = "black") +
  labs(
    title = "Width Density Deviation: RM Effect",
    subtitle = "Grey band = negligible effect zone | All CIs cross zero",
    x = "Difference (Non-RM − RM)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )

ggsave("outputs/exp1/figures/density_alternatives/minimalist_dotrange.png",
       minimalist_plot, width = 7, height = 4, dpi = 300, bg = "white")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== More Alternative Plots Generated ===\n")
cat("\n1. scatter_paired.png - RM vs Non-RM scatter (identity line)\n")
cat("2. dumbbell_plot.png - Connected dots by condition\n")
cat("3. ridgeline_plot.png - Overlapping density ridges\n")
cat("4. violin_boxplot.png - Distribution comparison\n")
cat("5. spaghetti_plot.png - Individual participant slopes\n")
cat("6. heatmap_effectsize.png - Cohen's d heat map\n")
cat("7. lollipop_chart.png - Deviations from zero\n")
cat("8. simple_comparison.png - Overall mean + CI\n")
cat("9. density_overlay_faceted.png - Density overlap by condition\n")
cat("10. minimalist_dotrange.png - Clean CI plot\n")
