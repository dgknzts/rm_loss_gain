# Alternative Visualizations for Width Density Deviation
# Multiple approaches to show "no difference" between RM and Non-RM
# Includes: Forest plots, Difference plots, Equivalence plots, Bland-Altman style

library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(patchwork)

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

# =============================================================================
# FIT MODEL AND COMPUTE ALL CONTRASTS
# =============================================================================

model <- lmer(width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

# Contrasts by width × setsize
emm_width_setsize <- emmeans(model, ~ rm_type | correct_width + correct_num)
contrasts_width_setsize <- as_tibble(summary(pairs(emm_width_setsize), infer = TRUE)) %>%
  mutate(condition = paste0("W", correct_width, "_N", correct_num))

# Contrasts by spacing × setsize
emm_spacing_setsize <- emmeans(model, ~ rm_type | spacing_category + correct_num)
contrasts_spacing_setsize <- as_tibble(summary(pairs(emm_spacing_setsize), infer = TRUE)) %>%
  mutate(condition = paste0(spacing_category, "_N", correct_num))

# Contrasts by width × spacing × setsize (all conditions)
emm_all <- emmeans(model, ~ rm_type | correct_width + spacing_category + correct_num)
contrasts_all <- as_tibble(summary(pairs(emm_all), infer = TRUE)) %>%
  mutate(condition = paste0("W", correct_width, "_", spacing_category, "_N", correct_num))

# Marginal contrasts (collapsed)
emm_by_width <- emmeans(model, ~ rm_type | correct_width)
contrasts_by_width <- as_tibble(summary(pairs(emm_by_width), infer = TRUE))

emm_by_setsize <- emmeans(model, ~ rm_type | correct_num)
contrasts_by_setsize <- as_tibble(summary(pairs(emm_by_setsize), infer = TRUE))

emm_by_spacing <- emmeans(model, ~ rm_type | spacing_category)
contrasts_by_spacing <- as_tibble(summary(pairs(emm_by_spacing), infer = TRUE))

emm_overall <- emmeans(model, ~ rm_type)
contrast_overall <- as_tibble(summary(pairs(emm_overall), infer = TRUE))

# Color scheme
width_colors <- c("0.25" = "#1B4F72", "0.4" = "#7D3C98", "0.55" = "#B7950B")
spacing_colors <- c("Smaller" = "#2E8B57", "Middle" = "#FFD700", "Larger" = "#DC143C")
setsize_colors <- c("3" = "#3498DB", "4" = "#E74C3C", "5" = "#27AE60")

# Create output directory
if (!dir.exists('outputs/exp1/figures/density_alternatives')) {
  dir.create('outputs/exp1/figures/density_alternatives', recursive = TRUE)
}

# =============================================================================
# 1. FOREST PLOTS
# =============================================================================

# Forest plot: All conditions (width × spacing × setsize)
forest_all <- contrasts_all %>%
  mutate(condition = fct_reorder(condition, estimate)) %>%
  ggplot(aes(x = estimate, y = condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.3, linewidth = 0.6) +
  geom_point(size = 2.5) +
  labs(
    title = "RM vs Non-RM Contrast: All Conditions",
    subtitle = "Width Density Deviation (Width × Spacing × Set Size)",
    x = "Difference (Non-RM - RM)",
    y = NULL
  ) +
  theme_scientific(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("outputs/exp1/figures/density_alternatives/forest_all_conditions.png",
       forest_all, width = 8, height = 10, dpi = 300, bg = "white")

# Forest plot: By width (collapsed over spacing and setsize)
forest_width <- contrasts_by_width %>%
  mutate(correct_width = factor(correct_width)) %>%
  ggplot(aes(x = estimate, y = correct_width, color = correct_width)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = width_colors, guide = "none") +
  labs(
    title = "RM vs Non-RM Contrast by Width",
    subtitle = "Collapsed across spacing and set size",
    x = "Difference (Non-RM - RM)",
    y = "True Width (°)"
  ) +
  theme_scientific(base_size = 12) +
  theme(plot.subtitle = element_text(size = 10, color = "grey40"))

ggsave("outputs/exp1/figures/density_alternatives/forest_by_width.png",
       forest_width, width = 7, height = 4, dpi = 300, bg = "white")

# Forest plot: By set size
forest_setsize <- contrasts_by_setsize %>%
  mutate(correct_num = factor(correct_num)) %>%
  ggplot(aes(x = estimate, y = correct_num, color = correct_num)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = setsize_colors, guide = "none") +
  labs(
    title = "RM vs Non-RM Contrast by Set Size",
    subtitle = "Collapsed across spacing and width",
    x = "Difference (Non-RM - RM)",
    y = "Set Size"
  ) +
  theme_scientific(base_size = 12) +
  theme(plot.subtitle = element_text(size = 10, color = "grey40"))

ggsave("outputs/exp1/figures/density_alternatives/forest_by_setsize.png",
       forest_setsize, width = 7, height = 4, dpi = 300, bg = "white")

# Forest plot: By spacing
forest_spacing <- contrasts_by_spacing %>%
  ggplot(aes(x = estimate, y = spacing_category, color = spacing_category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = spacing_colors, guide = "none") +
  labs(
    title = "RM vs Non-RM Contrast by Spacing",
    subtitle = "Collapsed across width and set size",
    x = "Difference (Non-RM - RM)",
    y = "Spacing Category"
  ) +
  theme_scientific(base_size = 12) +
  theme(plot.subtitle = element_text(size = 10, color = "grey40"))

ggsave("outputs/exp1/figures/density_alternatives/forest_by_spacing.png",
       forest_spacing, width = 7, height = 4, dpi = 300, bg = "white")

# =============================================================================
# 2. DIFFERENCE PLOTS
# =============================================================================

# Difference plot: By width × setsize
diff_width_setsize <- contrasts_width_setsize %>%
  ggplot(aes(x = correct_num, y = estimate, color = correct_width)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, linewidth = 0.8, position = position_dodge(0.4)) +
  geom_point(size = 3.5, position = position_dodge(0.4)) +
  scale_color_manual(values = width_colors, name = "Width (°)") +
  labs(
    title = "RM vs Non-RM Difference by Width and Set Size",
    subtitle = "Zero line indicates no difference",
    x = "Set Size",
    y = "Difference (Non-RM - RM)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("outputs/exp1/figures/density_alternatives/difference_by_width_setsize.png",
       diff_width_setsize, width = 7, height = 5, dpi = 300, bg = "white")

# Difference plot: By spacing × setsize
diff_spacing_setsize <- contrasts_spacing_setsize %>%
  ggplot(aes(x = correct_num, y = estimate, color = spacing_category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, linewidth = 0.8, position = position_dodge(0.4)) +
  geom_point(size = 3.5, position = position_dodge(0.4)) +
  scale_color_manual(values = spacing_colors, name = "Spacing") +
  labs(
    title = "RM vs Non-RM Difference by Spacing and Set Size",
    subtitle = "Zero line indicates no difference",
    x = "Set Size",
    y = "Difference (Non-RM - RM)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("outputs/exp1/figures/density_alternatives/difference_by_spacing_setsize.png",
       diff_spacing_setsize, width = 7, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 3. EQUIVALENCE PLOT
# =============================================================================

# Determine equivalence margin (using smallest detectable effect or practical threshold)
equiv_margin <- 0.03  # Can adjust based on what's practically meaningful

equiv_plot <- contrasts_width_setsize %>%
  mutate(condition = fct_reorder(condition, estimate)) %>%
  ggplot(aes(x = estimate, y = condition)) +
  annotate("rect", xmin = -equiv_margin, xmax = equiv_margin,
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_vline(xintercept = c(-equiv_margin, equiv_margin),
             linetype = "dotted", color = "darkgreen", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL), height = 0.3, linewidth = 0.6) +
  geom_point(size = 2.5) +
  labs(
    title = "Equivalence Analysis: RM vs Non-RM",
    subtitle = sprintf("Green zone = equivalence bounds (±%.2f)", equiv_margin),
    x = "Difference (Non-RM - RM)",
    y = "Condition (Width × Set Size)"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("outputs/exp1/figures/density_alternatives/equivalence_all.png",
       equiv_plot, width = 8, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 4. BLAND-ALTMAN STYLE PLOT
# =============================================================================

# Get emmeans for both RM and Non-RM
emm_detailed <- as_tibble(summary(emm_width_setsize))

ba_data <- emm_detailed %>%
  select(rm_type, correct_width, correct_num, emmean) %>%
  pivot_wider(names_from = rm_type, values_from = emmean) %>%
  mutate(
    mean_value = (`Non-RM` + RM) / 2,
    difference = `Non-RM` - RM
  )

ba_plot <- ba_data %>%
  ggplot(aes(x = mean_value, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = correct_width, shape = correct_num), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8, alpha = 0.2) +
  scale_color_manual(values = width_colors, name = "Width (°)") +
  scale_shape_manual(values = c("3" = 16, "4" = 17, "5" = 15), name = "Set Size") +
  labs(
    title = "Bland-Altman Style: RM vs Non-RM Agreement",
    subtitle = "No systematic bias if points scatter around zero with no trend",
    x = "Mean Width Density Deviation",
    y = "Difference (Non-RM - RM)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("outputs/exp1/figures/density_alternatives/blandaltman_style.png",
       ba_plot, width = 7, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 5. COMBINED SUMMARY PLOT
# =============================================================================

# Create a combined 2x2 figure with the best representations
combined_plot <- (forest_width + forest_spacing) / (diff_width_setsize + equiv_plot) +
  plot_annotation(
    title = "Width Density Deviation: No RM vs Non-RM Difference",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.tag = element_text(size = 12, face = "bold")
    )
  )

ggsave("outputs/exp1/figures/density_alternatives/combined_summary.png",
       combined_plot, width = 14, height = 10, dpi = 300, bg = "white")

# =============================================================================
# SUMMARY OUTPUT
# =============================================================================

cat("\n=== Alternative Density Deviation Plots Generated ===\n")
cat("\nForest plots:\n")
cat("  - forest_all_conditions.png (27 conditions)\n")
cat("  - forest_by_width.png (collapsed)\n")
cat("  - forest_by_setsize.png (collapsed)\n")
cat("  - forest_by_spacing.png (collapsed)\n")
cat("\nDifference plots:\n")
cat("  - difference_by_width_setsize.png\n")
cat("  - difference_by_spacing_setsize.png\n")
cat("\nEquivalence plot:\n")
cat("  - equivalence_all.png\n")
cat("\nBland-Altman style:\n")
cat("  - blandaltman_style.png\n")
cat("\nCombined summary:\n")
cat("  - combined_summary.png\n")

cat("\n=== Overall RM vs Non-RM Contrast ===\n")
cat(sprintf("Estimate: %.4f, 95%% CI [%.4f, %.4f], z = %.2f, p = %.3f\n",
            contrast_overall$estimate, contrast_overall$asymp.LCL, contrast_overall$asymp.UCL,
            contrast_overall$z.ratio, contrast_overall$p.value))
