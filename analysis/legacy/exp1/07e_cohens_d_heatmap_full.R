# Cohen's d Heat Map with All Conditions (Width × Set Size × Spacing)
# Visualizing effect sizes for RM vs Non-RM comparison

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
# FIT MODEL AND COMPUTE CONTRASTS
# =============================================================================

model <- lmer(width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

residual_sd <- sigma(model)

# Get contrasts for all combinations
emm_all <- emmeans(model, ~ rm_type | correct_width + correct_num + spacing_category)
contrasts_all <- as_tibble(summary(pairs(emm_all), infer = TRUE)) %>%
  mutate(cohen_d = estimate / residual_sd)

# =============================================================================
# HEAT MAP: FACETED BY SPACING
# =============================================================================

heatmap_faceted <- contrasts_all %>%
  mutate(
    correct_width = factor(correct_width, labels = c("0.25°", "0.4°", "0.55°")),
    correct_num = factor(correct_num, labels = c("N=3", "N=4", "N=5"))
  ) %>%
  ggplot(aes(x = correct_num, y = correct_width, fill = cohen_d)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.2f", cohen_d)),
            color = "black", fontface = "bold", size = 4) +
  facet_wrap(~spacing_category, ncol = 3) +
  scale_fill_gradient2(
    low = "#2E8B57", mid = "white", high = "#DC143C",
    midpoint = 0, limits = c(-0.95, 0.95),
    name = "Cohen's d"
  ) +
  labs(
    title = "Effect Size (Cohen's d): RM vs Non-RM",
    subtitle = "Width Density Deviation | Values near 0 (white) = no difference",
    x = "Set Size",
    y = "True Width"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    plot.subtitle = element_text(size = 10, color = "grey40")
  ) +
  coord_fixed()

print(heatmap_faceted)

# =============================================================================
# ALTERNATIVE: SINGLE GRID WITH SPACING ON Y-AXIS
# =============================================================================

heatmap_single <- contrasts_all %>%
  mutate(
    y_label = paste0(spacing_category, " | ", correct_width, "°"),
    y_label = factor(y_label, levels = unique(y_label[order(spacing_category, correct_width)]))
  ) %>%
  ggplot(aes(x = factor(correct_num), y = y_label, fill = cohen_d)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", cohen_d)),
            color = "black", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(
    low = "#2E8B57", mid = "white", high = "#DC143C",
    midpoint = 0, limits = c(-0.9, 0.9),
    name = "Cohen's d"
  ) +
  labs(
    title = "Effect Size (Cohen's d): RM vs Non-RM",
    subtitle = "All 27 conditions | Values near 0 (white) = no difference",
    x = "Set Size",
    y = "Spacing | Width"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures/density_alternatives')) {
  dir.create('outputs/exp1/figures/density_alternatives', recursive = TRUE)
}

ggsave("outputs/exp1/figures/density_alternatives/heatmap_cohens_d_by_spacing.png",
       heatmap_faceted, width = 10, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_alternatives/heatmap_cohens_d_full_grid.png",
       heatmap_single, width = 7, height = 8, dpi = 300, bg = "white")

cat("Saved: outputs/exp1/figures/density_alternatives/heatmap_cohens_d_by_spacing.png\n")
cat("Saved: outputs/exp1/figures/density_alternatives/heatmap_cohens_d_full_grid.png\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== Cohen's d Summary ===\n")
cat(sprintf("Range: %.3f to %.3f\n", min(contrasts_all$cohen_d), max(contrasts_all$cohen_d)))
cat(sprintf("Mean |d|: %.3f\n", mean(abs(contrasts_all$cohen_d))))
cat(sprintf("Conditions with |d| < 0.2 (small): %d/%d (%.1f%%)\n",
            sum(abs(contrasts_all$cohen_d) < 0.2), nrow(contrasts_all),
            100 * sum(abs(contrasts_all$cohen_d) < 0.2) / nrow(contrasts_all)))
