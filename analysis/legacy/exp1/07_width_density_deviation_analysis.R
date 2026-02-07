# Width Density Deviation Statistical Analysis and Visualization
# Mixed-effects model with emmeans contrasts + publication-ready figure

library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(patchwork)

# Source themes
source("analysis/functions/themes.R")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

format_num <- function(x, digits = 3) {
  formatC(x, format = 'f', digits = digits)
}

format_p <- function(p) {
  if (is.na(p)) 'p = NA'
  else if (p < 0.001) 'p < 0.001'
  else sprintf('p = %.3f', p)
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

# Load data
df <- read.csv('data/exp1/processed.csv')

# Create spacing categories
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

cat(sprintf("Analysis data: %d trials from %d participants\n",
            nrow(analysis_data), length(unique(analysis_data$subID))))

# =============================================================================
# STATISTICAL MODEL
# =============================================================================

# Fit 4-way mixed-effects model
model <- lmer(width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

# Calculate emmeans and contrasts by width and set size
emm_density_setsize <- emmeans(model, ~ rm_type | correct_width + correct_num)
density_dev_contrasts <- pairs(emm_density_setsize)

# =============================================================================
# EXPORT STATISTICAL RESULTS
# =============================================================================

if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}

write.csv(as.data.frame(summary(emm_density_setsize)),
          'outputs/exp1/tables/width_density_deviation_emmeans_by_width_setsize.csv',
          row.names = FALSE)

density_contrast_summary <- summary(density_dev_contrasts, infer = TRUE)
write.csv(as.data.frame(density_contrast_summary),
          'outputs/exp1/tables/width_density_deviation_rm_contrasts.csv',
          row.names = FALSE)

# =============================================================================
# EFFECT SIZE CALCULATION
# =============================================================================

residual_sd <- sigma(model)
overall_emm <- emmeans(model, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)

# Overall effect
o_means <- as_tibble(summary(overall_emm))
o_rm <- o_means %>% filter(rm_type == 'RM')
o_norm <- o_means %>% filter(rm_type == 'Non-RM')

cohen_d_overall <- overall_contrast$estimate / residual_sd

cat('\n=== OVERALL RM vs NON-RM COMPARISON ===\n')
cat(sprintf('RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s, d = %s\n',
            format_num(o_rm$emmean), format_num(o_rm$SE),
            format_num(o_norm$emmean), format_num(o_norm$SE),
            format_num(overall_contrast$z.ratio, 2), format_p(overall_contrast$p.value),
            format_num(cohen_d_overall, 3)))

# Individual contrasts with effect sizes
contrasts_with_d <- as_tibble(density_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

significant_effects <- sum(contrasts_with_d$p_fdr < 0.05)
total_effects <- nrow(contrasts_with_d)

cat(sprintf('\nSignificant effects: %d out of %d comparisons (%.1f%%)\n',
            significant_effects, total_effects,
            100 * significant_effects / total_effects))

strongest <- contrasts_with_d %>% slice_max(abs(estimate))
cat(sprintf('Strongest effect: Width %s, Set size %s (d = %s, %s)\n',
            strongest$correct_width, strongest$correct_num,
            format_num(strongest$cohen_d, 3), format_p(strongest$p.value)))

# Detailed pairwise comparison output
cat('\n=== DETAILED PAIRWISE COMPARISONS ===\n')

density_emmeans_detailed <- summary(emm_density_setsize)

for(i in 1:nrow(contrasts_with_d)) {
  row <- contrasts_with_d[i,]

  rm_mean <- density_emmeans_detailed %>%
    filter(correct_width == row$correct_width,
           correct_num == row$correct_num,
           rm_type == "RM")

  norm_mean <- density_emmeans_detailed %>%
    filter(correct_width == row$correct_width,
           correct_num == row$correct_num,
           rm_type == "Non-RM")

  p_main <- format_p(row$p.value)
  p_fdr <- if(row$p_fdr < 0.001) "FDR p < 0.001" else sprintf("FDR p = %.3f", row$p_fdr)

  cat(sprintf('Width %s, Set size %s: RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s (%s), d = %s, 95%% CI [%s, %s].\n',
              row$correct_width, row$correct_num,
              format_num(rm_mean$emmean), format_num(rm_mean$SE),
              format_num(norm_mean$emmean), format_num(norm_mean$SE),
              format_num(row$z.ratio, 2), p_main, p_fdr,
              format_num(row$cohen_d, 3),
              format_num(row$asymp.LCL, 3), format_num(row$asymp.UCL, 3)))
}

# =============================================================================
# VISUALIZATION
# =============================================================================

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Calculate participant means for width density deviation
participant_means <- analysis_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_density_deviation, na.rm = TRUE), .groups = "drop")

plot_data <- participant_means %>%
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
contrast_results <- density_contrast_summary %>%
  as_tibble() %>%
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
    plot_data %>%
      group_by(correct_width, correct_num) %>%
      summarise(star_y = max(ci_upper) + 0.005, .groups = "drop"),
    by = c("correct_width", "correct_num")
  )

# Create plot
density_plot <- plot_data %>%
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
  scale_y_continuous(name = "Width Density Deviation") +
  labs(title = "Width Density Deviation by True Width and Set Size") +
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
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )

# Display plot
print(density_plot)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

ggsave(
  filename = "outputs/exp1/figures/width_density_deviation_analysis.png",
  plot = density_plot,
  width = 9,
  height = 5,
  dpi = 300,
  units = "in",
  bg = "white"
)

# ggsave(
#   filename = "outputs/exp1/figures/width_density_deviation_analysis.pdf",
#   plot = density_plot,
#   width = 9,
#   height = 5,
#   units = "in",
#   bg = "white"
# )

cat("\n=== Analysis Complete ===\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("- width_density_deviation_emmeans_by_width_setsize.csv\n")
cat("- width_density_deviation_rm_contrasts.csv\n")
cat("\nFigures saved to outputs/exp1/figures/:\n")
cat("- width_density_deviation_analysis.png\n")
cat("- width_density_deviation_analysis.pdf\n")
