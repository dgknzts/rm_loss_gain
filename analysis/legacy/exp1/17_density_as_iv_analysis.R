# Density as Independent Variable Analysis
# Categorical and continuous density models with density deviation as outcome
# Model: rm_type * density * correct_num + (1 | subID)

library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(patchwork)

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

df <- read.csv('data/exp1/processed.csv')

# Create density tertiles
density_cuts <- quantile(df$actual_width_density, probs = c(1/3, 2/3), na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    density_category = case_when(
      actual_width_density <= density_cuts[1] ~ 'Low',
      actual_width_density <= density_cuts[2] ~ 'Medium',
      TRUE ~ 'High'
    ),
    density_category = factor(density_category, levels = c('Low', 'Medium', 'High'))
  ) %>%
  drop_na(width_density_deviation, actual_width_density)

cat(sprintf("Analysis data: %d trials from %d participants\n",
            nrow(analysis_data), length(unique(analysis_data$subID))))
cat(sprintf("Density tertile cutoffs: %.3f, %.3f\n", density_cuts[1], density_cuts[2]))

# Print density distribution
cat("\nDensity category distribution:\n")
print(table(analysis_data$density_category, analysis_data$rm_type))

# =============================================================================
# MODEL 1: CATEGORICAL DENSITY
# =============================================================================

cat("\n" , strrep("=", 60), "\n")
cat("MODEL 1: CATEGORICAL DENSITY ANALYSIS\n")
cat(strrep("=", 60), "\n")

model_cat <- lmer(width_density_deviation ~ rm_type * density_category * correct_num + (1 | subID),
                  data = analysis_data, REML = FALSE)

# emmeans by density_category and set size
emm_cat <- emmeans(model_cat, ~ rm_type | density_category + correct_num)
cat_contrasts <- pairs(emm_cat)

# Export results
if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}

write.csv(as.data.frame(summary(emm_cat)),
          'outputs/exp1/tables/density_as_iv_emmeans_categorical.csv',
          row.names = FALSE)

cat_contrast_summary <- summary(cat_contrasts, infer = TRUE)
write.csv(as.data.frame(cat_contrast_summary),
          'outputs/exp1/tables/density_as_iv_rm_contrasts.csv',
          row.names = FALSE)

# Effect sizes
residual_sd <- sigma(model_cat)
overall_emm <- emmeans(model_cat, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)

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

# Pairwise contrasts with effect sizes
contrasts_with_d <- as_tibble(cat_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

significant_effects <- sum(contrasts_with_d$p_fdr < 0.05)
total_effects <- nrow(contrasts_with_d)

cat(sprintf('\nSignificant effects: %d out of %d comparisons (%.1f%%)\n',
            significant_effects, total_effects,
            100 * significant_effects / total_effects))

# Detailed pairwise output
cat('\n=== DETAILED PAIRWISE COMPARISONS ===\n')
emm_detailed <- summary(emm_cat)

for(i in 1:nrow(contrasts_with_d)) {
  row <- contrasts_with_d[i,]

  rm_mean <- emm_detailed %>%
    filter(density_category == row$density_category,
           correct_num == row$correct_num,
           rm_type == "RM")

  norm_mean <- emm_detailed %>%
    filter(density_category == row$density_category,
           correct_num == row$correct_num,
           rm_type == "Non-RM")

  p_main <- format_p(row$p.value)
  p_fdr <- if(row$p_fdr < 0.001) "FDR p < 0.001" else sprintf("FDR p = %.3f", row$p_fdr)

  cat(sprintf('Density %s, Set size %s: RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s (%s), d = %s\n',
              row$density_category, row$correct_num,
              format_num(rm_mean$emmean), format_num(rm_mean$SE),
              format_num(norm_mean$emmean), format_num(norm_mean$SE),
              format_num(row$z.ratio, 2), p_main, p_fdr,
              format_num(row$cohen_d, 3)))
}

# =============================================================================
# MODEL 2: CONTINUOUS DENSITY
# =============================================================================

cat("\n", strrep("=", 60), "\n")
cat("MODEL 2: CONTINUOUS DENSITY ANALYSIS\n")
cat(strrep("=", 60), "\n")

model_cont <- lmer(width_density_deviation ~ rm_type * actual_width_density * correct_num + (1 | subID),
                   data = analysis_data, REML = FALSE)

# Model summary
cont_summary <- summary(model_cont)
cat("\nFixed effects:\n")
print(cont_summary$coefficients)

# Save continuous model summary
cont_coef <- as.data.frame(cont_summary$coefficients)
cont_coef$term <- rownames(cont_coef)
write.csv(cont_coef,
          'outputs/exp1/tables/density_as_iv_continuous_model_summary.csv',
          row.names = FALSE)

# Test interaction effects
cat("\n=== KEY INTERACTION EFFECTS ===\n")
coef_table <- cont_summary$coefficients
interaction_rows <- grep(":", rownames(coef_table))
for (row_idx in interaction_rows) {
  term <- rownames(coef_table)[row_idx]
  est <- coef_table[row_idx, "Estimate"]
  se <- coef_table[row_idx, "Std. Error"]
  t_val <- coef_table[row_idx, "t value"]
  cat(sprintf("%s: b = %.4f, SE = %.4f, t = %.2f\n", term, est, se, t_val))
}

# =============================================================================
# VISUALIZATION 1: CATEGORICAL DENSITY PLOT
# =============================================================================

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Participant means
participant_means <- analysis_data %>%
  group_by(subID, density_category, correct_num, rm_type) %>%
  summarise(participant_mean = mean(width_density_deviation, na.rm = TRUE), .groups = "drop")

plot_data <- participant_means %>%
  group_by(density_category, correct_num, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Significance stars
contrast_results <- cat_contrast_summary %>%
  as_tibble() %>%
  mutate(
    density_category = factor(density_category, levels = c('Low', 'Medium', 'High')),
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
      group_by(density_category, correct_num) %>%
      summarise(star_y = max(ci_upper) + 0.008, .groups = "drop"),
    by = c("density_category", "correct_num")
  )

cat_plot <- plot_data %>%
  ggplot(aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2, size = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_text(
    data = star_data,
    aes(x = correct_num, y = star_y, label = significance),
    color = "black", size = 5, hjust = 0.5, inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  facet_wrap(~density_category, labeller = as_labeller(function(x) paste("Density:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Density Deviation") +
  labs(title = "Width Density Deviation by Actual Density Category and Set Size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.92, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(cat_plot)

# =============================================================================
# VISUALIZATION 2: CONTINUOUS DENSITY PLOT
# =============================================================================

# Participant means for continuous plot
participant_means_cont <- analysis_data %>%
  group_by(subID, correct_num, rm_type) %>%
  summarise(
    mean_density = mean(actual_width_density, na.rm = TRUE),
    mean_deviation = mean(width_density_deviation, na.rm = TRUE),
    .groups = "drop"
  )

cont_plot <- ggplot(analysis_data, aes(x = actual_width_density, y = width_density_deviation, color = rm_type)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_continuous(name = "Actual Width Density") +
  scale_y_continuous(name = "Width Density Deviation") +
  labs(title = "Width Density Deviation as Function of Actual Density",
       subtitle = "Linear regression by RM type and set size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.92, 0.15),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(cont_plot)

# =============================================================================
# VISUALIZATION 3: ACTUAL vs REPORTED DENSITY SCATTER
# =============================================================================

# Calculate correlation statistics
calc_corr <- function(data) {
  test <- cor.test(data$actual_width_density, data$response_width_density, method = "pearson")
  sprintf("r = %.3f, p %s", test$estimate,
          if (test$p.value < 0.001) "< .001" else sprintf("= %.3f", test$p.value))
}

corr_by_rm <- analysis_data %>%
  group_by(rm_type) %>%
  summarise(
    corr_label = calc_corr(cur_data()),
    .groups = "drop"
  )

# Create scatter plot
scatter_plot <- ggplot(analysis_data, aes(x = actual_width_density, y = response_width_density, color = rm_type)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  annotate("text", x = 0.15, y = 0.95, label = corr_by_rm$corr_label[corr_by_rm$rm_type == "RM"],
           color = rm_colors["RM"], size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.15, y = 0.88, label = corr_by_rm$corr_label[corr_by_rm$rm_type == "Non-RM"],
           color = rm_colors["Non-RM"], size = 4, fontface = "bold", hjust = 0) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_continuous(name = "Actual Width Density", limits = c(0.1, 1)) +
  scale_y_continuous(name = "Reported Width Density", limits = c(0, 1)) +
  labs(title = "Actual vs Reported Width Density",
       subtitle = "Dashed line = perfect accuracy (y = x)") +
  coord_fixed(ratio = 1) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(scatter_plot)

# Scatter plot faceted by set size
scatter_by_setsize <- ggplot(analysis_data, aes(x = actual_width_density, y = response_width_density, color = rm_type)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_continuous(name = "Actual Width Density") +
  scale_y_continuous(name = "Reported Width Density") +
  labs(title = "Actual vs Reported Width Density by Set Size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(scatter_by_setsize)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

ggsave("outputs/exp1/figures/density_as_iv_categorical_analysis.png",
       cat_plot, width = 10, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_as_iv_categorical_analysis.pdf",
       cat_plot, width = 10, height = 5, bg = "white")

ggsave("outputs/exp1/figures/density_as_iv_continuous_analysis.png",
       cont_plot, width = 10, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/density_as_iv_continuous_analysis.pdf",
       cont_plot, width = 10, height = 5, bg = "white")

ggsave("outputs/exp1/figures/actual_vs_reported_density.png",
       scatter_plot, width = 7, height = 7, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/actual_vs_reported_density.pdf",
       scatter_plot, width = 7, height = 7, bg = "white")

ggsave("outputs/exp1/figures/actual_vs_reported_density_by_setsize.png",
       scatter_by_setsize, width = 10, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/actual_vs_reported_density_by_setsize.pdf",
       scatter_by_setsize, width = 10, height = 5, bg = "white")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("- density_as_iv_emmeans_categorical.csv\n")
cat("- density_as_iv_rm_contrasts.csv\n")
cat("- density_as_iv_continuous_model_summary.csv\n")
cat("\nFigures saved to outputs/exp1/figures/:\n")
cat("- density_as_iv_categorical_analysis.png/pdf\n")
cat("- density_as_iv_continuous_analysis.png/pdf\n")
cat("- actual_vs_reported_density.png/pdf\n")
cat("- actual_vs_reported_density_by_setsize.png/pdf\n")
