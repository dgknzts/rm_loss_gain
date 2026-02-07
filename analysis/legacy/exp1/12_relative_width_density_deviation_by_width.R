# Relative Width Density Deviation by True Width (Collapsed Across Set Sizes)
# Mixed-effects model with emmeans contrasts + visualization

library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)

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
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger')),
    relative_width_density_deviation = width_density_deviation / actual_width_density
  ) %>%
  drop_na(relative_width_density_deviation) %>%
  filter(is.finite(relative_width_density_deviation))

cat(sprintf("Analysis data: %d trials from %d participants\n",
            nrow(analysis_data), length(unique(analysis_data$subID))))

# =============================================================================
# STATISTICAL MODEL
# =============================================================================

model <- lmer(relative_width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

# emmeans by width only (collapsed across set sizes)
emm_width <- emmeans(model, ~ rm_type | correct_width)
width_contrasts <- pairs(emm_width)

# =============================================================================
# EXPORT RESULTS
# =============================================================================

if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}

write.csv(as.data.frame(summary(emm_width)),
          'outputs/exp1/tables/relative_width_density_deviation_emmeans_by_width.csv',
          row.names = FALSE)

contrast_summary <- summary(width_contrasts, infer = TRUE)
write.csv(as.data.frame(contrast_summary),
          'outputs/exp1/tables/relative_width_density_deviation_rm_contrasts_by_width.csv',
          row.names = FALSE)

# =============================================================================
# EFFECT SIZES
# =============================================================================

residual_sd <- sigma(model)

contrasts_with_d <- as_tibble(contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

cat('\n=== RM vs NON-RM BY TRUE WIDTH ===\n')
emm_detailed <- summary(emm_width)

for(i in 1:nrow(contrasts_with_d)) {
  row <- contrasts_with_d[i,]
  rm_mean <- emm_detailed %>% filter(correct_width == row$correct_width, rm_type == "RM")
  norm_mean <- emm_detailed %>% filter(correct_width == row$correct_width, rm_type == "Non-RM")

  cat(sprintf('Width %s: RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s, d = %s\n',
              row$correct_width,
              format_num(rm_mean$emmean), format_num(rm_mean$SE),
              format_num(norm_mean$emmean), format_num(norm_mean$SE),
              format_num(row$z.ratio, 2), format_p(row$p.value),
              format_num(row$cohen_d, 3)))
}

# =============================================================================
# VISUALIZATION
# =============================================================================

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

participant_means <- analysis_data %>%
  group_by(subID, correct_width, rm_type) %>%
  summarise(participant_mean = mean(relative_width_density_deviation, na.rm = TRUE), .groups = "drop")

plot_data <- participant_means %>%
  group_by(correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Significance stars
star_data <- contrast_summary %>%
  as_tibble() %>%
  mutate(
    correct_width = factor(correct_width),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  filter(significance != "") %>%
  left_join(
    plot_data %>%
      group_by(correct_width) %>%
      summarise(star_y = max(ci_upper) + 0.01, .groups = "drop"),
    by = "correct_width"
  )

density_plot <- plot_data %>%
  ggplot(aes(x = correct_width, y = grand_mean, color = rm_type)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2, size = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  geom_text(
    data = star_data,
    aes(x = correct_width, y = star_y, label = significance),
    color = "black", size = 5, hjust = 0.5, inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "True Width (°)") +
  scale_y_continuous(name = "Relative Width Density Deviation", labels = scales::percent_format()) +
  labs(title = "Relative Width Density Deviation by True Width",
       subtitle = "Collapsed across all set sizes, normalized by actual density") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11)
  )

print(density_plot)

# Save
if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

ggsave("outputs/exp1/figures/relative_width_density_deviation_by_width.png",
       density_plot, width = 7, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp1/figures/relative_width_density_deviation_by_width.pdf",
       density_plot, width = 7, height = 5, bg = "white")

cat("\nSaved: outputs/exp1/figures/relative_width_density_deviation_by_width.png\n")
