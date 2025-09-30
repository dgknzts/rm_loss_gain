# Width Deviation Statistical Analysis
# Mixed-effects model with emmeans contrasts

library(tidyverse)
library(lme4)
library(emmeans)

# Helper functions
format_num <- function(x, digits = 3) {
  formatC(x, format = 'f', digits = digits)
}

format_p <- function(p) {
  if (is.na(p)) 'p = NA'
  else if (p < 0.001) 'p < 0.001'
  else sprintf('p = %.3f', p)
}

# Load and prepare data
df <- read.csv('data/processed.csv')

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

# Fit model
model <- lmer(width_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

# Calculate emmeans and contrasts
emm_width_setsize <- emmeans(model, ~ rm_type | correct_width + correct_num)
width_dev_contrasts <- pairs(emm_width_setsize)

# Export results
if (!dir.exists('outputs/tables')) {
  dir.create('outputs/tables', recursive = TRUE)
}

write.csv(as.data.frame(summary(emm_width_setsize)),
          'outputs/tables/width_deviation_emmeans_by_width_setsize.csv', 
          row.names = FALSE)

width_contrast_summary <- summary(width_dev_contrasts, infer = TRUE)
write.csv(as.data.frame(width_contrast_summary),
          'outputs/tables/width_deviation_rm_contrasts.csv', 
          row.names = FALSE)

# Calculate effect sizes and format results
residual_sd <- sigma(model)
overall_emm <- emmeans(model, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)

# Overall effect
o_means <- as_tibble(summary(overall_emm))
o_rm <- o_means %>% filter(rm_type == 'RM')
o_norm <- o_means %>% filter(rm_type == 'Non-RM')

cohen_d_overall <- overall_contrast$estimate / residual_sd

cat('Overall RM vs Non-RM comparison:\\n')
cat(sprintf('RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s, d = %s\\n',
            format_num(o_rm$emmean), format_num(o_rm$SE), 
            format_num(o_norm$emmean), format_num(o_norm$SE),
            format_num(overall_contrast$z.ratio, 2), format_p(overall_contrast$p.value),
            format_num(cohen_d_overall, 3)))

# Individual contrasts with effect sizes
contrasts_with_d <- as_tibble(width_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

significant_effects <- sum(contrasts_with_d$p_fdr < 0.05)
total_effects <- nrow(contrasts_with_d)

cat(sprintf('\\nSignificant effects: %d out of %d comparisons (%.1f%%)\\n', 
            significant_effects, total_effects, 
            100 * significant_effects / total_effects))

strongest <- contrasts_with_d %>% slice_max(abs(estimate))
cat(sprintf('Strongest effect: Width %s, Set size %s (d = %s, %s)\\n',
            strongest$correct_width, strongest$correct_num,
            format_num(strongest$cohen_d, 3), format_p(strongest$p.value)))

# Detailed pairwise comparison output
cat('\\n=== DETAILED PAIRWISE COMPARISONS ===\\n')

# Get emmeans for detailed output
width_emmeans_detailed <- summary(emm_width_setsize)

for(i in 1:nrow(contrasts_with_d)) {
  row <- contrasts_with_d[i,]
  
  # Find corresponding emmeans
  rm_mean <- width_emmeans_detailed %>% 
    filter(correct_width == row$correct_width, 
           correct_num == row$correct_num, 
           rm_type == "RM")
  
  norm_mean <- width_emmeans_detailed %>% 
    filter(correct_width == row$correct_width, 
           correct_num == row$correct_num, 
           rm_type == "Non-RM")
  
  # Format p-values
  p_main <- format_p(row$p.value)
  p_fdr <- if(row$p_fdr < 0.001) "FDR p < 0.001" else sprintf("FDR p = %.3f", row$p_fdr)
  
  cat(sprintf('Width %s, Set size %s: RM (M = %s, SE = %s) vs Non-RM (M = %s, SE = %s), z = %s, %s (%s), d = %s, 95%% CI [%s, %s].\\n',
              row$correct_width, row$correct_num,
              format_num(rm_mean$emmean), format_num(rm_mean$SE),
              format_num(norm_mean$emmean), format_num(norm_mean$SE),
              format_num(row$z.ratio, 2), p_main, p_fdr,
              format_num(row$cohen_d, 3),
              format_num(row$asymp.LCL, 3), format_num(row$asymp.UCL, 3)))
}