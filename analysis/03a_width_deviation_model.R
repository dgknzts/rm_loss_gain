# --- Setup ---
library(tidyverse)
library(lme4)
library(emmeans)

format_num <- function(x, digits = 3) {
  formatC(x, format = 'f', digits = digits)
}

format_p <- function(p) {
  if (is.na(p)) {
    'p = NA'
  } else if (p < 0.001) {
    'p < 0.001'
  } else {
    sprintf('p = %.3f', p)
  }
}

add_ci_columns <- function(df) {
  if ('asymp.LCL' %in% names(df) && !('lower.CL' %in% names(df))) {
    df$lower.CL <- df$asymp.LCL
  }
  if ('asymp.UCL' %in% names(df) && !('upper.CL' %in% names(df))) {
    df$upper.CL <- df$asymp.UCL
  }
  if (!('lower.CL' %in% names(df))) {
    df$lower.CL <- NA_real_
  }
  if (!('upper.CL' %in% names(df))) {
    df$upper.CL <- NA_real_
  }
  if ('asymp.LCL' %in% names(df)) df$asymp.LCL <- NULL
  if ('asymp.UCL' %in% names(df)) df$asymp.UCL <- NULL
  df
}

# --- Import and clean ---
df <- read.csv('data/processed.csv')

spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'NoRM'), levels = c('NoRM', 'RM')),
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

# --- Model and emmeans ---
model <- lmer(width_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
              data = analysis_data, REML = FALSE)

emm_width_setsize <- emmeans(model, ~ rm_type | correct_width + correct_num)
width_dev_contrasts <- pairs(emm_width_setsize)

# --- Save tables ---
if (!dir.exists('outputs/tables')) {
  dir.create('outputs/tables', recursive = TRUE)
}

width_emm_summary <- summary(emm_width_setsize)
width_contrast_summary <- summary(width_dev_contrasts, infer = TRUE)
width_contrast_summary <- add_ci_columns(width_contrast_summary)

write.csv(as.data.frame(width_emm_summary),
          'outputs/tables/width_deviation_emmeans_by_width_setsize.csv', row.names = FALSE)
write.csv(as.data.frame(width_contrast_summary),
          'outputs/tables/width_deviation_rm_contrasts.csv', row.names = FALSE)

# --- APA-style summaries ---
residual_sd <- sigma(model)

overall_emm <- emmeans(model, ~ rm_type)
overall_means <- as_tibble(summary(overall_emm))

overall_contrast_df <- summary(pairs(overall_emm), infer = TRUE)
overall_contrast_df <- add_ci_columns(overall_contrast_df)

overall_contrast <- as_tibble(overall_contrast_df) %>%
  mutate(cohen_d = estimate / residual_sd,
         p_text = map_chr(p.value, format_p),
         z_text = format_num(z.ratio, digits = 2),
         ci_lower = format_num(lower.CL, digits = 3),
         ci_upper = format_num(upper.CL, digits = 3),
         d_text = format_num(cohen_d, digits = 3))

o_rm <- overall_means %>% filter(rm_type == 'RM')
o_norm <- overall_means %>% filter(rm_type == 'NoRM')

overall_sentence <- sprintf(
  'RM trials (M = %s, SE = %s) differed from NoRM trials (M = %s, SE = %s), z = %s, %s, d = %s, 95%% CI [%s, %s].',
  format_num(o_rm$emmean), format_num(o_rm$SE), format_num(o_norm$emmean), format_num(o_norm$SE),
  overall_contrast$z_text, overall_contrast$p_text, overall_contrast$d_text,
  overall_contrast$ci_lower, overall_contrast$ci_upper
)

means_wide <- as_tibble(width_emm_summary) %>%
  select(correct_width, correct_num, rm_type, emmean, SE) %>%
  pivot_wider(names_from = rm_type, values_from = c(emmean, SE), names_sep = '_')

contrast_table <- as_tibble(width_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH'),
    p_text = map_chr(p.value, format_p),
    p_fdr_text = map_chr(p_fdr, format_p),
    z_text = format_num(z.ratio, digits = 2),
    ci_lower = format_num(lower.CL, digits = 3),
    ci_upper = format_num(upper.CL, digits = 3),
    d_text = format_num(cohen_d, digits = 3)
  ) %>%
  left_join(means_wide, by = c('correct_width', 'correct_num'))

n_conditions <- nrow(contrast_table)
significant_conditions <- contrast_table %>% filter(p_fdr < 0.05) %>% nrow()
percent_significant <- if (n_conditions == 0) 0 else round(significant_conditions / n_conditions * 100, 1)

strongest_effect <- contrast_table %>% slice_max(order_by = abs(estimate), n = 1)

cat('\n--- APA Summary ---\n')
cat(overall_sentence, '\n\n')
cat(sprintf('RM vs NoRM effects were evaluated across %d width x set-size cells. Significant FDR-adjusted effects appeared in %d of %d cells (%s%%).\n',
            n_conditions, significant_conditions, n_conditions, format_num(percent_significant, digits = 1)))

if (n_conditions > 0) {
  cat(sprintf('Strongest effect: width %s, set size %s (RM - NoRM = %s, z = %s, %s; FDR %s, d = %s, 95%% CI [%s, %s]).\n\n',
              strongest_effect$correct_width, strongest_effect$correct_num,
              format_num(strongest_effect$estimate, digits = 3),
              strongest_effect$z_text, strongest_effect$p_text, strongest_effect$p_fdr_text,
              strongest_effect$d_text, strongest_effect$ci_lower, strongest_effect$ci_upper))

  apply(contrast_table, 1, function(row) {
    cat(sprintf('Width %s, set size %s: RM (M = %s, SE = %s) vs NoRM (M = %s, SE = %s), z = %s, %s (FDR %s), d = %s, 95%% CI [%s, %s].\n',
                row[['correct_width']], row[['correct_num']],
                format_num(as.numeric(row[['emmean_RM']])), format_num(as.numeric(row[['SE_RM']])),
                format_num(as.numeric(row[['emmean_NoRM']])), format_num(as.numeric(row[['SE_NoRM']])),
                row[['z_text']], row[['p_text']], row[['p_fdr_text']], row[['d_text']],
                row[['ci_lower']], row[['ci_upper']]))
  })
}

cat('\nWidth deviation analysis complete. Copy the sentences above directly into the manuscript.\n')
