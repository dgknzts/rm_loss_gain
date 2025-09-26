# --- Setup ---
library(tidyverse)
library(rstanarm)
library(emmeans)
library(bayestestR)
library(coda)

options(mc.cores = max(1, parallel::detectCores() - 1))
if (requireNamespace('rstan', quietly = TRUE)) {
  rstan::rstan_options(auto_write = TRUE)
}

standardize_hpd_columns <- function(df) {
  if ('estimate' %in% names(df)) {
    names(df)[names(df) == 'estimate'] <- 'median'
  } else if ('Estimate' %in% names(df)) {
    names(df)[names(df) == 'Estimate'] <- 'median'
  } else if (!('median' %in% names(df)) && ('point.est' %in% names(df))) {
    names(df)[names(df) == 'point.est'] <- 'median'
  }
  if ('lower.HPD' %in% names(df)) {
    names(df)[names(df) == 'lower.HPD'] <- 'lower'
  } else if ('HPD.lower' %in% names(df)) {
    names(df)[names(df) == 'HPD.lower'] <- 'lower'
  }
  if ('upper.HPD' %in% names(df)) {
    names(df)[names(df) == 'upper.HPD'] <- 'upper'
  } else if ('HPD.upper' %in% names(df)) {
    names(df)[names(df) == 'HPD.upper'] <- 'upper'
  }
  df
}

append_level_column <- function(df) {
  param_cols <- c('Parameter', 'parameter', 'Term', 'term', 'contrast', 'label', 'Comparison')
  param_col <- param_cols[param_cols %in% names(df)]
  if (length(param_col) == 0) {
    df$level <- NA_character_
  } else {
    df$level <- case_when(
      str_detect(df[[param_col[1]]], 'RM') ~ 'RM',
      str_detect(df[[param_col[1]]], 'NoRM') ~ 'NoRM',
      TRUE ~ df[[param_col[1]]]
    )
  }
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
  filter(is.finite(width_deviation_relative))

# --- Bayesian model ---
set.seed(1234)
model <- stan_lmer(
  width_deviation_relative ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
  data = analysis_data,
  prior = cauchy(0, 0.707, autoscale = FALSE),  # Half-Cauchy prior on standardized effects (Rouder et al., 2012 defaults)
  prior_intercept = student_t(3, 0, 10),
  prior_aux = exponential(0.1),
  prior_covariance = decov(regularization = 2, concentration = 1, shape = 1, scale = 1),
  chains = 4,
  iter = 3000,
  warmup = 1000,
  adapt_delta = 0.95,
  refresh = 0
)

# --- Posterior marginal means and contrasts ---
emm_overall <- emmeans(model, ~ rm_type)
posterior_means <- summary(emm_overall, point.est = 'median', type = 'HPD', level = 0.95) %>%
  as_tibble() %>%
  standardize_hpd_columns()

overall_difference <- contrast(emm_overall, method = list('RM - NoRM' = c(-1, 1)))
posterior_difference <- summary(overall_difference, point.est = 'median', type = 'HPD', level = 0.95) %>%
  as_tibble() %>%
  standardize_hpd_columns()

# --- Bayes factors via Savage-Dickey ---
bf_means_tbl <- describe_posterior(
  emm_overall,
  centrality = 'median',
  dispersion = TRUE,
  ci = 0.95,
  ci_method = 'hdi',
  test = 'bayesfactor',
  bf_prior = model
) %>%
  as_tibble() %>%
  append_level_column()

bf_difference_tbl <- describe_posterior(
  overall_difference,
  centrality = 'median',
  dispersion = TRUE,
  ci = 0.95,
  ci_method = 'hdi',
  test = 'bayesfactor',
  bf_prior = model
) %>%
  as_tibble()

# --- Persist summaries ---
if (!dir.exists('outputs/tables')) {
  dir.create('outputs/tables', recursive = TRUE)
}

write.csv(posterior_means, 'outputs/tables/relative_width_deviation_posterior_means.csv', row.names = FALSE)
write.csv(posterior_difference, 'outputs/tables/relative_width_deviation_posterior_difference.csv', row.names = FALSE)
write.csv(bf_means_tbl, 'outputs/tables/relative_width_deviation_bayes_vs_zero.csv', row.names = FALSE)
write.csv(bf_difference_tbl, 'outputs/tables/relative_width_deviation_bayes_difference.csv', row.names = FALSE)

# --- Posterior density plot against zero ---
emm_draws <- as.data.frame(as.mcmc(emm_overall)) %>%
  mutate(.draw = row_number()) %>%
  pivot_longer(-.draw, names_to = 'term', values_to = 'estimate') %>%
  mutate(rm_type = stringr::str_remove(term, '^rm_type')) %>%
  select(-term)

bayes_density_plot <- ggplot(emm_draws, aes(x = estimate, fill = rm_type, color = rm_type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black') +
  labs(
    title = 'Posterior means: relative width deviation',
    x = 'Relative width deviation',
    y = 'Density',
    fill = 'Condition',
    color = 'Condition'
  ) +
  theme_minimal()

bayes_density_plot

