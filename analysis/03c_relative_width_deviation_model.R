# ==============================================================================
# Bayesian Analysis of Width Deviation in Redundancy Masking
# ==============================================================================

# --- Setup ---
library(tidyverse)
library(rstanarm)
library(emmeans)
library(bayestestR)
library(ggplot2)
library(ggdist)
library(patchwork)

# Set up parallel processing
options(mc.cores = max(1, parallel::detectCores() - 1))
rstan::rstan_options(auto_write = TRUE)



# --- Data Import and Preprocessing ---
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

# --- Bayesian Model Fitting ---

set.seed(1234)
model <- stan_lmer(
  width_deviation_relative ~ rm_type * spacing_category * correct_num * correct_width + (1 | subID),
  data = analysis_data,
  prior = cauchy(0, 0.707, autoscale = FALSE),
  prior_intercept = student_t(3, 0, 10),
  prior_aux = exponential(0.1),
  prior_covariance = decov(regularization = 2, concentration = 1, shape = 1, scale = 1),
  chains = 4,
  iter = 3000,
  warmup = 1000,
  adapt_delta = 0.95,
  refresh = 0
)

# --- Extract Posterior Results ---
# Overall marginal means for RM vs NoRM
emm_overall <- emmeans(model, ~ rm_type)

# Get posterior summaries with proper column names
posterior_means <- summary(emm_overall, point.est = 'median', type = 'HPD', level = 0.95) %>%
  as_tibble() %>%
  rename(median = emmean, lower = lower.HPD, upper = upper.HPD)

# --- Bayes Factors ---

# Bayes factors for each condition vs zero
bf_means <- describe_posterior(
  emm_overall,
  centrality = 'median',
  dispersion = TRUE,
  ci = 0.95,
  ci_method = 'hdi',
  test = 'bayesfactor',
  bf_prior = model
) %>%
  as_tibble() %>%
  mutate(condition = c('NoRM', 'RM'))

# --- Create Output Directory ---
if (!dir.exists('outputs/tables')) {
  dir.create('outputs/tables', recursive = TRUE)
}

# --- Save Results ---
write.csv(posterior_means, 'outputs/tables/width_deviation_posterior_means.csv', row.names = FALSE)
write.csv(bf_means, 'outputs/tables/width_deviation_bayes_vs_zero.csv', row.names = FALSE)


# --- Print Summary ---
cat("\nBAYESIAN ANALYSIS SUMMARY\n")

cat("\nPosterior Means (95% HDI):\n")
for(i in 1:nrow(posterior_means)) {
  row <- posterior_means[i,]
  cat(sprintf("%s: %.4f [%.4f, %.4f]\n", 
              row$rm_type, row$median, row$lower, row$upper))
}

cat("\nBayes Factors (vs. zero):\n")
for(i in 1:nrow(bf_means)) {
  row <- bf_means[i,]
  cat(sprintf("%s: BF = %.2f\n", row$condition, row$log_BF))
}

cat("\nAnalysis complete!\n")