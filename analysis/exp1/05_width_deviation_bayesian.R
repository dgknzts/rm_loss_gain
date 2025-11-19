# Bayesian One-Sample T-Test Analysis
# RM and Non-RM conditions vs 0

library(tidyverse)
library(BayesFactor)

# Load processed data
df <- read.csv("data/exp1/processed.csv")

# Prepare data
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    condition = ifelse(number_deviation == -1, "RM", "Non-RM"),
    subID = factor(subID),
    exp_version = factor(exp_version)
  )

# Calculate mean width deviation for each subject and condition
subject_means <- df_analysis %>%
  group_by(subID, condition, exp_version) %>%
  summarise(
    mean_width_deviation = mean(width_deviation, na.rm = TRUE),
    .groups = "drop"
  )

# Separate RM and Non-RM means
rm_means <- subject_means %>%
  filter(condition == "RM") %>%
  pull(mean_width_deviation)

norm_means <- subject_means %>%
  filter(condition == "Non-RM") %>%
  pull(mean_width_deviation)

# Bayesian one-sample t-tests
bf_rm <- ttestBF(x = rm_means, mu = 0)
bf_norm <- ttestBF(x = norm_means, mu = 0)

# Extract posterior samples for effect size
posterior_rm <- posterior(bf_rm, iterations = 10000)
posterior_norm <- posterior(bf_norm, iterations = 10000)

delta_rm <- as.numeric(posterior_rm[, "delta"])
delta_norm <- as.numeric(posterior_norm[, "delta"])

# Calculate statistics
median_rm <- median(delta_rm)
ci_rm <- quantile(delta_rm, c(0.025, 0.975))
bf_10_rm <- exp(bf_rm@bayesFactor$bf)
bf_01_rm <- 1 / bf_10_rm
mean_rm <- mean(rm_means)
se_rm <- sd(rm_means) / sqrt(length(rm_means))

median_norm <- median(delta_norm)
ci_norm <- quantile(delta_norm, c(0.025, 0.975))
bf_10_norm <- exp(bf_norm@bayesFactor$bf)
bf_01_norm <- 1 / bf_10_norm
mean_norm <- mean(norm_means)
se_norm <- sd(norm_means) / sqrt(length(norm_means))

# Calculate density at x=0 for plotting
rm_density <- density(delta_rm, adjust = 1.2)
norm_density <- density(delta_norm, adjust = 1.2)
rm_density_at_zero <- rm_density$y[which.min(abs(rm_density$x - 0))]
norm_density_at_zero <- norm_density$y[which.min(abs(norm_density$x - 0))]
prior_density_at_zero <- dcauchy(0, location = 0, scale = 0.707)

# Prepare plot data
plot_data <- data.frame(
  delta = c(delta_rm, delta_norm),
  condition = factor(rep(c("RM", "Non-RM"), c(length(delta_rm), length(delta_norm))))
)

# Robustness analysis
continuous_scales <- c(
  seq(0.00001, 0.0001, by = 0.00001),  # Very small values: 0.00001 to 0.0001
  seq(0.0001, 0.001, by = 0.0001),     # Small values: 0.0001 to 0.001  
  seq(0.001, 0.01, by = 0.001),        # Medium-small values: 0.001 to 0.01
  seq(0.01, 0.1, by = 0.01),           # Medium values: 0.01 to 0.1
  seq(0.1, 1.5, by = 0.01)             # Larger values: 0.1 to 1.5
)
robustness_continuous <- tibble()

for (scale in continuous_scales) {
  bf_rm_rob <- ttestBF(x = rm_means, mu = 0, rscale = scale)
  bf_norm_rob <- ttestBF(x = norm_means, mu = 0, rscale = scale)
  
  robustness_continuous <- bind_rows(
    robustness_continuous,
    tibble(
      condition = c("RM", "Non-RM"),
      prior_scale = scale,
      bf01 = c(1 / exp(bf_rm_rob@bayesFactor$bf), 1 / exp(bf_norm_rob@bayesFactor$bf))
    )
  )
}

# Find critical points
rm_data <- robustness_continuous %>% filter(condition == "RM")
norm_data <- robustness_continuous %>% filter(condition == "Non-RM")

rm_target_idx <- which.min(abs(rm_data$bf01 - 3))
norm_target_idx <- which.min(abs(norm_data$bf01 - 1/3))

special_points <- tibble(
  condition = c("RM", "RM", "Non-RM", "Non-RM"),
  prior_scale = c(0.707, rm_data$prior_scale[rm_target_idx], 0.707, norm_data$prior_scale[norm_target_idx]),
  bf01 = c(
    1 / exp(ttestBF(x = rm_means, mu = 0, rscale = 0.707)@bayesFactor$bf),
    rm_data$bf01[rm_target_idx],
    1 / exp(ttestBF(x = norm_means, mu = 0, rscale = 0.707)@bayesFactor$bf),
    norm_data$bf01[norm_target_idx]
  ),
  point_type = c("Default", "BF=3", "Default", "BF=1/3")
)

# Export results for plotting
bayesian_results <- list(
  rm_means = rm_means,
  norm_means = norm_means,
  delta_rm = delta_rm,
  delta_norm = delta_norm,
  median_rm = median_rm,
  median_norm = median_norm,
  ci_rm = ci_rm,
  ci_norm = ci_norm,
  bf_01_rm = bf_01_rm,
  bf_01_norm = bf_01_norm,
  rm_density_at_zero = rm_density_at_zero,
  norm_density_at_zero = norm_density_at_zero,
  prior_density_at_zero = prior_density_at_zero,
  plot_data = plot_data,
  robustness = list(
    rm_data = rm_data,
    norm_data = norm_data,
    rm_points = special_points %>% filter(condition == "RM"),
    norm_points = special_points %>% filter(condition == "Non-RM"),
    rm_critical_scale = special_points$prior_scale[2],
    rm_critical_bf = special_points$bf01[2],
    norm_critical_scale = special_points$prior_scale[4],
    norm_critical_bf = special_points$bf01[4],
    log_base = 10
  )
)

# Save results
if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}

save(bayesian_results, file = "outputs/exp1/tables/bayesian_analysis_results.RData")

# Print results
cat('Bayesian One-Sample T-Tests Results:\\n\\n')
cat('RM Condition:\\n')
cat(sprintf('N = %d subjects\\n', length(rm_means)))
cat(sprintf('Mean width deviation: %.4f (SE = %.4f)\\n', mean_rm, se_rm))
cat(sprintf('Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\\n', median_rm, ci_rm[1], ci_rm[2]))
cat(sprintf('Bayes Factor BF01: %.4f\\n\\n', bf_01_rm))

cat('Non-RM Condition:\\n')
cat(sprintf('N = %d subjects\\n', length(norm_means)))
cat(sprintf('Mean width deviation: %.4f (SE = %.4f)\\n', mean_norm, se_norm))
cat(sprintf('Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\\n', median_norm, ci_norm[1], ci_norm[2]))
cat(sprintf('Bayes Factor BF01: %.4f\\n\\n', bf_01_norm))

cat('Robustness Analysis:\\n')
cat(sprintf('RM reaches BF01 = 3 at r = %.3f\\n', special_points$prior_scale[2]))
cat(sprintf('Non-RM reaches BF01 = 1/3 at r = %.6f\\n', special_points$prior_scale[4]))