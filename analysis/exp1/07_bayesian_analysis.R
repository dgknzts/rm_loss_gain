# Bayesian One-Sample T-Test Analysis - Experiment 1
# RM and Non-RM conditions vs 0

library(tidyverse)
library(BayesFactor)

# Load data
df <- read.csv("data/exp1/processed.csv")

# Prepare data
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(condition = ifelse(number_deviation == -1, "RM", "Non-RM"))

# Calculate mean relative width deviation for each subject and condition
subject_means <- df_analysis %>%
  group_by(subID, condition) %>%
  summarise(mean_width_deviation = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop")

# Separate RM and Non-RM means
rm_means <- subject_means %>% filter(condition == "RM") %>% pull(mean_width_deviation)
norm_means <- subject_means %>% filter(condition == "Non-RM") %>% pull(mean_width_deviation)

# Bayesian one-sample t-tests
bf_rm <- ttestBF(x = rm_means, mu = 0)
bf_norm <- ttestBF(x = norm_means, mu = 0)

# Extract posterior samples for effect size
posterior_rm <- posterior(bf_rm, iterations = 10000)
posterior_norm <- posterior(bf_norm, iterations = 10000)

delta_rm <- as.numeric(posterior_rm[, "delta"])
delta_norm <- as.numeric(posterior_norm[, "delta"])

# Calculate statistics
results <- list(
  rm = list(
    means = rm_means,
    delta = delta_rm,
    median_delta = median(delta_rm),
    ci = quantile(delta_rm, c(0.025, 0.975)),
    bf01 = 1 / exp(bf_rm@bayesFactor$bf),
    mean = mean(rm_means),
    se = sd(rm_means) / sqrt(length(rm_means)),
    n = length(rm_means)
  ),
  norm = list(
    means = norm_means,
    delta = delta_norm,
    median_delta = median(delta_norm),
    ci = quantile(delta_norm, c(0.025, 0.975)),
    bf01 = 1 / exp(bf_norm@bayesFactor$bf),
    mean = mean(norm_means),
    se = sd(norm_means) / sqrt(length(norm_means)),
    n = length(norm_means)
  )
)

# Calculate density at x=0 for plotting
rm_dens <- density(delta_rm, adjust = 1.2)
norm_dens <- density(delta_norm, adjust = 1.2)
results$rm$density_at_zero <- rm_dens$y[which.min(abs(rm_dens$x - 0))]
results$norm$density_at_zero <- norm_dens$y[which.min(abs(norm_dens$x - 0))]
results$prior_density_at_zero <- dcauchy(0, location = 0, scale = 0.707)

# Save results
save(results, file = "outputs/exp1/tables/bayesian_analysis_results.RData")

# Print summary
cat("Bayesian One-Sample T-Tests (Relative Width Deviation vs 0)\n\n")

cat("RM Condition:\n")
cat(sprintf("  N = %d, Mean = %.4f (SE = %.4f)\n", results$rm$n, results$rm$mean, results$rm$se))
cat(sprintf("  Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            results$rm$median_delta, results$rm$ci[1], results$rm$ci[2]))
cat(sprintf("  BF01 = %.4f\n\n", results$rm$bf01))

cat("Non-RM Condition:\n")
cat(sprintf("  N = %d, Mean = %.4f (SE = %.4f)\n", results$norm$n, results$norm$mean, results$norm$se))
cat(sprintf("  Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            results$norm$median_delta, results$norm$ci[1], results$norm$ci[2]))
cat(sprintf("  BF01 = %.4f\n", results$norm$bf01))
