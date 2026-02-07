# Density Deviation Analysis - Experiment 1
# Testing whether density perception is similar in RM vs Non-RM trials
# Grouped by density category (Low/Medium/High)

library(tidyverse)
library(BayesFactor)

source("analysis/functions/themes.R")

# RM colors
rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

df <- read.csv("data/exp1/processed.csv")

# Filter for RM and Non-RM trials only
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    subID = factor(subID)
  ) %>%
  drop_na(width_density_deviation, actual_width_density)

# Create density categories based on tertiles
density_tertiles <- quantile(analysis_data$actual_width_density, probs = c(1/3, 2/3))

analysis_data <- analysis_data %>%
  mutate(
    density_category = case_when(
      actual_width_density <= density_tertiles[1] ~ "Low",
      actual_width_density <= density_tertiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    density_category = factor(density_category, levels = c("Low", "Medium", "High"))
  )

cat("N trials:", nrow(analysis_data), "\n")
cat("N subjects:", length(unique(analysis_data$subID)), "\n")
cat("\nDensity tertile cutoffs:", round(density_tertiles, 3), "\n")
cat("Trials per density category:\n")
print(table(analysis_data$density_category))

# =============================================================================
# 2. DESCRIPTIVE STATISTICS BY DENSITY CATEGORY
# =============================================================================

# Subject means by condition and density category
subject_means <- analysis_data %>%
  group_by(subID, rm_type, density_category) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Grand means by density category
grand_means <- subject_means %>%
  group_by(rm_type, density_category) %>%
  summarise(
    n = n(),
    mean = mean(mean_density_dev),
    sd = sd(mean_density_dev),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

cat("\n=== DESCRIPTIVE STATISTICS BY DENSITY CATEGORY ===\n")
print(grand_means)

# Overall means (collapsed across density)
overall_means <- subject_means %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density_dev = mean(mean_density_dev), .groups = "drop") %>%
  group_by(rm_type) %>%
  summarise(
    n = n(),
    mean = mean(mean_density_dev),
    sd = sd(mean_density_dev),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

cat("\n=== OVERALL MEANS (COLLAPSED) ===\n")
print(overall_means)

# =============================================================================
# 3. BAYESIAN ANALYSIS BY DENSITY CATEGORY
# =============================================================================

cat("\n=== BAYESIAN ANALYSIS BY DENSITY CATEGORY ===\n")

bayesian_results <- list()

for (dens_cat in c("Low", "Medium", "High")) {
  cat(sprintf("\n--- %s Density ---\n", dens_cat))

  # Get paired differences for this density category
  paired_data <- subject_means %>%
    filter(density_category == dens_cat) %>%
    select(subID, rm_type, mean_density_dev) %>%
    pivot_wider(names_from = rm_type, values_from = mean_density_dev) %>%
    mutate(difference = RM - `Non-RM`) %>%
    drop_na(difference)

  cat(sprintf("N subjects: %d\n", nrow(paired_data)))

  if (nrow(paired_data) >= 3) {
    # Bayesian paired t-test
    bf_result <- ttestBF(x = paired_data$difference, mu = 0)
    bf_10 <- exp(bf_result@bayesFactor$bf)
    bf_01 <- 1 / bf_10

    # Posterior samples
    posterior <- posterior(bf_result, iterations = 10000)
    delta <- as.numeric(posterior[, "delta"])

    bayesian_results[[dens_cat]] <- list(
      bf_01 = bf_01,
      bf_10 = bf_10,
      delta = delta,
      median_delta = median(delta),
      ci = quantile(delta, c(0.025, 0.975)),
      paired_data = paired_data
    )

    cat(sprintf("BF01 (evidence for no difference): %.2f\n", bf_01))
    cat(sprintf("Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
                median(delta), quantile(delta, 0.025), quantile(delta, 0.975)))

    if (bf_01 > 3) {
      cat("-> Moderate evidence for H0: No difference\n")
    } else if (bf_01 > 1) {
      cat("-> Anecdotal evidence for H0\n")
    } else {
      cat("-> Evidence favors H1: There IS a difference\n")
    }
  }
}

# =============================================================================
# 4. OVERALL BAYESIAN TEST (COLLAPSED ACROSS DENSITY)
# =============================================================================

cat("\n=== OVERALL BAYESIAN TEST ===\n")

overall_paired <- subject_means %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density_dev = mean(mean_density_dev), .groups = "drop") %>%
  pivot_wider(names_from = rm_type, values_from = mean_density_dev) %>%
  mutate(difference = RM - `Non-RM`) %>%
  drop_na(difference)

bf_overall <- ttestBF(x = overall_paired$difference, mu = 0)
bf_10_overall <- exp(bf_overall@bayesFactor$bf)
bf_01_overall <- 1 / bf_10_overall

posterior_overall <- posterior(bf_overall, iterations = 10000)
delta_overall <- as.numeric(posterior_overall[, "delta"])

cat(sprintf("N subjects: %d\n", nrow(overall_paired)))
cat(sprintf("BF01 (overall): %.2f\n", bf_01_overall))
cat(sprintf("Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            median(delta_overall), quantile(delta_overall, 0.025), quantile(delta_overall, 0.975)))

# =============================================================================
# 5. SAVE RESULTS
# =============================================================================

density_results <- list(
  grand_means = grand_means,
  overall_means = overall_means,
  subject_means = subject_means,
  bayesian_by_category = bayesian_results,
  bf_01_overall = bf_01_overall,
  delta_overall = delta_overall,
  overall_paired = overall_paired,
  density_tertiles = density_tertiles
)

save(density_results, file = "outputs/exp1/tables/density_deviation_results.RData")
write.csv(grand_means, "outputs/exp1/tables/density_deviation_means.csv", row.names = FALSE)

cat("\nResults saved to outputs/exp1/tables/\n")
