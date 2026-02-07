# Density Deviation Analysis by Set Size and Density Category - Experiment 1
# Testing density perception across set sizes (3, 4, 5) and density levels (Low/Medium/High)

library(tidyverse)
library(BayesFactor)

source("analysis/functions/themes.R")

rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

df <- read.csv("data/exp1/processed.csv")

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    subID = factor(subID),
    correct_num = factor(correct_num)
  ) %>%
  drop_na(width_density_deviation, actual_width_density)

# Create density categories based on tertiles (3 equal groups)
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
cat("\nTrials per Set Size × Density Category:\n")
print(table(analysis_data$correct_num, analysis_data$density_category))

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================

# Full breakdown: Set Size × Density Category × RM
subject_means <- analysis_data %>%
  group_by(subID, rm_type, correct_num, density_category) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

grand_means <- subject_means %>%
  group_by(rm_type, correct_num, density_category) %>%
  summarise(
    n = n(),
    mean = mean(mean_density_dev),
    sd = sd(mean_density_dev),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

cat("\n=== DESCRIPTIVE STATISTICS (Set Size × Density × RM) ===\n")
print(grand_means)

# By Set Size only
grand_means_setsize <- subject_means %>%
  group_by(subID, rm_type, correct_num) %>%
  summarise(mean_density_dev = mean(mean_density_dev), .groups = "drop") %>%
  group_by(rm_type, correct_num) %>%
  summarise(
    n = n(),
    mean = mean(mean_density_dev),
    sd = sd(mean_density_dev),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

cat("\n=== MEANS BY SET SIZE ===\n")
print(grand_means_setsize)

# =============================================================================
# 3. BAYESIAN ANALYSIS BY SET SIZE × DENSITY CATEGORY
# =============================================================================

cat("\n=== BAYESIAN ANALYSIS BY SET SIZE × DENSITY ===\n")

bayesian_results <- list()

for (ss in c("3", "4", "5")) {
  for (dens in c("Low", "Medium", "High")) {
    condition_key <- paste0("ss", ss, "_", dens)
    cat(sprintf("\n--- Set Size: %s, Density: %s ---\n", ss, dens))

    paired_data <- subject_means %>%
      filter(correct_num == ss, density_category == dens) %>%
      select(subID, rm_type, mean_density_dev) %>%
      pivot_wider(names_from = rm_type, values_from = mean_density_dev) %>%
      mutate(difference = RM - `Non-RM`) %>%
      drop_na(difference)

    cat(sprintf("N subjects: %d\n", nrow(paired_data)))

    if (nrow(paired_data) >= 3) {
      bf_result <- ttestBF(x = paired_data$difference, mu = 0)
      bf_10 <- exp(bf_result@bayesFactor$bf)
      bf_01 <- 1 / bf_10

      posterior <- posterior(bf_result, iterations = 10000)
      delta <- as.numeric(posterior[, "delta"])

      bayesian_results[[condition_key]] <- list(
        correct_num = ss,
        density_category = dens,
        bf_01 = bf_01,
        bf_10 = bf_10,
        delta = delta,
        median_delta = median(delta),
        ci = quantile(delta, c(0.025, 0.975)),
        n_subjects = nrow(paired_data)
      )

      cat(sprintf("BF01: %.2f, delta: %.3f [%.3f, %.3f]\n",
                  bf_01, median(delta), quantile(delta, 0.025), quantile(delta, 0.975)))
    } else {
      cat("Insufficient data\n")
    }
  }
}

# =============================================================================
# 4. CREATE BF SUMMARY TABLE
# =============================================================================

bf_summary <- bind_rows(lapply(bayesian_results, function(x) {
  tibble(
    correct_num = x$correct_num,
    density_category = x$density_category,
    bf_01 = x$bf_01,
    median_delta = x$median_delta,
    ci_lower = x$ci[1],
    ci_upper = x$ci[2],
    n_subjects = x$n_subjects
  )
}))

cat("\n=== BF SUMMARY TABLE ===\n")
print(bf_summary)

# =============================================================================
# 5. SAVE RESULTS
# =============================================================================

density_by_condition_results <- list(
  grand_means = grand_means,
  grand_means_setsize = grand_means_setsize,
  subject_means = subject_means,
  bayesian_results = bayesian_results,
  bf_summary = bf_summary,
  density_tertiles = density_tertiles
)

save(density_by_condition_results, file = "outputs/exp1/tables/density_by_condition_results.RData")
write.csv(grand_means, "outputs/exp1/tables/density_by_condition_means.csv", row.names = FALSE)
write.csv(bf_summary, "outputs/exp1/tables/density_by_condition_bf.csv", row.names = FALSE)

cat("\nResults saved to outputs/exp1/tables/\n")
