# Bayesian Analysis of Width Density Deviation
# Tests whether RM preserves density perception (RM = NoRM ≈ 0)

library(tidyverse)
library(BayesFactor)

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv("data/exp1/processed.csv")

df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(ifelse(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    subID = factor(subID),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_density_deviation)

cat(sprintf("Analysis data: %d trials from %d participants\n\n",
            nrow(df_analysis), length(unique(df_analysis$subID))))

# =============================================================================
# SECTION 1: OVERALL BAYESIAN ANALYSIS
# =============================================================================

cat("=" %>% strrep(60), "\n")
cat("OVERALL DENSITY DEVIATION ANALYSIS\n")
cat("=" %>% strrep(60), "\n\n")

# Calculate participant means for overall analysis
subject_means_overall <- df_analysis %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
            .groups = "drop")

rm_means <- subject_means_overall %>%
  filter(rm_type == "RM") %>%
  pull(mean_density_dev)

norm_means <- subject_means_overall %>%
  filter(rm_type == "Non-RM") %>%
  pull(mean_density_dev)

# --- One-sample t-tests (testing against 0) ---
bf_rm <- ttestBF(x = rm_means, mu = 0)
bf_norm <- ttestBF(x = norm_means, mu = 0)

# --- Two-sample t-test (RM vs NoRM comparison) ---
bf_comparison <- ttestBF(x = rm_means, y = norm_means)

# Extract posterior samples
posterior_rm <- posterior(bf_rm, iterations = 10000)
posterior_norm <- posterior(bf_norm, iterations = 10000)
posterior_comp <- posterior(bf_comparison, iterations = 10000)

delta_rm <- as.numeric(posterior_rm[, "delta"])
delta_norm <- as.numeric(posterior_norm[, "delta"])
delta_comp <- as.numeric(posterior_comp[, "delta"])

# Calculate statistics
overall_results <- tibble(
  test = c("RM = 0", "Non-RM = 0", "RM vs Non-RM"),
  n = c(length(rm_means), length(norm_means), length(rm_means)),
  mean = c(mean(rm_means), mean(norm_means), mean(rm_means) - mean(norm_means)),
  se = c(sd(rm_means)/sqrt(length(rm_means)),
         sd(norm_means)/sqrt(length(norm_means)),
         sqrt(var(rm_means)/length(rm_means) + var(norm_means)/length(norm_means))),
  median_delta = c(median(delta_rm), median(delta_norm), median(delta_comp)),
  ci_lower = c(quantile(delta_rm, 0.025), quantile(delta_norm, 0.025), quantile(delta_comp, 0.025)),
  ci_upper = c(quantile(delta_rm, 0.975), quantile(delta_norm, 0.975), quantile(delta_comp, 0.975)),
  bf10 = c(exp(bf_rm@bayesFactor$bf), exp(bf_norm@bayesFactor$bf), exp(bf_comparison@bayesFactor$bf)),
  bf01 = 1 / bf10
)

# Calculate density at x=0 for plotting
rm_density <- density(delta_rm, adjust = 1.2)
norm_density <- density(delta_norm, adjust = 1.2)
comp_density <- density(delta_comp, adjust = 1.2)

rm_density_at_zero <- rm_density$y[which.min(abs(rm_density$x - 0))]
norm_density_at_zero <- norm_density$y[which.min(abs(norm_density$x - 0))]
comp_density_at_zero <- comp_density$y[which.min(abs(comp_density$x - 0))]
prior_density_at_zero <- dcauchy(0, location = 0, scale = 0.707)

# Print overall results
cat("One-Sample Tests (H0: mean = 0):\n\n")
cat("RM Condition:\n")
cat(sprintf("  N = %d subjects\n", length(rm_means)))
cat(sprintf("  Mean density deviation: %.4f (SE = %.4f)\n", mean(rm_means), sd(rm_means)/sqrt(length(rm_means))))
cat(sprintf("  Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            median(delta_rm), quantile(delta_rm, 0.025), quantile(delta_rm, 0.975)))
cat(sprintf("  BF01 = %.4f %s\n\n",
            1/exp(bf_rm@bayesFactor$bf),
            ifelse(1/exp(bf_rm@bayesFactor$bf) > 3, "(evidence for null)",
                   ifelse(1/exp(bf_rm@bayesFactor$bf) < 1/3, "(evidence against null)", "(inconclusive)"))))

cat("Non-RM Condition:\n")
cat(sprintf("  N = %d subjects\n", length(norm_means)))
cat(sprintf("  Mean density deviation: %.4f (SE = %.4f)\n", mean(norm_means), sd(norm_means)/sqrt(length(norm_means))))
cat(sprintf("  Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            median(delta_norm), quantile(delta_norm, 0.025), quantile(delta_norm, 0.975)))
cat(sprintf("  BF01 = %.4f %s\n\n",
            1/exp(bf_norm@bayesFactor$bf),
            ifelse(1/exp(bf_norm@bayesFactor$bf) > 3, "(evidence for null)",
                   ifelse(1/exp(bf_norm@bayesFactor$bf) < 1/3, "(evidence against null)", "(inconclusive)"))))

cat("Two-Sample Test (H0: RM = Non-RM):\n")
cat(sprintf("  Mean difference: %.4f\n", mean(rm_means) - mean(norm_means)))
cat(sprintf("  Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n",
            median(delta_comp), quantile(delta_comp, 0.025), quantile(delta_comp, 0.975)))
cat(sprintf("  BF01 = %.4f %s\n\n",
            1/exp(bf_comparison@bayesFactor$bf),
            ifelse(1/exp(bf_comparison@bayesFactor$bf) > 3, "(evidence for equivalence)",
                   ifelse(1/exp(bf_comparison@bayesFactor$bf) < 1/3, "(evidence for difference)", "(inconclusive)"))))

# =============================================================================
# SECTION 2: BY SET SIZE ANALYSIS
# =============================================================================

cat("=" %>% strrep(60), "\n")
cat("ANALYSIS BY SET SIZE\n")
cat("=" %>% strrep(60), "\n\n")

setsize_results <- tibble()

for (s in c(3, 4, 5)) {
  subject_means_s <- df_analysis %>%
    filter(correct_num == s) %>%
    group_by(subID, rm_type) %>%
    summarise(mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
              .groups = "drop")

  rm_means_s <- subject_means_s %>% filter(rm_type == "RM") %>% pull(mean_density_dev)
  norm_means_s <- subject_means_s %>% filter(rm_type == "Non-RM") %>% pull(mean_density_dev)

  if (length(rm_means_s) >= 3 && sd(rm_means_s) > 0) {
    bf_rm_s <- ttestBF(x = rm_means_s, mu = 0)
    post_rm_s <- posterior(bf_rm_s, iterations = 10000)
    delta_rm_s <- as.numeric(post_rm_s[, "delta"])

    setsize_results <- bind_rows(setsize_results, tibble(
      setsize = s,
      test = "RM = 0",
      n = length(rm_means_s),
      mean = mean(rm_means_s),
      median_delta = median(delta_rm_s),
      ci_lower = quantile(delta_rm_s, 0.025),
      ci_upper = quantile(delta_rm_s, 0.975),
      bf01 = 1 / exp(bf_rm_s@bayesFactor$bf)
    ))
  }

  if (length(norm_means_s) >= 3 && sd(norm_means_s) > 0) {
    bf_norm_s <- ttestBF(x = norm_means_s, mu = 0)
    post_norm_s <- posterior(bf_norm_s, iterations = 10000)
    delta_norm_s <- as.numeric(post_norm_s[, "delta"])

    setsize_results <- bind_rows(setsize_results, tibble(
      setsize = s,
      test = "Non-RM = 0",
      n = length(norm_means_s),
      mean = mean(norm_means_s),
      median_delta = median(delta_norm_s),
      ci_lower = quantile(delta_norm_s, 0.025),
      ci_upper = quantile(delta_norm_s, 0.975),
      bf01 = 1 / exp(bf_norm_s@bayesFactor$bf)
    ))
  }

  if (length(rm_means_s) >= 3 && length(norm_means_s) >= 3) {
    bf_comp_s <- ttestBF(x = rm_means_s, y = norm_means_s)
    post_comp_s <- posterior(bf_comp_s, iterations = 10000)
    delta_comp_s <- as.numeric(post_comp_s[, "delta"])

    setsize_results <- bind_rows(setsize_results, tibble(
      setsize = s,
      test = "RM vs Non-RM",
      n = length(rm_means_s),
      mean = mean(rm_means_s) - mean(norm_means_s),
      median_delta = median(delta_comp_s),
      ci_lower = quantile(delta_comp_s, 0.025),
      ci_upper = quantile(delta_comp_s, 0.975),
      bf01 = 1 / exp(bf_comp_s@bayesFactor$bf)
    ))
  }

  cat(sprintf("Set Size %d:\n", s))
  cat(sprintf("  RM: M = %.4f, BF01 = %.3f\n", mean(rm_means_s),
              setsize_results %>% filter(setsize == s, test == "RM = 0") %>% pull(bf01)))
  cat(sprintf("  Non-RM: M = %.4f, BF01 = %.3f\n", mean(norm_means_s),
              setsize_results %>% filter(setsize == s, test == "Non-RM = 0") %>% pull(bf01)))
  cat(sprintf("  RM vs Non-RM: BF01 = %.3f\n\n",
              setsize_results %>% filter(setsize == s, test == "RM vs Non-RM") %>% pull(bf01)))
}

# =============================================================================
# SECTION 3: BY WIDTH x SET SIZE ANALYSIS
# =============================================================================

cat("=" %>% strrep(60), "\n")
cat("ANALYSIS BY WIDTH x SET SIZE\n")
cat("=" %>% strrep(60), "\n\n")

widths <- c(0.25, 0.4, 0.55)
setsizes <- c(3, 4, 5)
width_setsize_results <- tibble()

for (w in widths) {
  for (s in setsizes) {
    subject_means_ws <- df_analysis %>%
      filter(correct_width == w, correct_num == s) %>%
      group_by(subID, rm_type) %>%
      summarise(mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
                .groups = "drop")

    rm_means_ws <- subject_means_ws %>% filter(rm_type == "RM") %>% pull(mean_density_dev)
    norm_means_ws <- subject_means_ws %>% filter(rm_type == "Non-RM") %>% pull(mean_density_dev)

    # RM = 0 test
    if (length(rm_means_ws) >= 3 && sd(rm_means_ws) > 0) {
      bf_rm_ws <- ttestBF(x = rm_means_ws, mu = 0)
      post_rm_ws <- posterior(bf_rm_ws, iterations = 10000)
      delta_rm_ws <- as.numeric(post_rm_ws[, "delta"])

      width_setsize_results <- bind_rows(width_setsize_results, tibble(
        width = w,
        setsize = s,
        test = "RM = 0",
        n = length(rm_means_ws),
        mean = mean(rm_means_ws),
        median_delta = median(delta_rm_ws),
        ci_lower = quantile(delta_rm_ws, 0.025),
        ci_upper = quantile(delta_rm_ws, 0.975),
        bf01 = 1 / exp(bf_rm_ws@bayesFactor$bf)
      ))
    }

    # Non-RM = 0 test
    if (length(norm_means_ws) >= 3 && sd(norm_means_ws) > 0) {
      bf_norm_ws <- ttestBF(x = norm_means_ws, mu = 0)
      post_norm_ws <- posterior(bf_norm_ws, iterations = 10000)
      delta_norm_ws <- as.numeric(post_norm_ws[, "delta"])

      width_setsize_results <- bind_rows(width_setsize_results, tibble(
        width = w,
        setsize = s,
        test = "Non-RM = 0",
        n = length(norm_means_ws),
        mean = mean(norm_means_ws),
        median_delta = median(delta_norm_ws),
        ci_lower = quantile(delta_norm_ws, 0.025),
        ci_upper = quantile(delta_norm_ws, 0.975),
        bf01 = 1 / exp(bf_norm_ws@bayesFactor$bf)
      ))
    }

    # RM vs Non-RM test
    if (length(rm_means_ws) >= 3 && length(norm_means_ws) >= 3) {
      bf_comp_ws <- ttestBF(x = rm_means_ws, y = norm_means_ws)
      post_comp_ws <- posterior(bf_comp_ws, iterations = 10000)
      delta_comp_ws <- as.numeric(post_comp_ws[, "delta"])

      width_setsize_results <- bind_rows(width_setsize_results, tibble(
        width = w,
        setsize = s,
        test = "RM vs Non-RM",
        n = length(rm_means_ws),
        mean = mean(rm_means_ws) - mean(norm_means_ws),
        median_delta = median(delta_comp_ws),
        ci_lower = quantile(delta_comp_ws, 0.025),
        ci_upper = quantile(delta_comp_ws, 0.975),
        bf01 = 1 / exp(bf_comp_ws@bayesFactor$bf)
      ))
    }
  }
}

# Print width x setsize summary
cat("BF01 Summary (evidence for null):\n\n")
for (test_type in c("RM = 0", "Non-RM = 0", "RM vs Non-RM")) {
  cat(sprintf("%s:\n", test_type))
  for (w in widths) {
    vals <- width_setsize_results %>%
      filter(test == test_type, width == w) %>%
      arrange(setsize) %>%
      pull(bf01)
    cat(sprintf("  Width %.2f: Set3=%.2f, Set4=%.2f, Set5=%.2f\n", w, vals[1], vals[2], vals[3]))
  }
  cat("\n")
}

# =============================================================================
# SECTION 4: ROBUSTNESS ANALYSIS (Overall)
# =============================================================================

cat("=" %>% strrep(60), "\n")
cat("ROBUSTNESS ANALYSIS\n")
cat("=" %>% strrep(60), "\n\n")

continuous_scales <- c(
  seq(0.01, 0.1, by = 0.01),
  seq(0.1, 1.5, by = 0.01)
)

robustness_data <- tibble()

for (scale in continuous_scales) {
  bf_rm_rob <- ttestBF(x = rm_means, mu = 0, rscale = scale)
  bf_norm_rob <- ttestBF(x = norm_means, mu = 0, rscale = scale)
  bf_comp_rob <- ttestBF(x = rm_means, y = norm_means, rscale = scale)

  robustness_data <- bind_rows(
    robustness_data,
    tibble(
      test = c("RM = 0", "Non-RM = 0", "RM vs Non-RM"),
      prior_scale = scale,
      bf01 = c(1 / exp(bf_rm_rob@bayesFactor$bf),
               1 / exp(bf_norm_rob@bayesFactor$bf),
               1 / exp(bf_comp_rob@bayesFactor$bf))
    )
  )
}

# Find critical scales
rm_rob_data <- robustness_data %>% filter(test == "RM = 0")
norm_rob_data <- robustness_data %>% filter(test == "Non-RM = 0")
comp_rob_data <- robustness_data %>% filter(test == "RM vs Non-RM")

rm_critical_idx <- which.min(abs(rm_rob_data$bf01 - 3))
norm_critical_idx <- which.min(abs(norm_rob_data$bf01 - 3))
comp_critical_idx <- which.min(abs(comp_rob_data$bf01 - 3))

cat(sprintf("RM = 0: Default BF01 = %.3f, reaches BF01 = 3 at r = %.3f\n",
            rm_rob_data$bf01[rm_rob_data$prior_scale == 0.71],
            rm_rob_data$prior_scale[rm_critical_idx]))
cat(sprintf("Non-RM = 0: Default BF01 = %.3f, reaches BF01 = 3 at r = %.3f\n",
            norm_rob_data$bf01[norm_rob_data$prior_scale == 0.71],
            norm_rob_data$prior_scale[norm_critical_idx]))
cat(sprintf("RM vs Non-RM: Default BF01 = %.3f, reaches BF01 = 3 at r = %.3f\n",
            comp_rob_data$bf01[comp_rob_data$prior_scale == 0.71],
            comp_rob_data$prior_scale[comp_critical_idx]))

# =============================================================================
# SECTION 5: EXPORT RESULTS
# =============================================================================

if (!dir.exists('outputs/exp1/tables')) {
  dir.create('outputs/exp1/tables', recursive = TRUE)
}

# Export CSV tables
write.csv(overall_results, 'outputs/exp1/tables/density_bayesian_overall.csv', row.names = FALSE)
write.csv(setsize_results, 'outputs/exp1/tables/density_bayesian_by_setsize.csv', row.names = FALSE)
write.csv(width_setsize_results, 'outputs/exp1/tables/density_bayesian_by_width_setsize.csv', row.names = FALSE)

# Export comprehensive results for plotting
density_bayesian_results <- list(
  # Overall data
  rm_means = rm_means,
  norm_means = norm_means,
  delta_rm = delta_rm,
  delta_norm = delta_norm,
  delta_comp = delta_comp,

  # Summary statistics
  overall_results = overall_results,

  # Density at zero for plotting
  rm_density_at_zero = rm_density_at_zero,
  norm_density_at_zero = norm_density_at_zero,
  comp_density_at_zero = comp_density_at_zero,
  prior_density_at_zero = prior_density_at_zero,

  # Conditional results
  setsize_results = setsize_results,
  width_setsize_results = width_setsize_results,

  # Robustness data
  robustness = list(
    data = robustness_data,
    rm_critical_scale = rm_rob_data$prior_scale[rm_critical_idx],
    norm_critical_scale = norm_rob_data$prior_scale[norm_critical_idx],
    comp_critical_scale = comp_rob_data$prior_scale[comp_critical_idx]
  )
)

save(density_bayesian_results, file = "outputs/exp1/tables/density_bayesian_results.RData")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("  - density_bayesian_overall.csv\n")
cat("  - density_bayesian_by_setsize.csv\n")
cat("  - density_bayesian_by_width_setsize.csv\n")
cat("  - density_bayesian_results.RData\n")
