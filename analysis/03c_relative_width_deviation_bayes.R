# Bayesian One-Sample T-Test Analysis
# RM and NoRM conditions vs 0 with combined JASP-style plots

# Load required libraries
library(tidyverse)
library(BayesFactor)
library(ggplot2)
library(patchwork)

# Shared aesthetics for plots
plot_colors <- c("RM" = "#B22222", "NoRM" = "#1F78B4")
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )


# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

# Load processed data
df <- read.csv("data/processed.csv")

# Filter for RM and NoRM conditions (number_deviation = -1 for RM, 0 for NoRM)
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    condition = ifelse(number_deviation == -1, "RM", "NoRM"),
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

# Separate RM and NoRM means
rm_means <- subject_means %>%
  filter(condition == "RM") %>%
  pull(mean_width_deviation)

norm_means <- subject_means %>%
  filter(condition == "NoRM") %>%
  pull(mean_width_deviation)

# =============================================================================
# 2. BAYESIAN ONE-SAMPLE T-TESTS
# =============================================================================

# Test 1: RM condition against 0
bf_rm <- ttestBF(x = rm_means, mu = 0)
print("Bayesian t-test for RM condition:")
print(bf_rm)

# Test 2: NoRM condition against 0
bf_norm <- ttestBF(x = norm_means, mu = 0)
print("\nBayesian t-test for NoRM condition:")
print(bf_norm)

# Extract posterior samples for effect size
posterior_rm <- posterior(bf_rm, iterations = 10000)
posterior_norm <- posterior(bf_norm, iterations = 10000)

# Extract delta (effect size) from posteriors
delta_rm <- as.numeric(posterior_rm[, "delta"])
delta_norm <- as.numeric(posterior_norm[, "delta"])

# =============================================================================
# 3. CALCULATE STATISTICS
# =============================================================================

# RM statistics
median_rm <- median(delta_rm)
ci_rm <- quantile(delta_rm, c(0.025, 0.975))
bf_10_rm <- exp(bf_rm@bayesFactor$bf)
bf_01_rm <- 1 / bf_10_rm
mean_rm <- mean(rm_means)
se_rm <- sd(rm_means) / sqrt(length(rm_means))

# NoRM statistics
median_norm <- median(delta_norm)
ci_norm <- quantile(delta_norm, c(0.025, 0.975))
bf_10_norm <- exp(bf_norm@bayesFactor$bf)
bf_01_norm <- 1 / bf_10_norm
mean_norm <- mean(norm_means)
se_norm <- sd(norm_means) / sqrt(length(norm_means))

# =============================================================================
# 4. CREATE COMBINED JASP-STYLE PLOT
# =============================================================================

# Prepare data for plotting
plot_data <- data.frame(
  delta = c(delta_rm, delta_norm),
  condition = factor(rep(c("RM", "NoRM"), c(length(delta_rm), length(delta_norm))))
)

# Create the combined plot
medians_data <- tibble(
  condition = c("RM", "NoRM"),
  median_value = c(median_rm, median_norm)
)

combined_plot <- ggplot(plot_data, aes(x = delta, color = condition)) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey20",
    linetype = "dashed",
    size = 0.8
  ) +
  geom_density(size = 1.1, adjust = 1.3) +
  geom_vline(
    data = medians_data,
    aes(xintercept = median_value, color = condition),
    linetype = "dotted",
    size = 0.8,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = 0, color = "grey20", size = 0.6) +
  scale_color_manual(values = plot_colors, name = "Posterior") +
  labs(
    title = "Bayesian One-Sample T-Tests: Width Deviation",
    subtitle = "Testing RM and NoRM conditions against mu = 0",
    x = "Effect size (delta)",
    y = "Density"
  ) +
  xlim(-2.2, 1.6) +
  plot_theme +
  theme(legend.position = "right")

print(combined_plot)
ggsave("outputs/figures/bayesian_combined_plot.png", combined_plot, width = 10, height = 6, dpi = 300)

# =============================================================================
# 5. CREATE SUMMARY TABLES
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("BAYESIAN ONE-SAMPLE T-TEST RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# RM Condition Summary
cat("\n--- RM Condition (vs mu = 0) ---\n")
cat(sprintf("N = %d subjects\n", length(rm_means)))
cat(sprintf("Mean width deviation: %.6f (SE = %.6f)\n", mean_rm, se_rm))
cat(sprintf("Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n", median_rm, ci_rm[1], ci_rm[2]))
cat(sprintf("Bayes Factor BF10: %.4f\n", bf_10_rm))
cat(sprintf("Bayes Factor BF01: %.4f\n", bf_01_rm))

if (bf_10_rm > 100) {
  evidence_rm <- "Extreme evidence for H1"
} else if (bf_10_rm > 30) {
  evidence_rm <- "Very strong evidence for H1"
} else if (bf_10_rm > 10) {
  evidence_rm <- "Strong evidence for H1"
} else if (bf_10_rm > 3) {
  evidence_rm <- "Moderate evidence for H1"
} else if (bf_10_rm > 1) {
  evidence_rm <- "Anecdotal evidence for H1"
} else if (bf_01_rm > 3) {
  evidence_rm <- "Moderate evidence for H0"
} else if (bf_01_rm > 1) {
  evidence_rm <- "Anecdotal evidence for H0"
} else {
  evidence_rm <- "No clear evidence"
}
cat("Interpretation:", evidence_rm, "\n")

# NoRM Condition Summary
cat("\n--- NoRM Condition (vs mu = 0) ---\n")
cat(sprintf("N = %d subjects\n", length(norm_means)))
cat(sprintf("Mean width deviation: %.6f (SE = %.6f)\n", mean_norm, se_norm))
cat(sprintf("Effect size (delta): Median = %.3f, 95%% CI [%.3f, %.3f]\n", median_norm, ci_norm[1], ci_norm[2]))
cat(sprintf("Bayes Factor BF10: %.4f\n", bf_10_norm))
cat(sprintf("Bayes Factor BF01: %.4f\n", bf_01_norm))

if (bf_10_norm > 100) {
  evidence_norm <- "Extreme evidence for H1"
} else if (bf_10_norm > 30) {
  evidence_norm <- "Very strong evidence for H1"
} else if (bf_10_norm > 10) {
  evidence_norm <- "Strong evidence for H1"
} else if (bf_10_norm > 3) {
  evidence_norm <- "Moderate evidence for H1"
} else if (bf_10_norm > 1) {
  evidence_norm <- "Anecdotal evidence for H1"
} else if (bf_01_norm > 3) {
  evidence_norm <- "Moderate evidence for H0"
} else if (bf_01_norm > 1) {
  evidence_norm <- "Anecdotal evidence for H0"
} else {
  evidence_norm <- "No clear evidence"
}
cat("Interpretation:", evidence_norm, "\n")


# =============================================================================
# 6. ROBUSTNESS CHECK WITH DIFFERENT PRIORS
# =============================================================================

prior_scales <- c(0.5, 0.707, 1.0, sqrt(2))
prior_names <- c("Medium", "Default (Wide)", "Wide", "Ultrawide")
prior_labels <- sprintf("%s (r = %.3f)", prior_names, prior_scales)

robustness_results <- tibble()

cat("\nRM Condition:\n")
for (i in seq_along(prior_scales)) {
  bf_robust_rm <- ttestBF(x = rm_means, mu = 0, rscale = prior_scales[i])
  bf10_rm_prior <- exp(bf_robust_rm@bayesFactor$bf)
  bf01_rm_prior <- 1 / bf10_rm_prior
  robustness_results <- bind_rows(
    robustness_results,
    tibble(
      condition = "RM",
      prior_label = prior_labels[i],
      prior_scale = prior_scales[i],
      bf01 = bf01_rm_prior
    )
  )
  cat(sprintf("  %s: BF01 = %.2f\n", prior_labels[i], bf01_rm_prior))
}

cat("\nNoRM Condition:\n")
for (i in seq_along(prior_scales)) {
  bf_robust_norm <- ttestBF(x = norm_means, mu = 0, rscale = prior_scales[i])
  bf10_norm_prior <- exp(bf_robust_norm@bayesFactor$bf)
  bf01_norm_prior <- 1 / bf10_norm_prior
  robustness_results <- bind_rows(
    robustness_results,
    tibble(
      condition = "NoRM",
      prior_label = prior_labels[i],
      prior_scale = prior_scales[i],
      bf01 = bf01_norm_prior
    )
  )
  cat(sprintf("  %s: BF01 = %.2f\n", prior_labels[i], bf01_norm_prior))
}

robustness_results <- robustness_results %>%
  mutate(
    prior_label = factor(prior_label, levels = prior_labels),
    condition = factor(condition, levels = c("RM", "NoRM"))
  )

rm_data <- robustness_results %>% filter(condition == "RM")
norm_data <- robustness_results %>% filter(condition == "NoRM")

rm_color <- unname(plot_colors["RM"])
norm_color <- unname(plot_colors["NoRM"])

robustness_plot_rm <- ggplot(rm_data, aes(x = prior_label, y = bf01, group = 1)) +
  geom_hline(yintercept = 1, color = "grey70", linetype = "dashed") +
  geom_point(color = rm_color, size = 3) +
  geom_line(color = rm_color, size = 0.9) +
  labs(
    title = "RM",
    subtitle = "BF01 across prior scales",
    x = "Prior scale",
    y = "BF01"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

robustness_plot_norm <- ggplot(norm_data, aes(x = prior_label, y = bf01, group = 1)) +
  geom_hline(yintercept = 1, color = "grey70", linetype = "dashed") +
  geom_point(color = norm_color, size = 3) +
  geom_line(color = norm_color, size = 0.9) +
  labs(
    title = "NoRM",
    subtitle = "BF01 across prior scales",
    x = "Prior scale",
    y = "BF01"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

robustness_patchwork <- robustness_plot_rm | robustness_plot_norm
robustness_patchwork <- robustness_patchwork +
  plot_annotation(
    title = "Bayes Factor Robustness (BF01)",
    subtitle = "Evidence for the null across Cauchy prior scales"
  )

print(robustness_patchwork)
ggsave("outputs/figures/bf01_prior_robustness.png", robustness_patchwork, width = 12, height = 5, dpi = 300)

cat("\nNote: Consistent evidence across different priors indicates robust results.\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
















# =============================================================================
# 8. CREATE ALTERNATIVE VISUALIZATION (OVERLAPPING DENSITIES)
# =============================================================================

clean_plot <- ggplot(plot_data, aes(x = delta)) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area",
    fill = "grey80",
    alpha = 0.3,
    n = 1000
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey60",
    linetype = "dashed",
    size = 0.8,
    n = 1000
  ) +
  geom_density(aes(fill = condition), alpha = 0.35, color = NA, size = 0.9) +
  geom_vline(xintercept = 0, color = "grey60", size = 0.6) +
  scale_fill_manual(values = plot_colors, name = "Condition") +
  labs(
    title = "Prior and Posterior Distributions",
    subtitle = sprintf("RM: BF10 = %.2f | NoRM: BF10 = %.2f", bf_10_rm, bf_10_norm),
    x = "Effect size (delta)",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(-2, 2)) +
  plot_theme +
  theme(legend.position = "top")

print(clean_plot)
ggsave("outputs/figures/bayesian_clean_plot.png", clean_plot, width = 8, height = 5, dpi = 300)
