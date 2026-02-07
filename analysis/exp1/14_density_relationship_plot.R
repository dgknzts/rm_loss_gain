# Reported vs Actual Density Relationship Plots - Experiment 1
# 1. Scatter plot faceted by set size
# 2. Individual participant slope comparison (RM vs Non-RM)

library(tidyverse)
library(BayesFactor)

source("analysis/functions/themes.R")

rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

df <- read.csv("data/exp1/processed.csv")

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    correct_num = factor(correct_num)
  ) %>%
  drop_na(actual_width_density, response_width_density)

cat("N trials:", nrow(analysis_data), "\n")
cat("N subjects:", length(unique(analysis_data$subID)), "\n")

# =============================================================================
# PLOT 1: BY SET SIZE (improved)
# =============================================================================

p1 <- ggplot(analysis_data, aes(x = actual_width_density, y = response_width_density,
                                 color = rm_type)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_point(alpha = 0.06, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5, alpha = 1) +
  facet_wrap(~ correct_num, labeller = labeller(correct_num = function(x) paste("Set Size", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(x = "Actual Density", y = "Reported Density") +
  theme_scientific() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

ggsave("outputs/exp1/figures/density_relationship_by_setsize.png", p1,
       width = 10, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_relationship_by_setsize.svg", p1,
       width = 10, height = 4, bg = "white")
cat("Saved: density_relationship_by_setsize.png\n")

# =============================================================================
# PLOT 2: INDIVIDUAL PARTICIPANT SLOPE COMPARISON
# =============================================================================

# Calculate slopes for each participant × RM condition
slope_data <- analysis_data %>%
  group_by(subID, rm_type) %>%
  filter(n() >= 3) %>%
  summarise(
    slope = coef(lm(response_width_density ~ actual_width_density))[2],
    intercept = coef(lm(response_width_density ~ actual_width_density))[1],
    n_trials = n(),
    .groups = "drop"
  ) %>%
  drop_na(slope)

# Summary statistics
slope_summary <- slope_data %>%
  group_by(rm_type) %>%
  summarise(
    n = n(),
    mean_slope = mean(slope),
    sd_slope = sd(slope),
    se_slope = sd_slope / sqrt(n),
    ci_lower = mean_slope - qt(0.975, n-1) * se_slope,
    ci_upper = mean_slope + qt(0.975, n-1) * se_slope,
    .groups = "drop"
  )

cat("\n=== SLOPE SUMMARY ===\n")
print(slope_summary)

# Paired data for comparison
paired_slopes <- slope_data %>%
  select(subID, rm_type, slope) %>%
  pivot_wider(names_from = rm_type, values_from = slope) %>%
  mutate(difference = RM - `Non-RM`)

cat("\n=== PAIRED T-TEST ON SLOPES ===\n")
t_result <- t.test(paired_slopes$RM, paired_slopes$`Non-RM`, paired = TRUE)
print(t_result)

# Bayesian paired t-test
bf_result <- ttestBF(x = paired_slopes$difference, mu = 0)
bf_10 <- exp(bf_result@bayesFactor$bf)
bf_01 <- 1 / bf_10
cat(sprintf("\nBayesian: BF10 = %.2f, BF01 = %.2f\n", bf_10, bf_01))

# Determine y-axis breaks that include 1
y_range <- range(c(slope_data$slope, slope_summary$ci_lower, slope_summary$ci_upper), na.rm = TRUE)
y_breaks <- sort(unique(c(seq(round(y_range[1], 1) - 0.1, round(y_range[2], 1) + 0.1, by = 0.2), 1)))

# Format BF label for overall comparison
bf_label_overall <- if (bf_01 > 1) {
  sprintf("BF01 = %.2f", bf_01)
} else {
  sprintf("BF10 = %.2f", bf_10)
}

# Create slope comparison plot (bar plot with individual dots)
p2 <- ggplot(slope_summary, aes(x = rm_type, y = mean_slope, fill = rm_type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.8, color = "black") +
  geom_point(data = slope_data, aes(x = rm_type, y = slope),
             position = position_jitter(width = 0.1, seed = 42),
             size = 2, alpha = 0.6, color = "black") +
  geom_line(data = slope_data, aes(x = rm_type, y = slope, group = subID),
            alpha = 0.2, color = "grey40") +
  scale_fill_manual(values = rm_colors, name = NULL) +
  scale_y_continuous(breaks = y_breaks) +
  labs(
    x = bf_label_overall,
    y = "Slope (Reported ~ Actual Density)"
  ) +
  theme_scientific() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 11, face = "italic", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggsave("outputs/exp1/figures/density_slope_comparison.png", p2,
       width = 4, height = 5, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_slope_comparison.svg", p2,
       width = 4, height = 5, bg = "white")
cat("Saved: density_slope_comparison.png\n")

# =============================================================================
# PLOT 3: SLOPE COMPARISON BY SET SIZE
# =============================================================================

# Calculate slopes for each participant × RM × set size
slope_data_setsize <- analysis_data %>%
  group_by(subID, rm_type, correct_num) %>%
  filter(n() >= 3) %>%  # Need at least 3 points for regression
  summarise(
    slope = coef(lm(response_width_density ~ actual_width_density))[2],
    intercept = coef(lm(response_width_density ~ actual_width_density))[1],
    n_trials = n(),
    .groups = "drop"
  ) %>%
  drop_na(slope)

# Summary statistics by set size
slope_summary_setsize <- slope_data_setsize %>%
  group_by(rm_type, correct_num) %>%
  summarise(
    n = n(),
    mean_slope = mean(slope),
    sd_slope = sd(slope),
    se_slope = sd_slope / sqrt(n),
    ci_lower = mean_slope - qt(0.975, n-1) * se_slope,
    ci_upper = mean_slope + qt(0.975, n-1) * se_slope,
    .groups = "drop"
  )

cat("\n=== SLOPE SUMMARY BY SET SIZE ===\n")
print(slope_summary_setsize)

# Calculate BF for each set size
bf_by_setsize <- list()
for (ss in c("3", "4", "5")) {
  paired_ss <- slope_data_setsize %>%
    filter(correct_num == ss) %>%
    select(subID, rm_type, slope) %>%
    pivot_wider(names_from = rm_type, values_from = slope) %>%
    mutate(difference = RM - `Non-RM`) %>%
    drop_na(difference)

  bf_ss <- ttestBF(x = paired_ss$difference, mu = 0)
  bf_10_ss <- exp(bf_ss@bayesFactor$bf)
  bf_01_ss <- 1 / bf_10_ss

  bf_by_setsize[[ss]] <- list(
    bf_10 = bf_10_ss,
    bf_01 = bf_01_ss,
    n = nrow(paired_ss)
  )

  cat(sprintf("Set Size %s: BF10 = %.2f, BF01 = %.2f\n", ss, bf_10_ss, bf_01_ss))
}

# Create BF labels for each set size
bf_labels_setsize <- sapply(c("3", "4", "5"), function(ss) {
  bf01 <- bf_by_setsize[[ss]]$bf_01
  bf10 <- bf_by_setsize[[ss]]$bf_10
  if (bf01 > 1) {
    sprintf("BF01 = %.2f", bf01)
  } else {
    sprintf("BF10 = %.2f", bf10)
  }
})

# Create BF annotation data for facets (below x-axis)
bf_annotations <- tibble(
  correct_num = factor(c("3", "4", "5")),
  label = bf_labels_setsize,
  x = 1.5,
  y = -Inf
)

# Determine y-axis range for set size plot
y_range_ss <- range(c(slope_data_setsize$slope,
                      slope_summary_setsize$ci_lower,
                      slope_summary_setsize$ci_upper), na.rm = TRUE)
y_breaks_ss <- sort(unique(c(seq(round(y_range_ss[1], 1) - 0.1,
                                  round(y_range_ss[2], 1) + 0.1, by = 0.2), 1)))

# Create faceted slope comparison plot by set size
p3 <- ggplot(slope_summary_setsize, aes(x = rm_type, y = mean_slope, fill = rm_type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.8, color = "black") +
  geom_point(data = slope_data_setsize, aes(x = rm_type, y = slope),
             position = position_jitter(width = 0.1, seed = 42),
             size = 1.5, alpha = 0.5, color = "black") +
  geom_line(data = slope_data_setsize, aes(x = rm_type, y = slope, group = subID),
            alpha = 0.15, color = "grey40") +
  geom_text(data = bf_annotations, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 3.5, fontface = "italic",
            vjust = 5) +
  facet_wrap(~ correct_num, labeller = labeller(correct_num = function(x) paste("Set Size", x))) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.2), limits = c(0, 1.2)) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Slope (Reported ~ Actual Density)"
  ) +
  theme_scientific() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    plot.margin = margin(t = 5, r = 10, b = 35, l = 10)
  )

ggsave("outputs/exp1/figures/density_slope_by_setsize.png", p3,
       width = 9, height = 4.5, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_slope_by_setsize.svg", p3,
       width = 9, height = 4.5, bg = "white")
cat("Saved: density_slope_by_setsize.png\n")

# Save slope data
write.csv(slope_data, "outputs/exp1/tables/density_slopes_by_participant.csv", row.names = FALSE)
write.csv(slope_summary, "outputs/exp1/tables/density_slopes_summary.csv", row.names = FALSE)
write.csv(slope_data_setsize, "outputs/exp1/tables/density_slopes_by_setsize.csv", row.names = FALSE)
write.csv(slope_summary_setsize, "outputs/exp1/tables/density_slopes_summary_by_setsize.csv", row.names = FALSE)

# Save results
density_slope_results <- list(
  slope_data = slope_data,
  slope_summary = slope_summary,
  paired_slopes = paired_slopes,
  t_test = t_result,
  bf_10 = bf_10,
  bf_01 = bf_01,
  slope_data_setsize = slope_data_setsize,
  slope_summary_setsize = slope_summary_setsize,
  bf_by_setsize = bf_by_setsize
)
save(density_slope_results, file = "outputs/exp1/tables/density_slope_results.RData")

cat("\nSlope data saved to outputs/exp1/tables/\n")
cat("\nDone!\n")
