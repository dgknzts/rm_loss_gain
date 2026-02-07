# Sequential Bayesian One-Sample T-Test Analysis - Exp2
# Monitors BF trajectory as participants accumulate

library(tidyverse)
library(BayesFactor)
library(ggplot2)

source("analysis/functions/themes.R")

# Load processed data
df <- read.csv("data/exp2/processed.csv")

# Prepare subject-level means (relative width deviation)
subject_means <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(condition = ifelse(number_deviation == -1, "RM", "Non-RM")) %>%
  group_by(subID, condition) %>%
  summarise(mean_width_deviation = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop") %>%
  arrange(subID)

rm_means <- subject_means %>% filter(condition == "RM")
norm_means <- subject_means %>% filter(condition == "Non-RM")

total_n <- nrow(rm_means)
min_n <- 10

# Sequential analysis parameters
bf_upper <- 3    # Evidence for H1
bf_lower <- 1/3  # Evidence for H0

# Sequential BF calculation
sequential_results <- tibble()

for (n in min_n:total_n) {
  # RM condition
  rm_subset <- rm_means$mean_width_deviation[1:n]
  bf_rm <- ttestBF(x = rm_subset, mu = 0)
  bf10_rm <- exp(bf_rm@bayesFactor$bf)
  bf01_rm <- 1 / bf10_rm

  # Non-RM condition
  norm_subset <- norm_means$mean_width_deviation[1:n]
  bf_norm <- ttestBF(x = norm_subset, mu = 0)
  bf10_norm <- exp(bf_norm@bayesFactor$bf)
  bf01_norm <- 1 / bf10_norm

  sequential_results <- bind_rows(
    sequential_results,
    tibble(
      n = n,
      condition = "RM",
      bf10 = bf10_rm,
      bf01 = bf01_rm,
      mean_effect = mean(rm_subset),
      stopped_h1 = bf10_rm > bf_upper,
      stopped_h0 = bf01_rm > bf_upper
    ),
    tibble(
      n = n,
      condition = "Non-RM",
      bf10 = bf10_norm,
      bf01 = bf01_norm,
      mean_effect = mean(norm_subset),
      stopped_h1 = bf10_norm > bf_upper,
      stopped_h0 = bf01_norm > bf_upper
    )
  )
}

# Find first stopping point for each condition
stopping_summary <- sequential_results %>%
  group_by(condition) %>%
  summarise(
    first_stop_h1 = ifelse(any(stopped_h1), min(n[stopped_h1]), NA),
    first_stop_h0 = ifelse(any(stopped_h0), min(n[stopped_h0]), NA),
    final_bf10 = last(bf10),
    final_bf01 = last(bf01),
    .groups = "drop"
  )

# Print results
cat("=== SEQUENTIAL BAYESIAN ANALYSIS RESULTS ===\n\n")
cat(sprintf("Total participants: %d\n", total_n))
cat(sprintf("Minimum N for analysis: %d\n", min_n))
cat(sprintf("Stopping boundaries: BF > %.1f (stop for effect) or BF < %.2f (stop for null)\n\n", bf_upper, bf_lower))

for (cond in c("RM", "Non-RM")) {
  row <- stopping_summary %>% filter(condition == cond)
  cat(sprintf("%s Condition:\n", cond))

  if (!is.na(row$first_stop_h1)) {
    cat(sprintf("  -> Stopped at N = %d for H1 (effect exists)\n", row$first_stop_h1))
  } else if (!is.na(row$first_stop_h0)) {
    cat(sprintf("  -> Stopped at N = %d for H0 (no effect)\n", row$first_stop_h0))
  } else {
    cat(sprintf("  -> No stopping criterion reached\n"))
  }
  cat(sprintf("  Final BF10 = %.3f, BF01 = %.3f\n\n", row$final_bf10, row$final_bf01))
}

# Create trajectory plot
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

trajectory_plot <- sequential_results %>%
  ggplot(aes(x = n, y = bf10, color = condition)) +
  geom_hline(yintercept = bf_upper, linetype = "dashed", color = "grey40", size = 0.8) +
  geom_hline(yintercept = bf_lower, linetype = "dashed", color = "grey40", size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey60", size = 0.6) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  annotate("text", x = min_n + 0.5, y = bf_upper * 1.3, label = "Stop for H1",
           hjust = 0, size = 3.5, color = "grey30") +
  annotate("text", x = min_n + 0.5, y = bf_lower * 0.7, label = "Stop for H0",
           hjust = 0, size = 3.5, color = "grey30") +
  scale_color_manual(values = rm_colors, name = "Condition") +
  scale_y_log10(breaks = c(0.1, 0.33, 1, 3, 10, 30, 100),
                labels = c("0.1", "1/3", "1", "3", "10", "30", "100")) +
  scale_x_continuous(breaks = seq(min_n, total_n, by = 1)) +
  labs(
    title = "Sequential Bayesian Analysis",
    subtitle = sprintf("One-sample t-test (H0: relative width deviation = 0) | Stopping: BF > 3 or < 1/3"),
    x = "Number of Participants",
    y = expression(BF[10])
  ) +
  theme_scientific() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# Save outputs
if (!dir.exists("outputs/exp2/figures")) dir.create("outputs/exp2/figures", recursive = TRUE)
if (!dir.exists("outputs/exp2/tables")) dir.create("outputs/exp2/tables", recursive = TRUE)

ggsave("outputs/exp2/figures/sequential_bayesian_analysis.png", trajectory_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

write.csv(sequential_results, "outputs/exp2/tables/sequential_bf_results.csv", row.names = FALSE)

cat("Outputs saved:\n")
cat("  - outputs/exp2/figures/sequential_bayesian_analysis.png\n")
cat("  - outputs/exp2/tables/sequential_bf_results.csv\n")
