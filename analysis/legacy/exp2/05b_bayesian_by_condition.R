# Bayesian One-Sample T-Tests by Width x Set Size x RM Type - Exp2
# Tests whether width deviation differs from 0 in each condition
# Goal: Show evidence for null (BF01) - no systematic bias

library(tidyverse)
library(BayesFactor)
library(patchwork)

source("analysis/functions/themes.R")

# Load data
df <- read_csv("data/exp2/processed.csv", show_col_types = FALSE)

# Filter to RM and Non-RM conditions
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = ifelse(number_deviation == -1, "RM", "Non-RM"),
    rm_type = factor(rm_type, levels = c("Non-RM", "RM"))
  )

# Define condition levels
widths <- c(0.25, 0.4, 0.55)
setsizes <- c(3, 4, 5)
rm_types <- c("Non-RM", "RM")

# Run Bayesian t-tests for each condition
results_list <- list()

for (w in widths) {
  for (s in setsizes) {
    for (r in rm_types) {

      # Filter to specific condition
      df_cell <- df_analysis %>%
        filter(correct_width == w, correct_num == s, rm_type == r)

      # Calculate subject means
      subject_means <- df_cell %>%
        group_by(subID) %>%
        summarise(mean_wd = mean(width_deviation, na.rm = TRUE), .groups = "drop") %>%
        pull(mean_wd)

      n_subjects <- length(subject_means)

      # Run Bayesian one-sample t-test
      if (n_subjects >= 3 && sd(subject_means) > 0) {
        bf <- ttestBF(x = subject_means, mu = 0)
        bf_10 <- exp(bf@bayesFactor$bf)
        bf_01 <- 1 / bf_10

        # Get posterior samples
        posterior_samples <- posterior(bf, iterations = 10000)
        delta_samples <- as.numeric(posterior_samples[, "delta"])

        median_delta <- median(delta_samples)
        ci_lower <- quantile(delta_samples, 0.025)
        ci_upper <- quantile(delta_samples, 0.975)

      } else {
        bf_01 <- NA
        median_delta <- NA
        ci_lower <- NA
        ci_upper <- NA
      }

      results_list[[length(results_list) + 1]] <- tibble(
        width = w,
        setsize = s,
        rm_type = r,
        n = n_subjects,
        bf_01 = bf_01,
        median_delta = median_delta,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      )
    }
  }
}

results <- bind_rows(results_list) %>%
  mutate(
    width_label = paste0(width, "°"),
    setsize_label = paste0("Set ", setsize),
    rm_type = factor(rm_type, levels = c("Non-RM", "RM"))
  )

# =============================================================================
# HEATMAP VISUALIZATION
# =============================================================================

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

heatmap_plot <- ggplot(results, aes(x = factor(width), y = factor(setsize), fill = bf_01)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", bf_01)), color = "white", size = 4, fontface = "bold") +
  facet_wrap(~ rm_type) +
  scale_fill_gradient2(
    low = "#DC143C", mid = "#FFA500", high = "#2E8B57",
    midpoint = 1, limits = c(0, 4),
    oob = scales::squish,
    name = expression(BF[0][1])
  ) +
  scale_x_discrete(labels = c("0.25°", "0.4°", "0.55°")) +
  labs(
    title = "Bayesian Evidence for Null (No Width Bias) - Exp2",
    subtitle = expression(BF[0][1] > 3 ~"= moderate evidence for null |"~ BF[0][1] > 10 ~"= strong evidence"),
    x = "True Width",
    y = "Set Size"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )

# =============================================================================
# FOREST PLOT VISUALIZATION
# =============================================================================

results_forest <- results %>%
  mutate(
    condition = paste0(width, "° / Set ", setsize),
    condition = factor(condition, levels = rev(unique(condition)))
  )

forest_plot <- ggplot(results_forest, aes(x = median_delta, y = condition, color = rm_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0.2, linewidth = 0.8
  ) +
  geom_point(size = 3, shape = 16) +
  facet_wrap(~ rm_type, scales = "free_y") +
  scale_color_manual(values = rm_colors, guide = "none") +
  labs(
    title = "Effect Sizes (Cohen's δ) by Condition - Exp2",
    subtitle = "Points = median effect size | Error bars = 95% credible interval",
    x = "Effect Size (δ)",
    y = "Condition (Width / Set Size)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# =============================================================================
# COMBINED FIGURE
# =============================================================================

combined_plot <- heatmap_plot / forest_plot +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 14, face = "bold"))
  ) +
  plot_layout(heights = c(1, 1.2))

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists("outputs/exp2/figures")) dir.create("outputs/exp2/figures", recursive = TRUE)
if (!dir.exists("outputs/exp2/tables")) dir.create("outputs/exp2/tables", recursive = TRUE)

ggsave("outputs/exp2/figures/bayesian_heatmap_by_condition.png",
       heatmap_plot, width = 9, height = 5, dpi = 300, bg = "white")

ggsave("outputs/exp2/figures/bayesian_forest_by_condition.png",
       forest_plot, width = 10, height = 6, dpi = 300, bg = "white")

ggsave("outputs/exp2/figures/bayesian_by_condition_combined.png",
       combined_plot, width = 10, height = 10, dpi = 300, bg = "white")

# Save CSV table
results_export <- results %>%
  select(rm_type, width, setsize, n, bf_01, median_delta, ci_lower, ci_upper) %>%
  arrange(rm_type, width, setsize)

write_csv(results_export, "outputs/exp2/tables/bayesian_by_condition_results.csv")

cat("Exp2 Bayesian analysis by condition complete.\n")
cat("Saved: outputs/exp2/figures/bayesian_heatmap_by_condition.png\n")
cat("Saved: outputs/exp2/figures/bayesian_forest_by_condition.png\n")
cat("Saved: outputs/exp2/figures/bayesian_by_condition_combined.png\n")
cat("Saved: outputs/exp2/tables/bayesian_by_condition_results.csv\n")
