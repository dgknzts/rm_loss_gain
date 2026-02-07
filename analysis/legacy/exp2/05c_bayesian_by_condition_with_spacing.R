# Bayesian One-Sample T-Tests by Width x Set Size x Spacing x RM Type - Exp2
# Tests whether width deviation differs from 0 in each condition
# Goal: Show evidence for null (BF01) - no systematic bias
# Note: Spacing categories derived from correct_space using tertile splits

library(tidyverse)
library(BayesFactor)
library(patchwork)

source("analysis/functions/themes.R")

# Load data
df <- read_csv("data/exp2/processed.csv", show_col_types = FALSE)

# Create spacing categories using FIXED cutoffs (same as exp1 preprocessing)
# Small: <= 0.8, Middle: 0.8-1.0, Large: > 1.0

# Filter to RM and Non-RM conditions and create spacing categories
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = ifelse(number_deviation == -1, "RM", "Non-RM"),
    rm_type = factor(rm_type, levels = c("Non-RM", "RM")),
    spacing = case_when(
      correct_space <= 0.8 ~ "Small",
      correct_space > 0.8 & correct_space <= 1 ~ "Middle",
      correct_space > 1 ~ "Large"
    ),
    spacing = factor(spacing, levels = c("Small", "Middle", "Large"))
  )

# Define condition levels
widths <- c(0.25, 0.4, 0.55)
setsizes <- c(3, 4, 5)
spacings <- c("Small", "Middle", "Large")
rm_types <- c("Non-RM", "RM")

# Run Bayesian t-tests for each condition (54 tests total)
results_list <- list()

for (w in widths) {
  for (s in setsizes) {
    for (sp in spacings) {
      for (r in rm_types) {

        # Filter to specific condition
        df_cell <- df_analysis %>%
          filter(correct_width == w, correct_num == s, spacing == sp, rm_type == r)

        # Calculate subject means
        subject_means <- df_cell %>%
          group_by(subID) %>%
          summarise(mean_wd = mean(width_deviation, na.rm = TRUE), .groups = "drop") %>%
          pull(mean_wd)

        n_subjects <- length(subject_means)

        # Run Bayesian one-sample t-test
        if (n_subjects >= 3 && !is.na(sd(subject_means)) && sd(subject_means) > 0) {
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
          spacing = sp,
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
}

results <- bind_rows(results_list) %>%
  mutate(
    width_label = paste0(width, "°"),
    setsize_label = paste0("Set ", setsize),
    rm_type = factor(rm_type, levels = c("Non-RM", "RM")),
    spacing = factor(spacing, levels = c("Small", "Middle", "Large"))
  )

# =============================================================================
# HEATMAP VISUALIZATION - Faceted by Spacing and RM Type
# =============================================================================

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Cap extreme BF values for visualization
results_viz <- results %>%
  mutate(bf_01_capped = pmin(bf_01, 4, na.rm = TRUE))

heatmap_plot <- ggplot(results_viz, aes(x = factor(width), y = factor(setsize), fill = bf_01_capped)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = ifelse(is.na(bf_01), "NA", sprintf("%.1f", bf_01))),
            color = "white", size = 3, fontface = "bold") +
  facet_grid(spacing ~ rm_type) +
  scale_fill_gradient2(
    low = "#DC143C", mid = "#FFA500", high = "#2E8B57",
    midpoint = 1, na.value = "grey50",
    name = expression(BF[0][1]),
    limits = c(0, 4),
    oob = scales::squish
  ) +
  scale_x_discrete(labels = c("0.25°", "0.4°", "0.55°")) +
  labs(
    title = "Bayesian Evidence for Null (No Width Bias) - Exp2",
    subtitle = expression(BF[0][1] > 3 ~"= moderate |"~ BF[0][1] > 10 ~"= strong evidence for null"),
    x = "True Width",
    y = "Set Size"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 9)
  )

# =============================================================================
# FOREST PLOT VISUALIZATION
# =============================================================================

results_forest <- results %>%
  mutate(
    condition = paste0(width, "° / Set ", setsize, " / ", spacing),
    condition = factor(condition, levels = rev(unique(condition)))
  )

forest_plot <- ggplot(results_forest, aes(x = median_delta, y = condition, color = rm_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0.3, linewidth = 0.6, position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 2, shape = 16, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = rm_colors, name = "Condition") +
  labs(
    title = "Effect Sizes (Cohen's δ) by Condition - Exp2",
    subtitle = "Points = median | Error bars = 95% CI",
    x = "Effect Size (δ)",
    y = "Width / Set Size / Spacing"
  ) +
  theme_scientific(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8)
  )

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

if (!dir.exists("outputs/exp2/figures")) dir.create("outputs/exp2/figures", recursive = TRUE)
if (!dir.exists("outputs/exp2/tables")) dir.create("outputs/exp2/tables", recursive = TRUE)

ggsave("outputs/exp2/figures/bayesian_heatmap_by_condition_with_spacing.png",
       heatmap_plot, width = 8, height = 8, dpi = 300, bg = "white")

ggsave("outputs/exp2/figures/bayesian_forest_by_condition_with_spacing.png",
       forest_plot, width = 10, height = 12, dpi = 300, bg = "white")

# Save CSV table
results_export <- results %>%
  select(rm_type, spacing, width, setsize, n, bf_01, median_delta, ci_lower, ci_upper) %>%
  arrange(rm_type, spacing, width, setsize)

write_csv(results_export, "outputs/exp2/tables/bayesian_by_condition_with_spacing_results.csv")

cat("Exp2 Bayesian analysis by condition (with spacing) complete.\n")
cat("Spacing categories (same fixed cutoffs as exp1):\n")
cat("  Small: <= 0.8\n")
cat("  Middle: 0.8 - 1.0\n")
cat("  Large: > 1.0\n")
cat("Total conditions tested: 54 (3 widths x 3 setsizes x 3 spacings x 2 RM types)\n")
cat("Saved: outputs/exp2/figures/bayesian_heatmap_by_condition_with_spacing.png\n")
cat("Saved: outputs/exp2/figures/bayesian_forest_by_condition_with_spacing.png\n")
cat("Saved: outputs/exp2/tables/bayesian_by_condition_with_spacing_results.csv\n")
