# Proper Bland-Altman Plot for Width Density Deviation
# Comparing RM vs Non-RM measurements

library(tidyverse)
library(ggplot2)

source("analysis/functions/themes.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM'))
  ) %>%
  drop_na(width_density_deviation)

# =============================================================================
# STEP 1: CALCULATE DIFFERENCE AND MEAN FOR EACH PARTICIPANT
# =============================================================================

# Get participant means for each condition
participant_means <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_density = mean(width_density_deviation, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = rm_type, values_from = mean_density) %>%
  mutate(
    # Difference: Non-RM minus RM
    difference = `Non-RM` - RM,
    # Mean: Average of the two measurements
    mean_value = (`Non-RM` + RM) / 2
  )

# =============================================================================
# STEP 2: CALCULATE BIAS AND LIMITS OF AGREEMENT
# =============================================================================

# Bias = mean of all differences
bias <- mean(participant_means$difference, na.rm = TRUE)

# Standard deviation of differences
sd_diff <- sd(participant_means$difference, na.rm = TRUE)

# Limits of Agreement (95%)
upper_loa <- bias + 1.96 * sd_diff
lower_loa <- bias - 1.96 * sd_diff

cat("=== Bland-Altman Statistics ===\n")
cat(sprintf("Bias (Mean Difference): %.4f\n", bias))
cat(sprintf("SD of Differences: %.4f\n", sd_diff))
cat(sprintf("Upper Limit of Agreement (+1.96 SD): %.4f\n", upper_loa))
cat(sprintf("Lower Limit of Agreement (-1.96 SD): %.4f\n", lower_loa))
cat(sprintf("Number of participants: %d\n", nrow(participant_means)))

# =============================================================================
# STEP 3: CREATE BLAND-ALTMAN PLOT
# =============================================================================

ba_plot <- participant_means %>%
  ggplot(aes(x = mean_value, y = difference)) +
  # Limits of Agreement shading
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = lower_loa, ymax = upper_loa,
           fill = "lightblue", alpha = 0.3) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.5) +
  # Bias line (mean difference)
  geom_hline(yintercept = bias, linetype = "solid", color = "#E74C3C", linewidth = 1) +
  # Upper and Lower Limits of Agreement
  geom_hline(yintercept = upper_loa, linetype = "dashed", color = "#3498DB", linewidth = 0.8) +
  geom_hline(yintercept = lower_loa, linetype = "dashed", color = "#3498DB", linewidth = 0.8) +
  # Data points
  geom_point(size = 4, alpha = 0.7, color = "black") +
  # Labels for the lines

  annotate("text", x = max(participant_means$mean_value) + 0.005, y = bias,
           label = sprintf("Bias = %.3f", bias),
           hjust = 0, vjust = -0.5, color = "#E74C3C", fontface = "bold", size = 3.5) +
  annotate("text", x = max(participant_means$mean_value) + 0.005, y = upper_loa,
           label = sprintf("+1.96 SD = %.3f", upper_loa),
           hjust = 0, vjust = -0.5, color = "#3498DB", size = 3.5) +
  annotate("text", x = max(participant_means$mean_value) + 0.005, y = lower_loa,
           label = sprintf("-1.96 SD = %.3f", lower_loa),
           hjust = 0, vjust = 1.5, color = "#3498DB", size = 3.5) +
  # Axis labels
  labs(
    title = "Bland-Altman Plot: RM vs Non-RM Agreement",
    subtitle = "Width Density Deviation | Bias near zero = good agreement",
    x = "Mean of RM and Non-RM",
    y = "Difference (Non-RM − RM)"
  ) +
  theme_scientific(base_size = 12) +
  theme(
    plot.subtitle = element_text(size = 10, color = "grey40")
  ) +
  # Expand x-axis to show labels
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(ba_plot)

# =============================================================================
# SAVE OUTPUT
# =============================================================================

if (!dir.exists('outputs/exp1/figures/density_alternatives')) {
  dir.create('outputs/exp1/figures/density_alternatives', recursive = TRUE)
}

ggsave("outputs/exp1/figures/density_alternatives/bland_altman_proper.png",
       ba_plot, width = 8, height = 6, dpi = 300, bg = "white")

cat("\nSaved: outputs/exp1/figures/density_alternatives/bland_altman_proper.png\n")

# =============================================================================
# INTERPRETATION
# =============================================================================

cat("\n=== Interpretation ===\n")
if (abs(bias) < 0.01) {
  cat("✓ Bias is very close to zero - excellent agreement on average\n")
} else if (abs(bias) < 0.02) {
  cat("✓ Bias is small - good agreement on average\n")
} else {
  cat("! Bias may indicate systematic difference between RM and Non-RM\n")
}

# Check for proportional bias (correlation between mean and difference)
cor_test <- cor.test(participant_means$mean_value, participant_means$difference)
cat(sprintf("\nProportional bias check (correlation): r = %.3f, p = %.3f\n",
            cor_test$estimate, cor_test$p.value))

if (cor_test$p.value > 0.05) {
  cat("✓ No significant proportional bias (no funnel pattern)\n")
} else {
  cat("! Significant proportional bias detected\n")
}

# Check how many points fall within LoA
within_loa <- sum(participant_means$difference >= lower_loa &
                    participant_means$difference <= upper_loa)
pct_within <- 100 * within_loa / nrow(participant_means)
cat(sprintf("\nPoints within Limits of Agreement: %d/%d (%.1f%%)\n",
            within_loa, nrow(participant_means), pct_within))
