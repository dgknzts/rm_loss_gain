# One-Bar Baseline Width Deviation Analysis
# Visualization of width judgment accuracy in baseline condition (no redundancy masking)

library(tidyverse)
library(viridis)

# Load one-bar baseline data
one_bar_data <- read.csv("datasets/one_bar_Exp1ABC.csv")

# Data preparation and participant-level aggregation
participant_data <- one_bar_data %>%
  group_by(participant, exp_version, correct_width) %>%
  summarise(
    mean_width_deviation = mean(width_deviation, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_width_deviation))

# Group-level statistics for error bars
group_stats <- participant_data %>%
  group_by(exp_version, correct_width) %>%
  summarise(
    n_participants = n(),
    mean_deviation = mean(mean_width_deviation, na.rm = TRUE),
    se_deviation = sd(mean_width_deviation, na.rm = TRUE) / sqrt(n_participants),
    ci_lower = mean_deviation - 1.96 * se_deviation,
    ci_upper = mean_deviation + 1.96 * se_deviation,
    .groups = "drop"
  )

# Custom theme for clean, elegant appearance
theme_baseline <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold"),
    panel.spacing = unit(1, "lines")
  )

# Create the baseline width deviation plot
baseline_plot <- ggplot() +
  # Individual participant points (semi-transparent, jittered)
  geom_point(
    data = participant_data,
    aes(x = correct_width, y = mean_width_deviation),
    alpha = 0.4,
    size = 2,
    color = "#2C3E50",
    position = position_jitter(width = 0.02, height = 0)
  ) +
  
  # Group means with confidence interval error bars
  geom_errorbar(
    data = group_stats,
    aes(x = correct_width, ymin = ci_lower, ymax = ci_upper),
    width = 0.03,
    size = 1,
    color = "#E74C3C"
  ) +
  
  geom_point(
    data = group_stats,
    aes(x = correct_width, y = mean_deviation),
    size = 4,
    color = "#E74C3C"
  ) +
  
  # Reference line at perfect accuracy (y = 0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  
  # Facet by experiment version
  facet_wrap(~exp_version, labeller = label_both) +
  
  # Labels and title
  labs(
    title = "One-Bar Baseline: Width Judgment Accuracy",
    subtitle = "Individual participant means (gray) and group means with 95% CI (red)",
    x = "Correct Width (degrees)",
    y = "Width Deviation (degrees)",
    caption = "Dashed line indicates perfect accuracy (deviation = 0)"
  ) +
  
  # Apply custom theme
  theme_baseline +
  
  # Scale adjustments for better readability
  scale_x_continuous(breaks = function(x) pretty(x, n = 4)) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 6))

# Display the plot
print(baseline_plot)

# Create figures directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Save the plot
ggsave(
  "figures/one_bar_baseline_width_deviation.png",
  baseline_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Print summary statistics
cat("ONE-BAR BASELINE SUMMARY\n")
cat("=======================\n\n")

summary_table <- group_stats %>%
  mutate(
    mean_deviation = round(mean_deviation, 3),
    se_deviation = round(se_deviation, 3),
    ci_range = paste0("[", round(ci_lower, 3), ", ", round(ci_upper, 3), "]")
  ) %>%
  select(exp_version, correct_width, n_participants, mean_deviation, se_deviation, ci_range)

print(summary_table)

cat("\nParticipant counts by experiment:\n")
participant_counts <- participant_data %>%
  group_by(exp_version) %>%
  summarise(n_participants = n_distinct(participant), .groups = "drop")
print(participant_counts)