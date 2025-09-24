# Experiment 2 Width Deviation Analysis
# Focused visualization with trial count size aesthetic and relative comparison

library(tidyverse)
library(patchwork)

# Load data
df <- read.csv("datasets/processed.csv")

# Prepare Experiment 2 data
exp2_data <- df %>%
  filter(exp_version == "Exp1B") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    RM_condition = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                         levels = c("NoRM", "RM")),
    correct_width = factor(correct_width),
    correct_num = factor(correct_num),
    width_deviation_relative = width_deviation / as.numeric(as.character(correct_width))
  ) %>%
  filter(!is.na(width_deviation), !is.na(width_deviation_relative))

# Calculate participant-level means for width deviation
participant_means <- exp2_data %>%
  group_by(subID, RM_condition, correct_width, correct_num) %>%
  summarise(
    mean_width_deviation = mean(width_deviation, na.rm = TRUE),
    mean_width_deviation_relative = mean(width_deviation_relative, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Calculate trial percentages within each condition for size aesthetic
trial_percentages <- exp2_data %>%
  group_by(correct_width, correct_num, RM_condition) %>%
  summarise(condition_trials = n(), .groups = "drop") %>%
  group_by(correct_width, correct_num) %>%
  mutate(
    total_condition_trials = sum(condition_trials),
    trial_percentage = (condition_trials / total_condition_trials) * 100
  ) %>%
  ungroup()

# Calculate grand means with confidence intervals (matching corWidth_setSize_RMvsNOT.R approach)
grand_means <- exp2_data %>%
  group_by(RM_condition, correct_width, correct_num) %>%
  summarise(
    emmean = mean(width_deviation, na.rm = TRUE),
    n_valid = sum(!is.na(width_deviation)),
    se = sd(width_deviation, na.rm = TRUE) / sqrt(n_valid),
    ci_lower = emmean - 1.96 * se,
    ci_upper = emmean + 1.96 * se,
    .groups = "drop"
  ) %>%
  left_join(trial_percentages, by = c("RM_condition", "correct_width", "correct_num"))

# Calculate participant-level means for relative width deviation (combined across conditions)
participant_means_relative <- exp2_data %>%
  group_by(subID, RM_condition) %>%
  summarise(
    mean_width_deviation_relative = mean(width_deviation_relative, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Calculate overall trial percentages for relative width deviation plot
overall_trial_percentages <- exp2_data %>%
  group_by(RM_condition) %>%
  summarise(condition_trials = n(), .groups = "drop") %>%
  mutate(
    total_trials = sum(condition_trials),
    trial_percentage = (condition_trials / total_trials) * 100
  )

# Calculate grand means for relative width deviation (matching corWidth_setSize_RMvsNOT.R approach)
grand_means_relative <- exp2_data %>%
  group_by(RM_condition) %>%
  summarise(
    emmean = mean(width_deviation_relative, na.rm = TRUE),
    n_valid = sum(!is.na(width_deviation_relative)),
    se = sd(width_deviation_relative, na.rm = TRUE) / sqrt(n_valid),
    ci_lower = emmean - 1.96 * se,
    ci_upper = emmean + 1.96 * se,
    .groups = "drop"
  ) %>%
  left_join(overall_trial_percentages, by = "RM_condition")

# Color scheme
colors <- c("RM" = "#E74C3C", "NoRM" = "#3498DB")

# Clean theme
theme_clean <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    panel.spacing = unit(0.8, "lines")
  )

# Plot 1: Width deviation by correct width, faceted by set size, with percentage-based size aesthetic
p1 <- ggplot(grand_means, aes(x = correct_width, y = emmean, 
                             color = RM_condition, size = trial_percentage)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.25, size = 0.5, alpha = 0.8, 
                position = position_dodge(width = 0.3)) +
  geom_point(alpha = 0.9, position = position_dodge(width = 0.3)) +
  facet_grid(~correct_num, labeller = label_both) +
  scale_color_manual(values = colors, name = "Condition") +
  scale_size_continuous(name = "Trial %", range = c(3, 6), 
                       breaks = c(30, 50, 70),
                       labels = c("30%", "50%", "70%")) +
  labs(
    title = "Experiment 2: Width Deviation by Correct Width and Set Size",
    x = "Correct Width",
    y = "Width Deviation"
  ) +
  theme_clean +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend(override.aes = list(color = "black"))
  )

# Plot 2: Relative width deviation bar plot (all conditions combined) with error bars
p2 <- ggplot(grand_means_relative, aes(x = RM_condition, y = emmean, 
                                      fill = RM_condition, size = trial_percentage)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, color = "black", alpha = 0.8, size = 1) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Experiment 2: Relative Width Deviation",
    x = "Condition",
    y = "Relative Width Deviation"
  ) +
  theme_clean +
  theme(legend.position = "none")

# Combine plots with patchwork
combined_plot <- p1 + p2 + 
  plot_layout(widths = c(2, 1))

# Display and save
print(combined_plot)

# Create figures directory if needed
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Save plot
ggsave("figures/exp2_width_deviation_focused.png", 
       combined_plot,
       width = 14, height = 8, dpi = 300, bg = "white")
