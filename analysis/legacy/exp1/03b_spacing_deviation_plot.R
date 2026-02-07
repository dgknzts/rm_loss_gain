# Spacing Deviation Plot with Significance Stars - Exp1
# Standalone publication-ready figure

library(tidyverse)
library(ggplot2)

source("analysis/functions/themes.R")

# Load data and contrast results
df <- read.csv("data/exp1/processed.csv")
spacing_contrast_results <- read.csv("outputs/exp1/tables/spacing_deviation_rm_contrasts.csv")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Data preparation
spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(spacing_deviation)

# Calculate participant means
spacing_participant_means <- analysis_data %>%
  group_by(subID, correct_num, spacing_category, rm_type) %>%
  summarise(participant_mean = mean(spacing_deviation, na.rm = TRUE), .groups = "drop")

plot_data_spacing <- spacing_participant_means %>%
  group_by(correct_num, spacing_category, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Add significance stars
spacing_contrast_results <- spacing_contrast_results %>%
  mutate(
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger')),
    correct_num = factor(correct_num),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

spacing_star_data <- spacing_contrast_results %>%
  filter(significance != "") %>%
  left_join(
    plot_data_spacing %>%
      group_by(spacing_category, correct_num) %>%
      summarise(star_y = max(ci_upper) + 0.01, .groups = "drop"),
    by = c("spacing_category", "correct_num")
  )

# Create plot
spacing_plot <- plot_data_spacing %>%
  ggplot(aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.8,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(
    size = 3.5,
    position = position_dodge(width = 0.4)
  ) +
  geom_text(
    data = spacing_star_data,
    aes(x = correct_num, y = star_y, label = significance),
    color = "black",
    size = 5,
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~spacing_category, labeller = as_labeller(function(x) paste("Spacing:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Spacing Deviation (°)") +
  labs(title = "Spacing Deviation by Spacing Category and Set Size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1, "lines"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(spacing_plot)

# Save outputs
if (!dir.exists("outputs/exp1/figures")) {
  dir.create("outputs/exp1/figures", recursive = TRUE)
}

ggsave("outputs/exp1/figures/spacing_deviation_by_category.png",
       spacing_plot, width = 9, height = 5, dpi = 300, bg = "white")

cat("Saved: outputs/exp1/figures/spacing_deviation_by_category.png\n")
cat("Saved: outputs/exp1/figures/spacing_deviation_by_category.pdf\n")
