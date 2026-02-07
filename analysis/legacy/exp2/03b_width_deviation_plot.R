# Width Deviation Plot with Significance Stars - Exp2
# Standalone publication-ready figure

library(tidyverse)
library(ggplot2)

source("analysis/functions/themes.R")

# Load data and contrast results
df <- read.csv("data/exp2/processed.csv")
width_contrast_results <- read.csv("outputs/exp2/tables/width_deviation_rm_contrasts.csv")

# Color scheme
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Data preparation
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

# Calculate participant means
width_participant_means <- analysis_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_deviation, na.rm = TRUE), .groups = "drop")

plot_data_width <- width_participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Add significance stars
width_contrast_results <- width_contrast_results %>%
  mutate(
    correct_width = factor(correct_width),
    correct_num = factor(correct_num),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

width_star_data <- width_contrast_results %>%
  filter(significance != "") %>%
  left_join(
    plot_data_width %>%
      group_by(correct_width, correct_num) %>%
      summarise(star_y = max(ci_upper) + 0.005, .groups = "drop"),
    by = c("correct_width", "correct_num")
  )

# Create plot
width_plot <- plot_data_width %>%
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
    data = width_star_data,
    aes(x = correct_num, y = star_y, label = significance),
    color = "black",
    size = 5,
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Deviation (°)") +
  labs(title = "Width Deviation by True Width and Set Size") +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1, "lines"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(width_plot)

# Save outputs
if (!dir.exists("outputs/exp2/figures")) {
  dir.create("outputs/exp2/figures", recursive = TRUE)
}

ggsave("outputs/exp2/figures/width_deviation_by_width.png",
       width_plot, width = 9, height = 5, dpi = 300, bg = "white")

cat("Saved: outputs/exp2/figures/width_deviation_by_width.png\n")
