# Width Deviation Plot - Experiment 1
# Grand means by set size, faceted by bar width

library(tidyverse)

source("analysis/functions/themes.R")

# Load data and contrast results
df <- read.csv("data/exp1/processed.csv")
contrast_results <- read.csv("outputs/exp1/tables/width_deviation_rm_contrasts.csv")

# RM colors
rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# Prepare data
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"), levels = c("Non-RM", "RM")),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

# Calculate participant means then grand means with 95% CI
participant_means <- analysis_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(mean_dev = mean(width_deviation), .groups = "drop")

plot_data <- participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(mean_dev),
    se = sd(mean_dev) / sqrt(n),
    t_crit = qt(0.975, n - 1),
    ci_lower = grand_mean - t_crit * se,
    ci_upper = grand_mean + t_crit * se,
    .groups = "drop"
  )

# Add significance stars from contrast results
star_data <- contrast_results %>%
  mutate(
    correct_width = factor(correct_width),
    correct_num = factor(correct_num),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  filter(sig != "") %>%
  left_join(
    plot_data %>%
      group_by(correct_width, correct_num) %>%
      summarise(y_pos = max(ci_upper) + 0.008, .groups = "drop"),
    by = c("correct_width", "correct_num")
  )

# Create plot
p <- ggplot(plot_data, aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, linewidth = 0.5,
                position = position_dodge(width = 0.4)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.4)) +
  geom_text(data = star_data,
            aes(x = correct_num, y = y_pos, label = sig),
            color = "black", size = 5, inherit.aes = FALSE) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(x = "Set Size", y = "Width Deviation (°)") +
  theme_scientific() +
  theme(
    legend.position = c(0.12, 0.18),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

print(p)

# Save plot
ggsave("outputs/exp1/figures/width_deviation.png", p,
       width = 7, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/width_deviation.svg", p,
       width = 7, height = 4, bg = "white")
