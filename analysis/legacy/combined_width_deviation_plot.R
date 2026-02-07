# Combined Width Deviation Plot - Exp1 and Exp2
# Panel A: Experiment 1 | Panel B: Experiment 2
# Fixed axes for comparison

library(tidyverse)
library(ggplot2)
library(patchwork)

source("analysis/functions/themes.R")

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

# Experiment 1
df_exp1 <- read.csv("data/exp1/processed.csv")

exp1_data <- df_exp1 %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

# Experiment 2
df_exp2 <- read.csv("data/exp2/processed.csv")

exp2_data <- df_exp2 %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

# Exp1 participant means
exp1_participant_means <- exp1_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_deviation, na.rm = TRUE), .groups = "drop")

exp1_plot_data <- exp1_participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# Exp2 participant means
exp2_participant_means <- exp2_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(participant_mean = mean(width_deviation, na.rm = TRUE), .groups = "drop")

exp2_plot_data <- exp2_participant_means %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(
    n = n(),
    grand_mean = mean(participant_mean, na.rm = TRUE),
    se = sd(participant_mean, na.rm = TRUE) / sqrt(n),
    ci_lower = grand_mean - qt(0.975, n-1) * se,
    ci_upper = grand_mean + qt(0.975, n-1) * se,
    .groups = "drop"
  )

# =============================================================================
# CALCULATE GLOBAL Y-AXIS LIMITS
# =============================================================================

all_ci_lower <- c(exp1_plot_data$ci_lower, exp2_plot_data$ci_lower)
all_ci_upper <- c(exp1_plot_data$ci_upper, exp2_plot_data$ci_upper)

y_lim <- c(
 floor(min(all_ci_lower, na.rm = TRUE) * 20) / 20,
  ceiling(max(all_ci_upper, na.rm = TRUE) * 20) / 20
)

cat(sprintf("Global Y limits: [%.3f, %.3f]\n", y_lim[1], y_lim[2]))

# =============================================================================
# PANEL A: EXPERIMENT 1
# =============================================================================

panel_a <- exp1_plot_data %>%
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
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Deviation (°)", limits = y_lim) +
  labs(
    title = "Experiment 1",
    subtitle = sprintf("N = %d participants", length(unique(exp1_data$subID)))
  ) +
  theme_scientific(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================================
# PANEL B: EXPERIMENT 2
# =============================================================================

panel_b <- exp2_plot_data %>%
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
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(name = "Set Size") +
  scale_y_continuous(name = "Width Deviation (°)", limits = y_lim) +
  labs(
    title = "Experiment 2",
    subtitle = sprintf("N = %d participants", length(unique(exp2_data$subID)))
  ) +
  theme_scientific(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================================
# COMBINE PANELS (VERTICAL LAYOUT)
# =============================================================================

combined_plot <- (panel_a / panel_b) +
  plot_annotation(
    title = "Width Deviation by True Width and Set Size",
    subtitle = "Green = Non-RM | Red = RM | Error bars = 95% CI",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0.5),
      plot.tag = element_text(size = 12, face = "bold")
    )
  )

print(combined_plot)

# =============================================================================
# SAVE OUTPUT
# =============================================================================

if (!dir.exists('outputs/combined')) {
  dir.create('outputs/combined', recursive = TRUE)
}

ggsave("outputs/combined/width_deviation_exp1_exp2_combined.png",
       combined_plot, width = 11, height = 8, dpi = 300, bg = "white")

cat("\nSaved: outputs/combined/width_deviation_exp1_exp2_combined.png\n")
