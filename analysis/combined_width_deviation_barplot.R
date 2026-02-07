# Combined Width Deviation Bar Plot
# Comparing Exp1 (Multi-bar probe) vs Exp2 (Single-bar probe)

library(tidyverse)

source("analysis/functions/themes.R")

# RM colors
rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

# Load exp1 data
df_exp1 <- read.csv("data/exp1/processed.csv") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    subID = as.character(subID),
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"), levels = c("Non-RM", "RM")),
    experiment = "Experiment 1\n(Multi-bar probe)"
  ) %>%
  drop_na(width_deviation_relative)

# Load exp2 data
df_exp2 <- read.csv("data/exp2/processed.csv") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    subID = as.character(subID),
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"), levels = c("Non-RM", "RM")),
    experiment = "Experiment 2\n(Single-bar probe)"
  ) %>%
  drop_na(width_deviation_relative)

# Combine data
df_combined <- bind_rows(df_exp1, df_exp2) %>%
  mutate(experiment = factor(experiment, levels = c("Experiment 1\n(Multi-bar probe)",
                                                      "Experiment 2\n(Single-bar probe)")))

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

# Subject means
subject_means <- df_combined %>%
  group_by(experiment, subID, rm_type) %>%
  summarise(
    mean_dev = mean(width_deviation_relative, na.rm = TRUE),
    .groups = "drop"
  )

# Grand means with 95% CI
plot_data <- subject_means %>%
  group_by(experiment, rm_type) %>%
  summarise(
    n = n(),
    mean = mean(mean_dev),
    sd = sd(mean_dev),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n - 1) * se,
    ci_upper = mean + qt(0.975, n - 1) * se,
    .groups = "drop"
  )

# =============================================================================
# CREATE PLOT (GROUPED VERSION)
# =============================================================================

# Shorter experiment labels for x-axis
plot_data <- plot_data %>%
  mutate(experiment_short = case_when(
    experiment == "Experiment 1\n(Multi-bar probe)" ~ "Exp 1\n(Multi-bar probe)",
    experiment == "Experiment 2\n(Single-bar probe)" ~ "Exp 2\n(Single-bar probe)"
  ))

p <- ggplot(plot_data, aes(x = experiment_short, y = mean, fill = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 0.8, color = "black",
                position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  labs(
    x = NULL,
    y = "Relative Width Deviation"
  ) +
  scale_y_continuous(limits = c(-0.15, 0.1)) +
  theme_scientific() +
  theme(
    legend.position = c(0.15, 0.85),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

print(p)

# Save plot
ggsave("outputs/combined_width_deviation_barplot.png", p,
       width = 5, height = 4, dpi = 300, bg = "white")
ggsave("outputs/combined_width_deviation_barplot.svg", p,
       width = 5, height = 4, bg = "white")

