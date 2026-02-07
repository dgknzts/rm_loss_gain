# Participant-specific number deviation analysis - Exp2
# Visualizes individual participant number deviations across set sizes (3-5)

library(tidyverse)
library(ggplot2)
library(RColorBrewer)

source("analysis/functions/themes.R")

# 1. Load preprocessed data
df <- read.csv("data/exp2/processed.csv")

cat(sprintf("Exp2 data: %d participants, %d trials\n",
            length(unique(df$subID)), nrow(df)))

# 2. Calculate participant means by set size
deviation_data <- df %>%
  filter(!is.na(number_deviation) & !is.na(subID)) %>%
  mutate(subID = factor(subID)) %>%
  group_by(subID, correct_num) %>%
  summarise(
    mean_dev = mean(number_deviation),
    sd_dev = sd(number_deviation),
    n_trials = n(),
    se_dev = sd_dev / sqrt(n_trials),
    ci_lower = mean_dev - 1.96 * se_dev,
    ci_upper = mean_dev + 1.96 * se_dev,
    .groups = "drop"
  )

# 3. Create color mapping for all participants
all_participants <- unique(deviation_data$subID)
n_participants <- length(all_participants)

# Use extended color palette (Set3 supports up to 12, extend if needed)
if (n_participants <= 12) {
  color_values <- brewer.pal(max(3, n_participants), "Set3")
} else {
  # For more than 12 participants, combine palettes
  base_colors <- brewer.pal(12, "Set3")
  extra_colors <- brewer.pal(8, "Dark2")
  color_values <- c(base_colors, extra_colors)[1:n_participants]
}

participant_colors <- setNames(color_values, all_participants)

# 4. Calculate grand average across participants
grand_avg <- deviation_data %>%
  group_by(correct_num) %>%
  summarise(
    grand_mean = mean(mean_dev),
    sd_dev = sd(mean_dev),
    n_subj = n(),
    se_dev = sd_dev / sqrt(n_subj),
    ci_lower = grand_mean - 1.96 * se_dev,
    ci_upper = grand_mean + 1.96 * se_dev,
    .groups = "drop"
  )

# 5. Determine y-axis limits
y_limits <- c(
  floor(min(c(deviation_data$ci_lower, grand_avg$ci_lower), na.rm = TRUE)) - 0.3,
  ceiling(max(c(deviation_data$ci_upper, grand_avg$ci_upper), na.rm = TRUE)) + 0.3
)

# 6. Create visualization
plot_deviation <- ggplot(deviation_data,
                         aes(x = factor(correct_num), y = mean_dev, color = subID)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = -1, linetype = "dotted", color = "#DC143C", alpha = 0.6, linewidth = 0.7) +
  geom_point(position = position_jitter(width = 0.12, seed = 123),
             shape = 21, fill = "white", stroke = 0.8, size = 3, alpha = 0.8) +
  geom_errorbar(data = grand_avg,
                aes(x = factor(correct_num), y = grand_mean,
                    ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 1.2, color = "black", inherit.aes = FALSE) +
  geom_point(data = grand_avg,
             aes(x = factor(correct_num), y = grand_mean),
             size = 5, shape = 16, color = "black", inherit.aes = FALSE) +
  scale_color_manual(values = participant_colors, name = "Participant") +
  scale_y_continuous(limits = y_limits) +
  scale_x_discrete(name = "Set Size") +
  labs(
    title = "Number Deviation by Set Size",
    y = "Mean Number Deviation"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines")
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 3, alpha = 1)))

# 6. Display the plot
print(plot_deviation)

# 7. Save outputs
if (!dir.exists("outputs/exp2/figures")) {
  dir.create("outputs/exp2/figures", recursive = TRUE)
}

ggsave("outputs/exp2/figures/participant_number_deviation.png",
       plot_deviation, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("outputs/exp2/figures/participant_number_deviation.pdf",
       plot_deviation, width = 8, height = 6, bg = "white")
cat("Saved: outputs/exp2/figures/participant_number_deviation.png\n")
cat("Saved: outputs/exp2/figures/participant_number_deviation.pdf\n")

# 8. Print summary statistics
cat("\n=== Summary Statistics ===\n")
summary_stats <- deviation_data %>%
  group_by(correct_num) %>%
  summarise(
    grand_mean = mean(mean_dev),
    sd = sd(mean_dev),
    min = min(mean_dev),
    max = max(mean_dev),
    .groups = "drop"
  )
print(summary_stats)

cat("\n=== Analysis Complete ===\n")
cat(sprintf("Analyzed %d participants across %d set sizes\n",
            n_participants, length(unique(deviation_data$correct_num))))
