# Number Deviation Plot - Experiment 1
# Visualizes RM prevalence: participant dots + grand mean by set size

library(tidyverse)
library(RColorBrewer)

source("analysis/functions/themes.R")

# Load data
df <- read.csv("data/exp1/processed.csv")

# Calculate participant means by set size
participant_means <- df %>%
  filter(!is.na(number_deviation)) %>%
  mutate(subID = factor(subID)) %>%
  group_by(subID, correct_num) %>%
  summarise(
    mean_dev = mean(number_deviation),
    n_trials = n(),
    .groups = "drop"
  )

# Create color palette for participants
n_participants <- length(unique(participant_means$subID))
if (n_participants <= 12) {
  color_values <- brewer.pal(max(3, n_participants), "Set3")
} else {
  color_values <- c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))[1:n_participants]
}
participant_colors <- setNames(color_values, unique(participant_means$subID))

# Calculate grand average with 95% CI (t-distribution)
grand_avg <- participant_means %>%
  group_by(correct_num) %>%
  summarise(
    grand_mean = mean(mean_dev),
    sd_dev = sd(mean_dev),
    n_subj = n(),
    se_dev = sd_dev / sqrt(n_subj),
    t_crit = qt(0.975, n_subj - 1),
    ci_lower = grand_mean - t_crit * se_dev,
    ci_upper = grand_mean + t_crit * se_dev,
    .groups = "drop"
  )

# Create plot
p <- ggplot() +
  # Reference line at 0 (accurate reporting)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = -1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  # Participant dots (small, jittered, semi-transparent)
  geom_point(data = participant_means,
             aes(x = factor(correct_num), y = mean_dev),
             position = position_jitter(width = 0.15, seed = 42),
             size = 1, alpha = 0.2) +

  # Grand mean error bars (95% CI)
  geom_errorbar(data = grand_avg,
                aes(x = factor(correct_num), y = grand_mean,
                    ymin = ci_lower, ymax = ci_upper),
                width = 0.08, linewidth = 1, color = "black") +

  # Grand mean dots (large, black)
  geom_point(data = grand_avg,
             aes(x = factor(correct_num), y = grand_mean),
             size = 3, color = "black") +

  # Scales and labels
  scale_color_manual(values = participant_colors, guide = "none") +
  scale_y_continuous(limits = c(-1.2, 0.5)) +
  labs(
    x = "Set Size",
    y = "Number Deviation"
  ) +
  theme_scientific() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  )

print(p)

# Save plot
ggsave("outputs/exp1/figures/number_deviation.png", p,
       width = 4, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/number_deviation.svg", p,
       width = 4, height = 4, bg = "white")
