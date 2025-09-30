library(tidyverse)
library(ggplot2)

# Load all initial trials
df <- read_csv("data/processed.csv") %>%
  filter(trial_type == "initial")

# Calculate RM percentage for each width/spacing/set size combination
strongest_conditions <- df %>%
  group_by(correct_width, correct_space, correct_num) %>%
  summarise(
    total_trials = n(),
    rm_trials = sum(number_deviation == -1, na.rm = TRUE),
    rm_percentage = (rm_trials / total_trials) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(rm_percentage))

print("Strongest RM conditions by percentage (initial trials only):")
print(strongest_conditions)

# Plot RM percentage by width, spacing, and set size
ggplot(strongest_conditions, aes(x = correct_space, y = rm_percentage, color = factor(correct_num))) +
  geom_line(alpha = 0.7) +
  geom_point(size = 4, alpha = 0.7) +
  facet_wrap(~ paste("Width:", correct_width)) +
  labs(
    title = "RM Percentage by Width, Spacing, and Set Size (Initial Trials Only)",
    x = "Center-to-Center Spacing", 
    y = "RM Percentage (%)",
    color = "Set Size",
    size = "Total Trials"
  ) +
  theme_minimal()
