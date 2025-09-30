# ==============================================================================
# Extract Participant-Level Data for JASP Analysis
# ==============================================================================

library(tidyverse)

# Load your processed data
df <- read.csv('data/processed.csv')

# Filter and prepare data (same as your main analysis)
analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'NoRM'), levels = c('NoRM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= quantile(correct_space, probs = 1/3, na.rm = TRUE) ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  filter(is.finite(width_deviation_relative))

# ==============================================================================
# Method 1: Wide Format (One row per participant) - RECOMMENDED FOR JASP
# ==============================================================================

# Calculate participant means for each condition
participant_means_wide <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(
    mean_width_deviation = mean(width_deviation_relative, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  # Convert to wide format (separate columns for RM and NoRM)
  pivot_wider(
    names_from = rm_type,
    values_from = c(mean_width_deviation, n_trials),
    names_sep = "_"
  ) %>%
  # Clean up column names
  rename(
    participant_id = subID,
    norm_mean = mean_width_deviation_NoRM,
    rm_mean = mean_width_deviation_RM,
    norm_n_trials = n_trials_NoRM,
    rm_n_trials = n_trials_RM
  ) %>%
  # Calculate difference score (optional - useful for paired t-test)
  mutate(
    difference = rm_mean - norm_mean,
    abs_difference = abs(rm_mean) - abs(norm_mean)  # For accuracy comparison
  ) %>%
  # Remove participants with missing data in either condition
  filter(!is.na(norm_mean) & !is.na(rm_mean))

# Check the data
cat("Wide format data summary:\n")
cat(sprintf("Number of participants: %d\n", nrow(participant_means_wide)))
cat(sprintf("Participants with both conditions: %d\n", sum(!is.na(participant_means_wide$norm_mean) & !is.na(participant_means_wide$rm_mean))))

# Preview
print("First few rows:")
print(head(participant_means_wide))

# Save for JASP
write.csv(participant_means_wide, 'outputs/tables/participant_means_for_jasp.csv', row.names = FALSE)

# ==============================================================================
# Method 2: Long Format (Alternative for some JASP analyses)
# ==============================================================================

participant_means_long <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(
    mean_width_deviation = mean(width_deviation_relative, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  rename(
    participant_id = subID,
    condition = rm_type,
    mean_deviation = mean_width_deviation
  )

# Save long format too
write.csv(participant_means_long, 'outputs/tables/participant_means_long_for_jasp.csv', row.names = FALSE)
