# ==============================================================================
# REDUNDANCY MASKING STUDY - EXP2 DATA PREPROCESSING PIPELINE
# ==============================================================================
# This script processes raw exp2 data files into analysis-ready format
# Input: Raw CSV files in data/exp2/raw/
# Output: Processed dataset in data/exp2/processed.csv
# NOTE: Exp2 has only width adjustments (no spacing adjustments)
# ==============================================================================

library(tidyverse)
library(ggplot2)

# ==============================================================================
# LOAD AND COMBINE RAW DATA FILES
# ==============================================================================

# Initialize empty data frame to store combined results
final_data <- data.frame()

# List all exp2 data files in raw data directory
files <- list.files(path = "data/exp2/raw/", pattern = "^p[0-9]+_exp1_.*\\.csv$")

cat("Found", length(files), "raw data files to process\n")

# Process each participant's data file
for (i in seq_along(files)) {
  file_name <- files[i]
  full_file_path <- file.path("data/exp2/raw/", file_name)
  
  # Read the CSV file
  df <- read.csv(full_file_path, header = TRUE)
    
    # Find the first row after practice trials and last trial of main experiment
    start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
  end_point <- as.numeric(nrow(df))
  
  # Extract main experiment trials
  df <- df[(start_point + 1):(end_point), ]
  
  # Select and clean relevant columns (no spacing columns for exp2)
  df <- df %>%
    filter(key_resp.rt != "None") %>% #only 1 trial rt is not saved due too quick response from sbj12
    mutate(key_resp.rt = as.double(key_resp.rt)) %>%
    select(
      participant,
      stim_length,
      amount,
      center_to_center,
      w,
      trial_type,
      loop.thisN,
      response,
      key_resp.rt,
      starting_width_deg,
      response_width_degree,
      next_5.rt,
      resp_group
    ) %>%
    # Remove rows that contain between block rows
    na.omit() %>%
    # Take absolute value of response width
    mutate(response_width_degree = abs(response_width_degree)) %>%
    # Clean response column (remove "num_" prefix)
    mutate(response = as.numeric(gsub("num_", "", response))) %>%
    # Add experiment version information
    mutate(exp_version = "exp2") %>%
    mutate()
  
  # Combine with other participants
  final_data <- bind_rows(final_data, df)
}

cat("Processed", nrow(final_data), "trials from", length(unique(final_data$participant)), "participants\n\n")

# ==============================================================================
# DATA CLEANING AND VARIABLE CREATION
# ==============================================================================

# Reorder columns and rename for analysis
col_order <- c("participant", "resp_group", "trial_type", "loop.thisN", "amount", "response",
               "key_resp.rt", "center_to_center", "w", "response_width_degree",
               "starting_width_deg", "next_5.rt", "stim_length", "exp_version")

final_data <- final_data[, col_order]

# Add 1 to trial numbers to remove zeros
final_data$loop.thisN <- final_data$loop.thisN + 1

# Rename columns to analysis-friendly names
colnames(final_data) <- c("subID", "keyboard_condition", "trial_type", "trial_number", "correct_num",
                          "response_num", "response_rt", "correct_space", "correct_width",
                          "response_width", "probe_width", "adjustment_duration",
                          "stim_length", "exp_version")

# Convert subject ID to factor
final_data$subID <- as.factor(final_data$subID) 

# ==============================================================================
# COMPUTE DERIVED VARIABLES
# ==============================================================================

df <- final_data %>%
  mutate(
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    number_deviation_ratio = number_deviation / correct_num,
    width_deviation_ratio = response_width / correct_width,
    actual_pooled_width = correct_width * correct_num,
    response_pooled_width = response_width * response_num,
    pooled_width_deviation = response_pooled_width - actual_pooled_width,
    width_deviation_relative = if_else(correct_width != 0, width_deviation / correct_width, NA_real_),
    absolute_width_deviation = abs(width_deviation)
  )

# ==============================================================================
# DATA FILTERING AND EXCLUSION TRACKING
# ==============================================================================

cat("Applying data quality filters...\n")

df_unfiltered <- df
initial_count <- nrow(df_unfiltered)

# Step 1: Number deviation filter
df_step1 <- df_unfiltered %>%
  filter(number_deviation >= -4, number_deviation <= 4)

# Step 2: Per-participant SD filter for response_rt (±2 SD)
df_step2 <- df_step1 %>%
  group_by(subID) %>%
  mutate(
    rt_mean = mean(response_rt, na.rm = TRUE),
    rt_sd = sd(response_rt, na.rm = TRUE),
    rt_lower = rt_mean - 2.5 * rt_sd,
    rt_upper = rt_mean + 2.5 * rt_sd,
    rt_outlier = response_rt < rt_lower | response_rt > rt_upper
  ) %>%
  ungroup() %>%
  #filter(!rt_outlier) %>%
  select(-rt_mean, -rt_sd, -rt_lower, -rt_upper, -rt_outlier)

# Step 3: Per-participant SD filter for adjustment_duration (±2.5 SD)
df_step3 <- df_step2 %>%
  group_by(subID) %>%
  mutate(
    adj_mean = mean(adjustment_duration, na.rm = TRUE),
    adj_sd = sd(adjustment_duration, na.rm = TRUE),
    adj_lower = adj_mean - 2.5 * adj_sd,
    adj_upper = adj_mean + 2.5 * adj_sd,
    adj_outlier = adjustment_duration < adj_lower | adjustment_duration > adj_upper
  ) %>%
  ungroup() %>%
  #filter(!adj_outlier) %>%
  select(-adj_mean, -adj_sd, -adj_lower, -adj_upper, -adj_outlier)

# Step 4: Exclude specific participants
excluded_participants <- c() #
df_filtered <- df_step3 %>%
  filter(!subID %in% excluded_participants)

# Track exclusions at each step
n_excluded_participants <- length(excluded_participants)
filter_steps <- tibble(
  step = c("Initial data",
           "Number deviation [-4,4]",
           "Response RT (±2.5 SD per participant)",
           "Adjustment duration (±2.5 SD per participant)",
           paste0("Excluded participants (n=", n_excluded_participants, ")")),
  remaining_trials = c(initial_count, nrow(df_step1), nrow(df_step2), nrow(df_step3), nrow(df_filtered)),
  excluded_trials = c(0, NA, NA, NA, NA)
) %>%
  mutate(
    excluded_trials = lag(remaining_trials, default = initial_count) - remaining_trials,
    percent_excluded = round(excluded_trials / lag(remaining_trials, default = initial_count) * 100, 2)
  )

# Print exclusion summary
cat("\n=== DATA FILTERING SUMMARY ===\n")
print(filter_steps, n = Inf)
cat("\nFinal dataset:\n")
cat("- Trials:", nrow(df_filtered), "\n")
cat("- Participants:", length(unique(df_filtered$subID)), "\n")
cat("- Total exclusion rate:", round((initial_count - nrow(df_filtered)) / initial_count * 100, 2), "%\n\n")

# Create exclusion visualization
exclusion_plot <- ggplot(filter_steps[-1, ], aes(x = step, y = excluded_trials)) +
  geom_col(fill = "steelblue", alpha = 0.7, color = "black") +
  geom_text(aes(label = paste0(excluded_trials, "\n(", percent_excluded, "%)")),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Data Exclusions by Filtering Step - Exp2",
    subtitle = paste0("Final dataset: ", nrow(df_filtered), " trials from ",
                      length(unique(df_filtered$subID)), " participants"),
    x = "Filtering Step",
    y = "Trials Excluded"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

if (!dir.exists("outputs/exp2/figures")) {
  dir.create("outputs/exp2/figures", recursive = TRUE)
}
ggsave("outputs/exp2/figures/data_exclusions_exp2.png", exclusion_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# SAVE PROCESSED DATA
# ==============================================================================

# Save final processed dataset
write.csv(df_filtered, "data/exp2/processed.csv", row.names = FALSE)

