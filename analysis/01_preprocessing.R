# ==============================================================================
# REDUNDANCY MASKING STUDY - DATA PREPROCESSING PIPELINE
# ==============================================================================
# This script processes raw exp1 data files into analysis-ready format
# Input: Raw CSV files in data/raw/
# Output: Processed dataset in data/processed.csv
# ==============================================================================

library(tidyverse)
library(ggplot2)

cat("Starting data preprocessing pipeline for Exp1...\n\n")

# ==============================================================================
# LOAD AND COMBINE RAW DATA FILES
# ==============================================================================

# Initialize empty data frame to store combined results
final_data <- data.frame()

# List all exp1 data files in raw data directory
files <- list.files(path = "data/raw/", pattern = "^[0-9]+_exp1_.*\\.csv$")

cat("Found", length(files), "raw data files to process\n")

# Process each participant's data file
for (i in seq_along(files)) {
  file_name <- files[i]
  full_file_path <- file.path("data/raw/", file_name)
  
  # Read the CSV file
  df <- read.csv(full_file_path, header = TRUE)
  
  # Find the first row after practice trials and last trial of main experiment
  start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
  end_point <- ifelse(nrow(df) == 129, 
                      as.numeric(which(!is.na(df$text.started))) - 1, 
                      as.numeric(nrow(df)))
  
  # Extract main experiment trials
  df <- df[(start_point + 1):(end_point), ]
  
  # Select and clean relevant columns
  df <- df %>%
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
      starting_space_deg,
      response_width_degree,
      response_spacing_degree,
      next_5.rt,
      resp_group
    ) %>%
    # Remove rows that contain between block rows
    na.omit() %>%
    # Take absolute value of response width
    mutate(response_width_degree = abs(response_width_degree)) %>%
    # Clean response column (remove "num_" prefix)
    mutate(response = as.numeric(gsub("num_", "", response))) %>%
    # Handle response_spacing_degree
    mutate(
      response_spacing_degree = case_when(
        response_spacing_degree == "9999" ~ 0,
        as.numeric(response_spacing_degree) >= 0 ~ as.numeric(response_spacing_degree)
      ),
      starting_space_deg = case_when(
        starting_space_deg > 3000 ~ 0,
        starting_space_deg < 3000 ~ starting_space_deg
      )
    ) %>%
    # Add experiment version information
    mutate(exp_version = "exp1")
  
  # Combine with other participants
  final_data <- bind_rows(final_data, df)
}

cat("Processed", nrow(final_data), "trials from", length(unique(final_data$participant)), "participants\n\n")

# ==============================================================================
# DATA CLEANING AND VARIABLE CREATION
# ==============================================================================

# Reorder columns and rename for analysis
col_order <- c("participant", "resp_group", "trial_type", "loop.thisN", "amount", "response", 
               "key_resp.rt", "center_to_center", "response_spacing_degree", "starting_space_deg", 
               "w", "response_width_degree", "starting_width_deg", "next_5.rt", "stim_length", "exp_version")

final_data <- final_data[, col_order]

# Add 1 to trial numbers to remove zeros
final_data$loop.thisN <- final_data$loop.thisN + 1 

# Rename columns to analysis-friendly names
colnames(final_data) <- c("subID", "keyboard_condition", "trial_type", "trial_number", "correct_num", 
                         "response_num", "response_rt", "correct_space", "response_space", "probe_space", 
                         "correct_width", "response_width", "probe_width", "adjustment_duration", 
                         "stim_length", "exp_version")

# Convert subject ID to factor
final_data$subID <- as.factor(final_data$subID)

# ==============================================================================
# COMPUTE DERIVED VARIABLES
# ==============================================================================

df <- final_data %>%
  mutate(
    # Basic deviation measures
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    spacing_deviation = response_space - correct_space,
    
    # Ratio measures
    number_deviation_ratio = number_deviation / correct_num,
    width_deviation_ration = response_width / correct_width,
    spacing_deviation_ratio = response_space / correct_space,
    
    # Stimulus length measures
    response_stim_length = (response_space * (response_num - 1)) + response_width,
    compression_rate = response_stim_length / stim_length,
    
    # Pooled width measures
    actual_pooled_width = correct_width * correct_num,
    response_pooled_width = response_width * response_num,
    pooled_width_deviation = response_pooled_width - actual_pooled_width,
    
    # Density measures
    actual_width_density = actual_pooled_width / stim_length,
    response_width_density = response_pooled_width / response_stim_length,
    width_density_deviation = response_width_density - actual_width_density,
    
    # Edge-to-edge spacing measures
    actual_edge_to_edge_spacing = correct_space - correct_width,
    response_edge_to_edge_spacing = response_space - response_width,
    edge_to_edge_spacing_deviation = response_edge_to_edge_spacing - actual_edge_to_edge_spacing,
    
    # Relative deviation measures (normalized by correct values)
    width_deviation_relative = if_else(correct_width != 0, width_deviation / correct_width, NA_real_),
    spacing_deviation_relative = if_else(correct_space != 0, spacing_deviation / correct_space, NA_real_),
    
    # Placeholder for baseline correction (not available for exp1-only version)
    corrected_width_deviation = NA_real_,
    corrected_width_deviation_relative = NA_real_,
    
    # Spacing condition categories for exp1
    spacing = case_when(
      correct_space <= 0.8 ~ "small",
      correct_space > 0.8 & correct_space <= 1 ~ "middle",
      correct_space > 1 ~ "large",
      TRUE ~ NA_character_
    )
  )

# ==============================================================================
# DATA FILTERING AND EXCLUSION TRACKING
# ==============================================================================

cat("Applying data quality filters...\n")

# Store unfiltered data for exclusion tracking
df_unfiltered <- df
initial_count <- nrow(df_unfiltered)

# Apply filters step by step
df_filtered <- df_unfiltered %>%
  filter(response_rt < 10) %>%                                    # Remove very slow responses
  filter(number_deviation < 4, number_deviation > -4) %>%        # Remove extreme number deviations  
  filter(response_edge_to_edge_spacing > 0 | response_num == 1)   # Remove negative spacing (except single items)

# Track exclusions at each step
filter_steps <- tibble(
  step = c("Initial data", "RT < 10s", "Number deviation [-4,4]", "Edge-to-edge spacing > 0"),
  remaining_trials = c(
    initial_count,
    nrow(df_unfiltered %>% filter(response_rt < 10)),
    nrow(df_unfiltered %>% filter(response_rt < 10, number_deviation < 4, number_deviation > -4)),
    nrow(df_filtered)
  ),
  excluded_trials = c(0, NA, NA, NA)
) %>%
  mutate(excluded_trials = lag(remaining_trials, default = initial_count) - remaining_trials)

# Print exclusion summary
cat("\n=== DATA FILTERING SUMMARY ===\n")
print(filter_steps)
cat("\nFinal dataset:\n")
cat("- Trials:", nrow(df_filtered), "\n")
cat("- Participants:", length(unique(df_filtered$subID)), "\n")
cat("- Exclusion rate:", round((initial_count - nrow(df_filtered)) / initial_count * 100, 2), "%\n\n")

# Create exclusion visualization
exclusion_plot <- ggplot(filter_steps[-1, ], aes(x = step, y = excluded_trials)) +
  geom_col(fill = "steelblue", alpha = 0.7, color = "black") +
  geom_text(aes(label = excluded_trials), vjust = -0.5, size = 4) +
  labs(
    title = "Data Exclusions by Filtering Step - Exp1",
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

# Save exclusion plot
if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}
ggsave("outputs/figures/data_exclusions_exp1.png", exclusion_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# SAVE PROCESSED DATA
# ==============================================================================

# Save final processed dataset
write.csv(df_filtered, "data/processed.csv", row.names = FALSE)

cat("Preprocessing complete!\n")
cat("Processed data saved to: data/processed.csv\n")
cat("Exclusion plot saved to: outputs/figures/data_exclusions_exp1.png\n")
