# One-Bar Width Analysis - Experiments 1A, 1B, and 1C
# Load required libraries
library(tidyverse)

# =============================================================================
# 1. DEFINE FILE PATHS AND LOAD DATA
# =============================================================================

# Define directories
exp1A_control_dir <- "datasets/Exp1A/one_bar/"
exp1BC_dirs <- c("datasets/Exp1B/", "datasets/Exp1C/")

# Get Exp1A control files
files_A <- list.files(path = exp1A_control_dir,
                      pattern = "^[0-9]+_control_.*\\.csv$",
                      full.names = TRUE)

# Get Exp1B and Exp1C files
files_BC <- unlist(lapply(exp1BC_dirs, function(dir) {
  list.files(path = dir, 
             pattern = "^[0-9]+_exp1_.*\\.csv$", 
             full.names = TRUE) 
}))

# Combine all file paths
all_files <- c(files_A, files_BC)

# Load all CSV files
raw_data_list <- list()
for (i in seq_along(all_files)) {
  raw_data_list[[i]] <- read.csv(all_files[i], header = TRUE)
}

# =============================================================================
# 2. PROCESS DATA FROM EACH EXPERIMENT
# =============================================================================

# Initialize combined dataset
one_bar_width <- data.frame()

# Process each file
for (i in seq_along(raw_data_list)) {
  df <- raw_data_list[[i]]
  current_file_path <- all_files[i]
  
  # Determine experiment version based on file path
  if (grepl("Exp1A/one_bar/", current_file_path)) {
    exp_version <- "Exp1A"
    
    # Process Exp1A data (all rows are one-bar trials)
    if (all(c("participant", "w", "response_width_degree") %in% colnames(df))) {
      processed_data <- df %>%
        select(participant, w, response_width_degree) %>%
        mutate(
          # Convert to numeric if stored as character/factor
          w = as.numeric(as.character(w)),
          response_width_degree = as.numeric(as.character(response_width_degree))
        ) %>%
        filter(!is.na(participant) & !is.na(w) & !is.na(response_width_degree)) %>%
        mutate(
          response_width_degree = abs(response_width_degree),
          width_deviation = round(response_width_degree - w, 6)  # Round to avoid scientific notation
        ) %>%
        rename(correct_width = w) %>%
        mutate(exp_version = exp_version) %>%
        select(participant, correct_width, response_width_degree, width_deviation, exp_version)
      
      one_bar_width <- bind_rows(one_bar_width, processed_data)
    }
    
  } else if (grepl("Exp1B", current_file_path) || grepl("Exp1C", current_file_path)) {
    exp_version <- ifelse(grepl("Exp1B", current_file_path), "Exp1B", "Exp1C")
    
    # Process Exp1B/1C data (filter for one-bar control trials)
    if ("first.thisN" %in% colnames(df)) {
      control_trials <- df %>% filter(!is.na(first.thisN))
      
      if (nrow(control_trials) > 0 && 
          all(c("participant", "w", "response_width_degree") %in% colnames(control_trials))) {
        
        processed_data <- control_trials %>%
          select(participant, w, response_width_degree) %>%
          mutate(
            # Convert to numeric if stored as character/factor
            w = as.numeric(as.character(w)),
            response_width_degree = as.numeric(as.character(response_width_degree))
          ) %>%
          filter(!is.na(participant) & !is.na(w) & !is.na(response_width_degree)) %>%
          mutate(
            response_width_degree = round(abs(response_width_degree),2),
            width_deviation = response_width_degree - w # Round to avoid scientific notation
          ) %>%
          rename(correct_width = w) %>%
          mutate(exp_version = exp_version) %>%
          select(participant, correct_width, response_width_degree, width_deviation, exp_version)
        
        one_bar_width <- bind_rows(one_bar_width, processed_data)
      }
    }
  }
}

# =============================================================================
# 3. SAVE PROCESSED DATA
# =============================================================================

write.csv(one_bar_width, "datasets/one_bar_Exp1ABC.csv", row.names = FALSE)

# =============================================================================
# 4. CREATE SUMMARY STATISTICS
# =============================================================================

# Summary by experiment version
summary_stats <- one_bar_width %>%
  group_by(exp_version) %>%
  summarise(
    n_participants = n_distinct(participant),
    n_trials = n(),
    mean_width_deviation = mean(width_deviation),
    sd_width_deviation = sd(width_deviation)
  )

print(summary_stats)

# Trial counts per participant
trial_counts <- one_bar_width %>%
  group_by(participant, exp_version) %>%
  summarise(n_trials = n(), .groups = "drop")

print(trial_counts)

# =============================================================================
# 5. PREPARE DATA FOR VISUALIZATION
# =============================================================================

# Calculate average across different correct_widths for each participant and experiment
averaged_data <- one_bar_width %>%
  group_by(participant, exp_version) %>%
  summarise(
    mean_correct_width = mean(correct_width),
    mean_response_width = mean(response_width_degree),
    .groups = "drop"
  ) %>%
  mutate(
    width_deviation_ratio = mean_response_width / mean_correct_width
  )

print("Averaged data summary:")
print(averaged_data)

# =============================================================================
# 6. CREATE VISUALIZATION
# =============================================================================

width_boxplot <- ggplot(averaged_data, 
                        aes(x = width_deviation_ratio, 
                            y = as.factor(participant))) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0, height = 0.1, size = 0.5, alpha = 0.3) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Width Deviation Ratio by Participant",
    subtitle = "Experiments 1A, 1B, and 1C (Averaged across correct widths)",
    x = "Width Deviation Ratio (Reported / Correct)",
    y = "Participant ID",
    caption = "Red line indicates perfect accuracy (ratio = 1.0)"
  ) +
  facet_wrap(~ exp_version) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "none"
  )

print(width_boxplot)
