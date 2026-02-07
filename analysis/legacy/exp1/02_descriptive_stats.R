# ==============================================================================
# REDUNDANCY MASKING STUDY - DESCRIPTIVE STATISTICS
# ==============================================================================
# This script generates comprehensive descriptive statistics for exp1
# Input: Processed dataset from data/processed.csv
# Output: Descriptive tables saved to outputs/exp1/tables/
# ==============================================================================

library(tidyverse)
library(knitr)

cat("Generating descriptive statistics for Exp1...\n\n")

# Load processed data
df <- read.csv("data/exp1/processed.csv")

# ==============================================================================
# SAMPLE CHARACTERISTICS
# ==============================================================================

cat("=== SAMPLE CHARACTERISTICS ===\n")

# Basic sample info
total_trials <- nrow(df)
total_participants <- length(unique(df$subID))
trials_per_participant <- df %>%
  group_by(subID) %>%
  summarise(n_trials = n(), .groups = "drop")

cat("Total trials:", total_trials, "\n")
cat("Total participants:", total_participants, "\n")
cat("Mean trials per participant:", round(mean(trials_per_participant$n_trials), 1), "\n")
cat("Range trials per participant:", min(trials_per_participant$n_trials), "-", max(trials_per_participant$n_trials), "\n\n")

# ==============================================================================
# EXPERIMENTAL DESIGN OVERVIEW
# ==============================================================================

cat("=== EXPERIMENTAL DESIGN ===\n")

# RM condition distribution
rm_distribution <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(RM_condition = ifelse(number_deviation == -1, "RM", "NoRM")) %>%
  count(RM_condition) %>%
  mutate(percentage = round(n/sum(n) * 100, 1))

print(rm_distribution)

# Factor level distributions
cat("\nSet sizes (correct_num):", paste(sort(unique(df$correct_num)), collapse = ", "), "\n")
cat("Bar widths (correct_width):", paste(sort(unique(df$correct_width)), collapse = ", "), "\n")
cat("Spacing range (correct_space):", round(min(df$correct_space), 2), "-", round(max(df$correct_space), 2), "\n")

# Spacing categories
spacing_dist <- df %>%
  count(spacing) %>%
  mutate(percentage = round(n/sum(n) * 100, 1))
cat("\nSpacing categories:\n")
print(spacing_dist)

# ==============================================================================
# OUTCOME VARIABLE DESCRIPTIVES
# ==============================================================================

cat("\n=== OUTCOME VARIABLE DESCRIPTIVES ===\n")

# Prepare data for RM analysis
df_rm <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    RM_condition = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                         levels = c("NoRM", "RM")),
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
      correct_space <= 0.9 ~ "Middle", 
      TRUE ~ "Larger"
    )
  )

# Key outcome variables for analysis
outcome_vars <- c(
  "width_deviation", "spacing_deviation", "width_density_deviation",
  "pooled_width_deviation", "edge_to_edge_spacing_deviation",
  "width_deviation_relative", "spacing_deviation_relative"
)

# Generate descriptives by RM condition
descriptives <- df_rm %>%
  select(RM_condition, all_of(outcome_vars)) %>%
  gather(key = "variable", value = "value", -RM_condition) %>%
  group_by(variable, RM_condition) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 4),
    sd = round(sd(value, na.rm = TRUE), 4),
    median = round(median(value, na.rm = TRUE), 4),
    q25 = round(quantile(value, 0.25, na.rm = TRUE), 4),
    q75 = round(quantile(value, 0.75, na.rm = TRUE), 4),
    min = round(min(value, na.rm = TRUE), 4),
    max = round(max(value, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(descriptives)

# ==============================================================================
# EXPERIMENTAL DESIGN BALANCE
# ==============================================================================

cat("\n=== EXPERIMENTAL DESIGN BALANCE ===\n")

# Trials by condition combinations
design_balance <- df_rm %>%
  mutate(
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(spacing_category)
  ) %>%
  count(RM_condition, correct_num, correct_width, spacing_category) %>%
  arrange(RM_condition, correct_num, correct_width, spacing_category)

print(design_balance)

# Check for missing combinations
cat("\nDesign completeness check:\n")
expected_combinations <- expand_grid(
  RM_condition = c("NoRM", "RM"),
  correct_num = unique(df_rm$correct_num),
  correct_width = unique(df_rm$correct_width),
  spacing_category = c("Smaller", "Middle", "Larger")
) %>%
  nrow()

observed_combinations <- design_balance %>% nrow()

cat("Expected combinations:", expected_combinations, "\n")
cat("Observed combinations:", observed_combinations, "\n")
cat("Design completeness:", round(observed_combinations/expected_combinations * 100, 1), "%\n")

# ==============================================================================
# CORRELATIONS BETWEEN OUTCOME VARIABLES
# ==============================================================================

cat("\n=== CORRELATIONS BETWEEN OUTCOME VARIABLES ===\n")

# Correlation matrix for key variables
correlation_vars <- df_rm %>%
  select(all_of(outcome_vars)) %>%
  select_if(~ !all(is.na(.)))

if (ncol(correlation_vars) > 1) {
  cor_matrix <- cor(correlation_vars, use = "complete.obs")
  cat("Correlation matrix:\n")
  print(round(cor_matrix, 3))
}

# ==============================================================================
# SAVE DESCRIPTIVE TABLES
# ==============================================================================

# Create outputs directory if it doesn't exist
if (!dir.exists("outputs/exp1/tables")) {
  dir.create("outputs/exp1/tables", recursive = TRUE)
}

# Save main descriptive statistics table
write.csv(descriptives, "outputs/exp1/tables/descriptive_statistics_by_condition.csv", row.names = FALSE)

# Save design balance table
write.csv(design_balance, "outputs/exp1/tables/experimental_design_balance.csv", row.names = FALSE)

# Save sample characteristics
sample_summary <- tibble(
  measure = c("Total trials", "Total participants", "Mean trials per participant", 
              "Min trials per participant", "Max trials per participant"),
  value = c(total_trials, total_participants, round(mean(trials_per_participant$n_trials), 1),
            min(trials_per_participant$n_trials), max(trials_per_participant$n_trials))
)

write.csv(sample_summary, "outputs/exp1/tables/sample_characteristics.csv", row.names = FALSE)

# Save RM condition distribution
write.csv(rm_distribution, "outputs/exp1/tables/rm_condition_distribution.csv", row.names = FALSE)

cat("\nDescriptive analysis complete!\n")
cat("Tables saved to outputs/exp1/tables/:\n")
cat("- descriptive_statistics_by_condition.csv\n")
cat("- experimental_design_balance.csv\n") 
cat("- sample_characteristics.csv\n")
cat("- rm_condition_distribution.csv\n")

