# Load necessary libraries
library(tidyverse)

# Read the datasets
processed_data <- read.csv("datasets/processed.csv")
one_bar_data <- read.csv("datasets/one_bar_Exp1ABC.csv")

# Prepare one_bar data for merging
one_bar_data <- one_bar_data %>%
  rename(subID = participant) %>%
  mutate(subID = as.character(subID))

# Ensure consistent data types
processed_data$subID <- as.character(processed_data$subID)

# Combine datasets using left join to preserve all main experiment data
combined_data <- processed_data %>%
  left_join(one_bar_data, 
            by = c("subID", "correct_width", "exp_version"),
            suffix = c("_main", "_onebar"),
            relationship = "many-to-many")


# Save the combined dataset
write.csv(combined_data, "datasets/combined_data.csv", row.names = FALSE)

