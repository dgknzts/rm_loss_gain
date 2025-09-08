# Baseline Correction Comparison - Simplified
# Side-by-side comparison of original vs baseline-corrected width deviations

library(tidyverse)
library(patchwork)

# Load datasets
one_bar_data <- read.csv("datasets/one_bar_Exp1ABC.csv")
main_data <- read.csv("datasets/processed.csv")

# Calculate individual baseline biases
baseline_biases <- one_bar_data %>%
  group_by(participant, exp_version, correct_width) %>%
  summarise(baseline_bias = mean(width_deviation, na.rm = TRUE), .groups = "drop") %>%
  rename(subID = participant)

# Prepare main data and apply baseline correction
corrected_data <- main_data %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    RM_condition = ifelse(number_deviation == -1, "RM", "no-RM"),
    RM_condition = factor(RM_condition, levels = c("no-RM", "RM"))
  ) %>%
  left_join(baseline_biases, by = c("subID", "exp_version", "correct_width")) %>%
  mutate(
    baseline_corrected_width_deviation = ifelse(
      !is.na(baseline_bias),
      width_deviation - baseline_bias,
      NA_real_
    )
  ) %>%
  filter(!is.na(baseline_corrected_width_deviation))  # Keep only corrected data

# Custom theme
theme_clean <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )

# Function to create comparison plot for one experiment
create_experiment_comparison <- function(exp_name) {
  
  # Filter data for this experiment
  exp_data <- corrected_data %>% filter(exp_version == exp_name)
  
  if(nrow(exp_data) == 0) return(NULL)
  
  # Calculate summary statistics for original width deviation
  original_summary <- exp_data %>%
    group_by(RM_condition, correct_width, correct_num) %>%
    summarise(
      mean_dev = mean(width_deviation, na.rm = TRUE),
      se_dev = sd(width_deviation, na.rm = TRUE) / sqrt(sum(!is.na(width_deviation))),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = mean_dev - 1.96 * se_dev,
      ci_upper = mean_dev + 1.96 * se_dev
    )
  
  # Calculate summary statistics for baseline-corrected width deviation  
  corrected_summary <- exp_data %>%
    group_by(RM_condition, correct_width, correct_num) %>%
    summarise(
      mean_dev = mean(baseline_corrected_width_deviation, na.rm = TRUE),
      se_dev = sd(baseline_corrected_width_deviation, na.rm = TRUE) / sqrt(sum(!is.na(baseline_corrected_width_deviation))),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = mean_dev - 1.96 * se_dev,
      ci_upper = mean_dev + 1.96 * se_dev
    )
  
  # Original width deviation plot
  p1 <- ggplot(original_summary, aes(x = correct_width, y = mean_dev, 
                                    color = RM_condition, group = RM_condition)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.02, size = 0.8, alpha = 0.8,
                  position = position_dodge(width = 0.03)) +
    geom_line(size = 1, alpha = 0.8, position = position_dodge(width = 0.03)) +
    geom_point(size = 3, position = position_dodge(width = 0.03)) +
    facet_wrap(~correct_num, labeller = label_both) +
    scale_color_manual(name = "RM Condition", 
                       values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
    labs(
      title = "Original Width Deviation",
      x = "Correct Width (degrees)",
      y = "Width Deviation (degrees)"
    ) +
    theme_clean
  
  # Baseline-corrected width deviation plot
  p2 <- ggplot(corrected_summary, aes(x = correct_width, y = mean_dev, 
                                     color = RM_condition, group = RM_condition)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.02, size = 0.8, alpha = 0.8,
                  position = position_dodge(width = 0.03)) +
    geom_line(size = 1, alpha = 0.8, position = position_dodge(width = 0.03)) +
    geom_point(size = 3, position = position_dodge(width = 0.03)) +
    facet_wrap(~correct_num, labeller = label_both) +
    scale_color_manual(name = "RM Condition", 
                       values = c("no-RM" = "#3498db", "RM" = "#e74c3c")) +
    labs(
      title = "Baseline-Corrected Width Deviation",
      x = "Correct Width (degrees)",
      y = "Width Deviation (degrees)"
    ) +
    theme_clean
  
  # Combine plots using patchwork
  combined_plot <- (p1 | p2) + 
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste("Width Deviation Comparison:", exp_name),
      subtitle = "Original vs Baseline-Corrected Measures (95% CI)",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
      )
    ) &
    theme(legend.position = "bottom")
  
  return(combined_plot)
}

# Create and save plots for each experiment
experiments <- c("Exp1A", "Exp1B", "Exp1C")

for(exp in experiments) {
  cat("Creating plot for", exp, "...\n")
  
  plot <- create_experiment_comparison(exp)
  
  if(!is.null(plot)) {
    filename <- paste0("figures/", tolower(exp), "_baseline_comparison.png")
    
    ggsave(filename, plot,
           width = 14, height = 8, dpi = 300, bg = "white")
    
    cat("Saved:", filename, "\n")
    print(plot)
  } else {
    cat("No data available for", exp, "\n")
    
  }
}

cat("\nComparison plots complete!\n")