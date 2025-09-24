# ==============================================================================
# PLOTTING HELPER FUNCTIONS
# ==============================================================================
# Reusable functions for creating redundancy masking visualizations
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(scales)

# Source theme functions
source("analysis/functions/themes.R")

# ==============================================================================
# CORE PLOTTING FUNCTIONS
# ==============================================================================

#' Create RM probability plot by experimental conditions
#' @param data Prepared RM data
#' @param facet_by Variable to facet by (default: "spacing_category")
#' @param include_individuals Whether to show individual participant points
#' @return ggplot object
plot_rm_probability <- function(data, facet_by = "spacing_category", include_individuals = TRUE) {
  
  # Calculate group-level probabilities
  prob_data <- data %>%
    group_by(correct_num, correct_width, .data[[facet_by]]) %>%
    summarise(
      rm_rate = mean(rm_type == "RM"),
      n_trials = n(),
      se = sqrt(rm_rate * (1 - rm_rate) / n_trials),
      ci_lower = pmax(0, rm_rate - 1.96 * se),
      ci_upper = pmin(1, rm_rate + 1.96 * se),
      set_size_num = as.numeric(as.character(correct_num)),
      .groups = "drop"
    )
  
  # Base plot
  p <- ggplot(prob_data, aes(x = set_size_num, y = rm_rate, color = correct_width))
  
  # Add individual points if requested
  if (include_individuals) {
    individual_data <- data %>%
      group_by(subID, correct_num, correct_width, .data[[facet_by]]) %>%
      summarise(rm_rate = mean(rm_type == "RM"), .groups = "drop") %>%
      mutate(set_size_num = as.numeric(as.character(correct_num)))
    
    p <- p +
      geom_point(data = individual_data, 
                 aes(x = set_size_num, y = rm_rate, color = correct_width),
                 size = 0.8, alpha = 0.3, position = position_jitter(width = 0.05, height = 0))
  }
  
  # Add main plot elements
  p <- p +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.1, size = 1, alpha = 0.8) +
    geom_point(size = 4, alpha = 0.9) +
    geom_line(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = get_rm_colors("width_conditions"), name = "Bar Width (°)") +
    scale_x_continuous(breaks = unique(prob_data$set_size_num)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), 
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "Redundancy Masking Probability by Experimental Conditions",
      x = "Set Size (Number of Bars)",
      y = "RM Probability"
    ) +
    theme_scientific()
  
  # Add faceting if specified
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_by)), labeller = label_both)
  }
  
  return(p)
}

#' Create primary RM effects bar plot
#' @param data Prepared RM data
#' @param outcome_var Name of outcome variable
#' @param add_zero_ref Whether to add reference line at zero
#' @return ggplot object
plot_primary_rm_effects <- function(data, outcome_var, add_zero_ref = TRUE) {
  
  # Calculate means and confidence intervals
  plot_data <- data %>%
    group_by(rm_type) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
      se = sd(.data[[outcome_var]], na.rm = TRUE) / sqrt(n),
      ci_lower = mean_outcome - 1.96 * se,
      ci_upper = mean_outcome + 1.96 * se,
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = rm_type, y = mean_outcome, fill = rm_type))
  
  # Add zero reference line if requested
  if (add_zero_ref) {
    p <- p + add_zero_line()
  }
  
  p <- p +
    geom_col(alpha = 0.8, color = "white", size = 1, width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "black", alpha = 0.8, size = 1) +
    scale_fill_manual(values = get_rm_colors("primary")) +
    labs(
      title = "Primary RM Effect",
      subtitle = paste("Effect on", format_axis_label(outcome_var)),
      x = "Condition",
      y = format_axis_label(outcome_var)
    ) +
    theme_scientific() +
    theme(legend.position = "none")
  
  return(p)
}

#' Create RM effects by factor interaction plot
#' @param data Prepared RM data
#' @param outcome_var Name of outcome variable
#' @param x_var Variable for x-axis
#' @param facet_var Variable for faceting (optional)
#' @param add_zero_ref Whether to add reference line at zero
#' @return ggplot object
plot_rm_by_factor <- function(data, outcome_var, x_var, facet_var = NULL, add_zero_ref = TRUE) {
  
  # Calculate means and confidence intervals
  group_vars <- c("rm_type", x_var)
  if (!is.null(facet_var)) {
    group_vars <- c(group_vars, facet_var)
  }
  
  plot_data <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
      se = sd(.data[[outcome_var]], na.rm = TRUE) / sqrt(n),
      ci_lower = mean_outcome - 1.96 * se,
      ci_upper = mean_outcome + 1.96 * se,
      .groups = "drop"
    )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = .data[[x_var]], y = mean_outcome, 
                            color = rm_type, group = rm_type))
  
  # Add zero reference line if requested
  if (add_zero_ref) {
    p <- p + add_zero_line()
  }
  
  p <- p +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.1, size = 0.8, alpha = 0.8,
                  position = position_dodge(width = 0.3)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.3)) +
    geom_line(size = 1, alpha = 0.8, position = position_dodge(width = 0.3)) +
    scale_color_manual(values = get_rm_colors("primary"), name = "Condition") +
    labs(
      title = paste("RM Effects by", str_to_title(str_replace_all(x_var, "_", " "))),
      x = format_axis_label(x_var),
      y = format_axis_label(outcome_var)
    ) +
    theme_scientific()
  
  # Add faceting if specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), labeller = label_both)
  }
  
  return(p)
}

#' Create multiple outcomes comparison plot
#' @param data Prepared RM data
#' @param outcome_vars Vector of outcome variable names
#' @param add_zero_ref Whether to add reference line at zero
#' @return ggplot object
plot_multiple_outcomes <- function(data, outcome_vars, add_zero_ref = TRUE) {
  
  # Reshape data for multiple outcomes
  plot_data <- data %>%
    select(rm_type, all_of(outcome_vars)) %>%
    pivot_longer(cols = all_of(outcome_vars), names_to = "outcome", values_to = "value") %>%
    group_by(rm_type, outcome) %>%
    summarise(
      n = sum(!is.na(value)),
      mean_value = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n),
      ci_lower = mean_value - 1.96 * se,
      ci_upper = mean_value + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(
      outcome_label = map_chr(outcome, format_axis_label),
      outcome_label = factor(outcome_label)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = outcome_label, y = mean_value, fill = rm_type))
  
  # Add zero reference line if requested
  if (add_zero_ref) {
    p <- p + add_zero_line()
  }
  
  p <- p +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8, color = "white", size = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.8), width = 0.3, 
                  color = "black", alpha = 0.8, size = 0.8) +
    scale_fill_manual(values = get_rm_colors("primary"), name = "Condition") +
    labs(
      title = "RM Effects Across Multiple Outcome Variables",
      x = "Outcome Variable",
      y = "Deviation from Correct Value"
    ) +
    theme_scientific() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Create relative deviations plot
#' @param data Prepared RM data
#' @param relative_vars Vector of relative deviation variable names
#' @return ggplot object
plot_relative_deviations <- function(data, relative_vars) {
  
  # Calculate relative deviations
  plot_data <- data %>%
    select(rm_type, all_of(relative_vars)) %>%
    pivot_longer(cols = all_of(relative_vars), names_to = "outcome", values_to = "value") %>%
    filter(!is.na(value), is.finite(value)) %>%
    group_by(rm_type, outcome) %>%
    summarise(
      n = sum(!is.na(value)),
      mean_value = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n),
      ci_lower = mean_value - 1.96 * se,
      ci_upper = mean_value + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(
      outcome_label = map_chr(outcome, format_axis_label)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = outcome_label, y = mean_value, fill = rm_type)) +
    add_zero_line() +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8, color = "white", size = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.8), width = 0.3, 
                  color = "black", alpha = 0.8, size = 0.8) +
    scale_fill_manual(values = get_rm_colors("primary"), name = "Condition") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Relative Deviation Analysis", 
      subtitle = "Normalized deviations as proportion of correct values",
      x = "Relative Deviation Type",
      y = "Relative Deviation (%)"
    ) +
    theme_scientific()
  
  return(p)
}

# ==============================================================================
# SPECIALIZED PLOTTING FUNCTIONS
# ==============================================================================

#' Create comprehensive figure combining multiple plots
#' @param data Prepared RM data
#' @param primary_outcome Primary outcome variable for main analysis
#' @param secondary_outcomes Vector of secondary outcome variables
#' @return Combined patchwork plot
create_comprehensive_figure <- function(data, primary_outcome = "width_deviation", 
                                      secondary_outcomes = c("spacing_deviation", "pooled_width_deviation")) {
  
  # Individual plots
  fig1 <- plot_rm_probability(data)
  fig2 <- plot_primary_rm_effects(data, primary_outcome)  
  fig3 <- plot_rm_by_factor(data, primary_outcome, "correct_width", "correct_num")
  fig4 <- plot_multiple_outcomes(data, c(primary_outcome, secondary_outcomes))
  
  # Combine using patchwork
  combined <- (fig1 / (fig2 | fig3)) / fig4
  
  return(combined)
}

#' Create publication-ready figure with consistent styling
#' @param plot_list List of ggplot objects
#' @param layout Layout specification for patchwork
#' @param title Main title for combined figure
#' @return Combined patchwork plot with consistent styling
create_publication_figure <- function(plot_list, layout = NULL, title = NULL) {
  
  # Apply consistent theme to all plots
  styled_plots <- map(plot_list, ~ .x + theme_scientific())
  
  # Combine plots
  if (is.null(layout)) {
    combined <- wrap_plots(styled_plots)
  } else {
    combined <- wrap_plots(styled_plots, design = layout)
  }
  
  # Add overall title if provided
  if (!is.null(title)) {
    combined <- combined + 
      plot_annotation(
        title = title,
        theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      )
  }
  
  return(combined)
}