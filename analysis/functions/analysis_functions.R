# ==============================================================================
# STATISTICAL ANALYSIS HELPER FUNCTIONS
# ==============================================================================
# Reusable functions for redundancy masking statistical analysis
# ==============================================================================

library(tidyverse)
library(lme4)
library(emmeans)

# ==============================================================================
# DATA PREPARATION FUNCTIONS
# ==============================================================================

#' Prepare data for RM analysis with proper factor coding
#' @param data Raw processed data
#' @param spacing_method Method for creating spacing categories ("quantile" or "fixed")
#' @return Prepared data frame for statistical analysis
prepare_rm_data <- function(data, spacing_method = "quantile") {
  
  # Filter and create basic RM variables
  data_prepared <- data %>%
    filter(number_deviation %in% c(-1, 0)) %>%
    mutate(
      rm_type = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                       levels = c("NoRM", "RM")),
      correct_num = factor(correct_num),
      correct_width = factor(correct_width),
      subID = factor(subID)
    )
  
  # Create spacing categories based on method
  if (spacing_method == "quantile") {
    # Use quantile splits (default for most experiments)
    data_prepared <- data_prepared %>%
      mutate(
        spacing_category = case_when(
          correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
          correct_space <= quantile(correct_space, 2/3) ~ "Middle",
          TRUE ~ "Larger"
        )
      )
  } else if (spacing_method == "fixed") {
    # Use fixed thresholds (for exp1/Exp1B specifically)
    data_prepared <- data_prepared %>%
      mutate(
        spacing_category = case_when(
          correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
          correct_space <= 0.9 ~ "Middle",  # Exp1B-specific threshold
          TRUE ~ "Larger"
        )
      )
  }
  
  # Finalize factor levels
  data_prepared <- data_prepared %>%
    mutate(
      spacing_category = factor(spacing_category, levels = c("Smaller", "Middle", "Larger"))
    )
  
  return(data_prepared)
}

#' Calculate descriptive statistics by groups
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param group_vars Vector of grouping variable names
#' @return Data frame with descriptive statistics
calculate_descriptives <- function(data, outcome_var, group_vars) {
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean = mean(.data[[outcome_var]], na.rm = TRUE),
      sd = sd(.data[[outcome_var]], na.rm = TRUE),
      se = sd / sqrt(n),
      median = median(.data[[outcome_var]], na.rm = TRUE),
      q25 = quantile(.data[[outcome_var]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[outcome_var]], 0.75, na.rm = TRUE),
      ci_lower = mean - 1.96 * se,
      ci_upper = mean + 1.96 * se,
      .groups = "drop"
    )
}

# ==============================================================================
# STATISTICAL MODELING FUNCTIONS
# ==============================================================================

#' Fit full factorial mixed-effects model
#' @param data Prepared data frame
#' @param outcome_var Name of outcome variable  
#' @param include_all_interactions Whether to include all 4-way interactions
#' @return Fitted lmer model object
fit_rm_model <- function(data, outcome_var, include_all_interactions = TRUE) {
  
  if (include_all_interactions) {
    formula_str <- paste(outcome_var, "~ rm_type * spacing_category * correct_num * correct_width + (1 | subID)")
  } else {
    formula_str <- paste(outcome_var, "~ rm_type + spacing_category + correct_num + correct_width + (1 | subID)")
  }
  
  model <- lmer(as.formula(formula_str), data = data, REML = FALSE)
  return(model)
}

#' Extract primary RM contrasts from model
#' @param model Fitted lmer model
#' @return List with emmeans and contrasts
get_primary_rm_effects <- function(model) {
  
  # Get marginal means
  rm_emmeans <- emmeans(model, ~ rm_type)
  
  # Get primary contrast
  rm_contrast <- pairs(rm_emmeans)
  
  # Calculate effect size
  rm_means_summary <- summary(rm_emmeans)
  rm_diff <- diff(rm_means_summary$emmean)
  cohens_d <- rm_diff / sigma(model)
  
  # Test individual conditions against zero
  rm_vs_zero <- test(rm_emmeans, null = 0)
  
  return(list(
    emmeans = rm_emmeans,
    contrast = rm_contrast,
    vs_zero = rm_vs_zero,
    cohens_d = cohens_d,
    means_summary = rm_means_summary
  ))
}

#' Get RM effects by experimental factors
#' @param model Fitted lmer model
#' @param factor_name Name of factor to examine ("spacing_category", "correct_num", "correct_width")
#' @return emmeans contrasts object
get_rm_by_factor <- function(model, factor_name) {
  
  formula_str <- paste("~ rm_type |", factor_name)
  rm_by_factor <- emmeans(model, as.formula(formula_str))
  contrasts <- pairs(rm_by_factor)
  
  return(list(
    emmeans = rm_by_factor,
    contrasts = contrasts
  ))
}

#' Get complex interaction effects
#' @param model Fitted lmer model
#' @param factor1 First factor name
#' @param factor2 Second factor name
#' @return emmeans contrasts object
get_rm_interaction <- function(model, factor1, factor2) {
  
  formula_str <- paste("~ rm_type |", factor1, "*", factor2)
  rm_interaction <- emmeans(model, as.formula(formula_str))
  contrasts <- pairs(rm_interaction)
  
  return(list(
    emmeans = rm_interaction,
    contrasts = contrasts
  ))
}

# ==============================================================================
# RESULTS SUMMARY FUNCTIONS
# ==============================================================================

#' Create comprehensive analysis summary
#' @param model Fitted lmer model
#' @param outcome_name Name of outcome variable for labeling
#' @return List of all analysis results
analyze_rm_outcome <- function(model, outcome_name) {
  
  cat("Analyzing", outcome_name, "...\n")
  
  # Primary effects
  primary <- get_primary_rm_effects(model)
  
  # Factor-specific effects
  by_spacing <- get_rm_by_factor(model, "spacing_category")
  by_num <- get_rm_by_factor(model, "correct_num") 
  by_width <- get_rm_by_factor(model, "correct_width")
  
  # Interaction effects
  spacing_width <- get_rm_interaction(model, "spacing_category", "correct_width")
  num_width <- get_rm_interaction(model, "correct_num", "correct_width")
  
  # Model fit statistics
  fit_stats <- tibble(
    outcome = outcome_name,
    aic = AIC(model),
    bic = BIC(model),
    residual_sd = sigma(model),
    cohens_d = primary$cohens_d
  )
  
  return(list(
    outcome = outcome_name,
    primary = primary,
    by_spacing = by_spacing,
    by_num = by_num,
    by_width = by_width,
    spacing_width_interaction = spacing_width,
    num_width_interaction = num_width,
    fit_stats = fit_stats,
    model = model
  ))
}

#' Save analysis results to CSV files
#' @param results Results list from analyze_rm_outcome
#' @param output_dir Directory to save results
save_analysis_results <- function(results, output_dir = "outputs/tables") {
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  outcome_name <- results$outcome
  
  # Save primary results
  write.csv(summary(results$primary$contrast), 
            file.path(output_dir, paste0(outcome_name, "_primary_contrast.csv")), 
            row.names = FALSE)
  
  write.csv(summary(results$primary$emmeans), 
            file.path(output_dir, paste0(outcome_name, "_emmeans.csv")), 
            row.names = FALSE)
  
  write.csv(summary(results$primary$vs_zero), 
            file.path(output_dir, paste0(outcome_name, "_vs_zero.csv")), 
            row.names = FALSE)
  
  # Save factor-specific results
  write.csv(summary(results$by_spacing$contrasts), 
            file.path(output_dir, paste0(outcome_name, "_by_spacing.csv")), 
            row.names = FALSE)
  
  write.csv(summary(results$by_num$contrasts), 
            file.path(output_dir, paste0(outcome_name, "_by_setsize.csv")), 
            row.names = FALSE)
  
  write.csv(summary(results$by_width$contrasts), 
            file.path(output_dir, paste0(outcome_name, "_by_width.csv")), 
            row.names = FALSE)
  
  # Save interaction results
  write.csv(summary(results$spacing_width_interaction$contrasts), 
            file.path(output_dir, paste0(outcome_name, "_spacing_width_interaction.csv")), 
            row.names = FALSE)
  
  write.csv(summary(results$num_width_interaction$contrasts), 
            file.path(output_dir, paste0(outcome_name, "_num_width_interaction.csv")), 
            row.names = FALSE)
  
  # Save model fit statistics
  write.csv(results$fit_stats, 
            file.path(output_dir, paste0(outcome_name, "_model_fit.csv")), 
            row.names = FALSE)
  
  cat("Results for", outcome_name, "saved to", output_dir, "\n")
}