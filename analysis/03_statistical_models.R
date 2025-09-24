# ==============================================================================
# REDUNDANCY MASKING STUDY - STATISTICAL ANALYSIS
# ==============================================================================
# This script performs comprehensive mixed-effects modeling analysis
# Input: Processed dataset from data/processed.csv
# Output: Statistical results saved to outputs/tables/
# ==============================================================================

library(tidyverse)
library(lme4)
library(emmeans)

cat("Starting statistical analysis for Exp1...\n\n")

# Load processed data
df <- read.csv("data/processed.csv")

# ==============================================================================
# DATA PREPARATION FOR STATISTICAL ANALYSIS
# ==============================================================================

cat("Preparing data for statistical analysis...\n")

# Filter and prepare data for RM analysis
df_analysis <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%  # Focus on RM vs NoRM trials
  mutate(
    # Create spacing categories using exp1 thresholds
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
      correct_space <= 0.9 ~ "Middle",  # Specific exp1 threshold
      TRUE ~ "Larger"
    ),
    # Create properly ordered factors
    rm_type = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                     levels = c("NoRM", "RM")),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(spacing_category, levels = c("Smaller", "Middle", "Larger")),
    subID = factor(subID)
  )

# Print analysis sample info
cat("Analysis sample:\n")
cat("- Total trials:", nrow(df_analysis), "\n")
cat("- Participants:", length(unique(df_analysis$subID)), "\n")
cat("- RM trials:", sum(df_analysis$rm_type == "RM"), "\n")
cat("- NoRM trials:", sum(df_analysis$rm_type == "NoRM"), "\n\n")

# ==============================================================================
# PRIMARY ANALYSIS: WIDTH DEVIATION
# ==============================================================================

cat("=== PRIMARY ANALYSIS: WIDTH DEVIATION ===\n")

# Fit full factorial mixed-effects model
cat("Fitting full factorial model...\n")
model_width <- lmer(width_deviation ~ rm_type * spacing_category * correct_num * correct_width +
                    (1 | subID), 
                    data = df_analysis, REML = FALSE)

# Model summary
cat("\nModel summary:\n")
print(summary(model_width))

# ==============================================================================
# PRIMARY RESEARCH QUESTION: RM vs NoRM
# ==============================================================================

cat("\n--- PRIMARY RESEARCH QUESTION: RM vs NoRM ---\n")

# Marginal means for RM conditions
rm_emmeans <- emmeans(model_width, ~ rm_type)
print("Marginal means by RM condition:")
print(rm_emmeans)

# Primary contrast: RM vs NoRM
rm_contrast <- pairs(rm_emmeans)
print("\nPrimary contrast (RM vs NoRM):")
print(rm_contrast)

# Effect size calculation
rm_means_summary <- summary(rm_emmeans)
rm_diff <- diff(rm_means_summary$emmean)
pooled_sd <- sigma(model_width)
cohens_d <- rm_diff / pooled_sd

cat("\nEffect size (Cohen's d):", round(cohens_d, 3), "\n")

# ==============================================================================
# INDIVIDUAL CONDITION TESTS AGAINST ZERO
# ==============================================================================

cat("\n--- INDIVIDUAL CONDITIONS vs ZERO BASELINE ---\n")

# Test each condition against zero
rm_vs_zero <- test(rm_emmeans, null = 0)
print("Individual condition tests against zero:")
print(rm_vs_zero)

# ==============================================================================
# RM EFFECTS BY EXPERIMENTAL FACTORS
# ==============================================================================

cat("\n=== RM EFFECTS BY EXPERIMENTAL FACTORS ===\n")

# RM effects by spacing category
cat("RM effects by spacing category:\n")
rm_by_spacing <- emmeans(model_width, ~ rm_type | spacing_category)
rm_spacing_contrasts <- pairs(rm_by_spacing)
print(rm_spacing_contrasts)

# RM effects by set size
cat("\nRM effects by set size:\n")
rm_by_num <- emmeans(model_width, ~ rm_type | correct_num)
rm_num_contrasts <- pairs(rm_by_num)
print(rm_num_contrasts)

# RM effects by bar width
cat("\nRM effects by bar width:\n")
rm_by_width <- emmeans(model_width, ~ rm_type | correct_width)
rm_width_contrasts <- pairs(rm_by_width)
print(rm_width_contrasts)

# ==============================================================================
# COMPLEX INTERACTION ANALYSES
# ==============================================================================

cat("\n=== COMPLEX INTERACTION ANALYSES ===\n")

# RM effects by spacing × width interaction
cat("RM effects by spacing category × bar width:\n")
rm_spacing_width <- emmeans(model_width, ~ rm_type | spacing_category * correct_width)
rm_spacing_width_contrasts <- pairs(rm_spacing_width)
print(rm_spacing_width_contrasts)

# RM effects by set size × width interaction
cat("\nRM effects by set size × bar width:\n")
rm_num_width <- emmeans(model_width, ~ rm_type | correct_num * correct_width)
rm_num_width_contrasts <- pairs(rm_num_width)
print(rm_num_width_contrasts)

# ==============================================================================
# ADDITIONAL OUTCOME VARIABLES (if needed)
# ==============================================================================

cat("\n=== ADDITIONAL OUTCOME ANALYSES ===\n")

# Function to analyze additional outcomes
analyze_outcome <- function(outcome_var, data) {
  cat("\nAnalyzing", outcome_var, "...\n")
  
  # Fit model
  formula_str <- paste(outcome_var, "~ rm_type * spacing_category * correct_num * correct_width + (1 | subID)")
  model <- lmer(as.formula(formula_str), data = data, REML = FALSE)
  
  # Primary RM contrast
  rm_emmeans <- emmeans(model, ~ rm_type)
  rm_contrast <- pairs(rm_emmeans)
  
  # Return results
  list(
    emmeans = summary(rm_emmeans),
    contrast = summary(rm_contrast),
    model_summary = summary(model)
  )
}

# Analyze key secondary outcomes
secondary_outcomes <- c("spacing_deviation", "pooled_width_deviation", "width_density_deviation")

secondary_results <- map(secondary_outcomes, ~ analyze_outcome(.x, df_analysis))
names(secondary_results) <- secondary_outcomes

# Print secondary outcome results
for (outcome in secondary_outcomes) {
  cat("\n--- ", toupper(outcome), " ---\n")
  print(secondary_results[[outcome]]$contrast)
}

# ==============================================================================
# SAVE STATISTICAL RESULTS
# ==============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Create outputs directory
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

# Save primary analysis results
write.csv(summary(rm_contrast), "outputs/tables/primary_rm_contrast_width.csv", row.names = FALSE)
write.csv(summary(rm_emmeans), "outputs/tables/primary_rm_emmeans_width.csv", row.names = FALSE)
write.csv(summary(rm_vs_zero), "outputs/tables/conditions_vs_zero_width.csv", row.names = FALSE)

# Save factor-specific results
write.csv(summary(rm_spacing_contrasts), "outputs/tables/rm_by_spacing_width.csv", row.names = FALSE)
write.csv(summary(rm_num_contrasts), "outputs/tables/rm_by_setsize_width.csv", row.names = FALSE)
write.csv(summary(rm_width_contrasts), "outputs/tables/rm_by_barwidth_width.csv", row.names = FALSE)

# Save interaction results
write.csv(summary(rm_spacing_width_contrasts), "outputs/tables/rm_spacing_x_width_interaction.csv", row.names = FALSE)
write.csv(summary(rm_num_width_contrasts), "outputs/tables/rm_setsize_x_width_interaction.csv", row.names = FALSE)

# Save secondary outcome results
for (outcome in secondary_outcomes) {
  filename <- paste0("outputs/tables/rm_contrast_", outcome, ".csv")
  write.csv(secondary_results[[outcome]]$contrast, filename, row.names = FALSE)
}

# Save model coefficients and fit statistics
model_summary_df <- tibble(
  parameter = c("Effect_size_cohens_d", "Model_AIC", "Model_BIC", "Residual_SD"),
  value = c(cohens_d, AIC(model_width), BIC(model_width), sigma(model_width))
)
write.csv(model_summary_df, "outputs/tables/model_fit_statistics.csv", row.names = FALSE)

cat("\nStatistical analysis complete!\n")
cat("Results saved to outputs/tables/:\n")
cat("- Primary RM contrast: primary_rm_contrast_width.csv\n")
cat("- RM marginal means: primary_rm_emmeans_width.csv\n")
cat("- Conditions vs zero: conditions_vs_zero_width.csv\n")
cat("- Factor-specific effects: rm_by_[factor]_width.csv\n")
cat("- Interaction effects: rm_[factor1]_x_[factor2]_interaction.csv\n")
cat("- Secondary outcomes: rm_contrast_[outcome].csv\n")
cat("- Model fit statistics: model_fit_statistics.csv\n")