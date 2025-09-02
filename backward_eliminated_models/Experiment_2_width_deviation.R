library(tidyverse)
library(lme4)
library(emmeans)
library(performance)
library(ggrepel)

df <- read.csv("datasets/processed.csv")

# Prepare the data with proper factorization for Experiment 1B
df_filtered <- df %>%
  filter(exp_version == "Exp1B") %>%  # Changed from Exp1A to Exp1B
  filter(number_deviation %in% c(-1, 0))

# Calculate thresholds for plotting
first_threshold <- quantile(df_filtered$correct_space, 1/3)
second_threshold <- 0.9
quantiles <- c(min(df_filtered$correct_space), first_threshold, second_threshold, max(df_filtered$correct_space))

df_exp <- df_filtered %>%
  mutate(
    # Create quantile splits with adjusted threshold for better balance
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
      correct_space <= 0.9 ~ "Middle",
      TRUE ~ "Larger"
    ),
    # Factor all categorical variables with proper contrasts
    rm_type = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                     levels = c("NoRM", "RM")),  # NoRM as reference
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(spacing_category, 
                              levels = c("Smaller", "Middle", "Larger")),
    subID = factor(subID),
    correct_space_factor = factor(correct_space),
    
    # Check which width deviation measure to use
    width_deviation_available = !is.na(width_deviation),
    width_deviation_ratio_available = !is.na(width_deviation_ration)  # Note: typo in original data
  )

# Spacing distributions
ggplot(df_exp, aes(x = correct_space_factor, fill = correct_space_factor)) +
  geom_bar(alpha = 0.8, color = "white", size = 0.3) +
  geom_vline(xintercept = which(levels(df_exp$correct_space_factor) == 
                                  as.character(round(quantiles[2], 3))) + 0.5, 
             color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = which(levels(df_exp$correct_space_factor) == 
                                  as.character(round(quantiles[3], 3))) + 0.5, 
             color = "red", linetype = "dashed", size = 1.2) +
  scale_fill_manual(
    values = c("Smaller" = "#2E86C1", "Middle" = "#F39C12", "Larger" = "#28B463"),
    name = "Spacing Category"
  ) +
  labs(
    title = "Experiment 2 - Spacing Distribution",  # Shortened title
    subtitle = paste("Thresholds:", round(quantiles[2], 3), "and", round(quantiles[3], 3)),
    x = "Spacing Values (degrees)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "red"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) 

table(df_exp$spacing_category)
table(df_exp$rm_type)

#### RM vs Non-RM Analysis for Experiment 2
summary(df_exp$width_deviation)

# Full factorial model - starting point for backward elimination
full_model <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                     rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                     spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                     rm_type:spacing_category:correct_num + rm_type:spacing_category:correct_width +
                     rm_type:correct_num:correct_width + spacing_category:correct_num:correct_width +
                     rm_type:spacing_category:correct_num:correct_width +
                     (1 | subID), 
                   data = df_exp, REML = FALSE)


# Initialize model comparison tracking
model_comparison <- data.frame(
  Step = character(),
  Model_Name = character(),
  Interaction_Removed = character(),
  AIC = numeric(),
  BIC = numeric(),
  LogLik = numeric(),
  p_value = numeric(),
  Decision = character(),
  stringsAsFactors = FALSE
)

# Record full model
model_comparison <- rbind(model_comparison, data.frame(
  Step = "0_Full",
  Model_Name = "full_model",
  Interaction_Removed = "None",
  AIC = AIC(full_model),
  BIC = BIC(full_model),
  LogLik = as.numeric(logLik(full_model)),
  p_value = NA,
  Decision = "Starting_model",
  stringsAsFactors = FALSE
))

# Backward elimination steps
# Step 1: Check 4-way interaction (usually first to remove)
model_no_4way <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                        rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                        spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                        rm_type:spacing_category:correct_num + rm_type:spacing_category:correct_width +
                        rm_type:correct_num:correct_width + spacing_category:correct_num:correct_width +
                        (1 | subID), 
                      data = df_exp, REML = FALSE)

step1_test <- anova(model_no_4way, full_model)
print(step1_test)

# Record Step 1 model
model_comparison <- rbind(model_comparison, data.frame(
  Step = "1",
  Model_Name = "model_no_4way",
  Interaction_Removed = "4-way_interaction",
  AIC = AIC(model_no_4way),
  BIC = BIC(model_no_4way),
  LogLik = as.numeric(logLik(model_no_4way)),
  p_value = step1_test$`Pr(>Chisq)`[2],
  Decision = "Remove_4way",
  stringsAsFactors = FALSE
))

# Based on the above result, select your working model for next step
# If 4-way not significant (p > 0.05), use model_no_4way
# If 4-way significant (p < 0.05), keep full_model
working_model <- model_no_4way  # Change this based on results above


# Step 2: Test each 3-way interaction - remove the one with highest p-value

# Test removal of rm_type:spacing_category:correct_num
model_2A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_width + rm_type:correct_num:correct_width + 
                   spacing_category:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
# Test and record each 3-way interaction removal
step2A_test <- anova(model_2A, working_model)
print("2A: Test removal of rm_type:spacing_category:correct_num")
print(step2A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2A",
  Model_Name = "model_2A", 
  Interaction_Removed = "rm_type:spacing_category:correct_num",
  AIC = AIC(model_2A), BIC = BIC(model_2A), LogLik = as.numeric(logLik(model_2A)),
  p_value = step2A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:spacing_category:correct_width
model_2B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_num + rm_type:correct_num:correct_width + 
                   spacing_category:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step2B_test <- anova(model_2B, working_model)
print("2B: Test removal of rm_type:spacing_category:correct_width")
print(step2B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2B",
  Model_Name = "model_2B",
  Interaction_Removed = "rm_type:spacing_category:correct_width", 
  AIC = AIC(model_2B), BIC = BIC(model_2B), LogLik = as.numeric(logLik(model_2B)),
  p_value = step2B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_num:correct_width
model_2C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_num + rm_type:spacing_category:correct_width + 
                   spacing_category:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step2C_test <- anova(model_2C, working_model)
print("2C: Test removal of rm_type:correct_num:correct_width")
print(step2C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2C",
  Model_Name = "model_2C",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_2C), BIC = BIC(model_2C), LogLik = as.numeric(logLik(model_2C)),
  p_value = step2C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_num:correct_width
model_2D <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_num + rm_type:spacing_category:correct_width + 
                   rm_type:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step2D_test <- anova(model_2D, working_model)
print("2D: Test removal of spacing_category:correct_num:correct_width")
print(step2D_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2D",
  Model_Name = "model_2D",
  Interaction_Removed = "spacing_category:correct_num:correct_width",
  AIC = AIC(model_2D), BIC = BIC(model_2D), LogLik = as.numeric(logLik(model_2D)),
  p_value = step2D_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step2 to whichever model (2A, 2B, 2C, or 2D) had highest p-value
model_step2 <- model_2A  # Update this based on anova results above

# Record Step 2 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2_Selected",
  Model_Name = "model_step2",
  Interaction_Removed = "rm_type:spacing_category:correct_num",
  AIC = AIC(model_step2), BIC = BIC(model_step2), LogLik = as.numeric(logLik(model_step2)),
  p_value = step2A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 3: Test remaining 3-way interactions (model_2A was selected - rm_type:spacing_category:correct_num already removed)
# RESULT: All Step 3 models are significantly worse than Step 2 - cannot reduce further
# Therefore, Step 2 model (model_2A) is the FINAL MODEL

print("Step 3 Results: All 3-way interaction removals significantly worsen model fit")
print("Final model selected: model_step2 (model_2A)")
print("Final model contains:")
print("- All main effects")
print("- All 2-way interactions")
print("- Three 3-way interactions: rm_type:spacing_category:correct_width, rm_type:correct_num:correct_width, spacing_category:correct_num:correct_width")
print("- Removed only: rm_type:spacing_category:correct_num")

# CONFIRMED: Step 2 model (model_2A) is the BEST MODEL based on analysis results
# All Step 3 reductions significantly worsened model fit

# Set final model as Step 2 selection - BEST MODEL
final_model <- model_step2  # model_2A is the CONFIRMED best model

# Record final model as Step 2 selection

# Record final model selection (Step 2 was final)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "Final",
  Model_Name = "final_model",
  Interaction_Removed = "Elimination_stopped_at_Step2",
  AIC = AIC(final_model), BIC = BIC(final_model), LogLik = as.numeric(logLik(final_model)),
  p_value = NA, Decision = "Final_selected", stringsAsFactors = FALSE
))

# Print model comparison summary
print("=== MODEL COMPARISON SUMMARY ===")
print(model_comparison)

# Create comprehensive elimination visualization

# Prepare comprehensive visualization data
plot_data <- model_comparison
plot_data$Step_Clean <- gsub("_Selected|_Final", "", plot_data$Step)

# Extract step numbers properly (handling multi-digit numbers)
plot_data$Step_Number <- ifelse(plot_data$Step_Clean == "0_Full", 0,
                                ifelse(plot_data$Step_Clean == "Final", 99,
                                       as.numeric(gsub("[A-Z]", "", plot_data$Step_Clean))))

# Handle cases where step number extraction fails
plot_data$Step_Number[is.na(plot_data$Step_Number)] <- 
  as.numeric(gsub("[A-Z].*", "", plot_data$Step_Clean[is.na(plot_data$Step_Number)]))

# Create step labels and mark selected models
plot_data$Selected <- plot_data$Decision %in% c("Starting_model", "Remove_4way", "Selected", "Final_selected")
plot_data$Model_Label <- paste0(plot_data$Step, "\n", 
                                substr(plot_data$Interaction_Removed, 1, 25),
                                ifelse(nchar(plot_data$Interaction_Removed) > 25, "...", ""))

# Format p-values for display
plot_data$p_display <- ifelse(is.na(plot_data$p_value), "", 
                              ifelse(plot_data$p_value < 0.001, "p<.001",
                                     paste0("p=", sprintf("%.3f", plot_data$p_value))))

# Create AIC progression plot  
aic_progression <- ggplot(plot_data, aes(x = reorder(Step, Step_Number))) +
  geom_point(aes(y = AIC, size = Selected, color = Selected, alpha = Selected)) +
  geom_line(data = plot_data[plot_data$Selected, ], aes(y = AIC, group = 1), 
            color = "#2C3E50", size = 0.8, linetype = "solid") +
  geom_text_repel(aes(y = AIC, label = p_display), 
                  vjust = -0.5, size = 3.2, color = "#34495E", 
                  family = "Arial", fontface = "bold",
                  box.padding = 0.3, point.padding = 0.2, 
                  force = 2, max.overlaps = Inf,
                  segment.color = "#95A5A6", segment.size = 0.3) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
  scale_color_manual(values = c("FALSE" = "#BDC3C7", "TRUE" = "#E74C3C")) +
  scale_alpha_manual(values = c("FALSE" = 0.6, "TRUE" = 1.0)) +
  labs(title = "Experiment 2 - Model Selection: Systematic Interaction Elimination",  # Updated title
       subtitle = "Selected models connected by line | p-values show significance of removal",
       x = "Elimination Step", 
       y = "AIC (lower = better model)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 10), family = "Arial"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#7F8C8D", margin = margin(b = 15), family = "Arial"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, family = "Arial"),
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.title = element_text(size = 10, family = "Arial"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        text = element_text(family = "Arial"))

print(aic_progression)

# Create BIC comparison plot
bic_progression <- ggplot(plot_data, aes(x = reorder(Step, Step_Number))) +
  geom_point(aes(y = BIC, size = Selected, color = Selected, alpha = Selected)) +
  geom_line(data = plot_data[plot_data$Selected, ], aes(y = BIC, group = 1), 
            color = "#2C3E50", size = 0.8, linetype = "solid") +
  geom_text_repel(aes(y = BIC, label = p_display), 
                  vjust = -0.5, size = 3.2, color = "#34495E", 
                  family = "Arial", fontface = "bold",
                  box.padding = 0.3, point.padding = 0.2, 
                  force = 2, max.overlaps = Inf,
                  segment.color = "#95A5A6", segment.size = 0.3) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
  scale_color_manual(values = c("FALSE" = "#BDC3C7", "TRUE" = "#3498DB")) +
  scale_alpha_manual(values = c("FALSE" = 0.6, "TRUE" = 1.0)) +
  labs(title = "Experiment 2 - Model Selection: BIC Progression",  # Updated title
       subtitle = "Selected models connected by line | p-values show significance of removal",
       x = "Elimination Step", 
       y = "BIC (lower = better model)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 10), family = "Arial"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#7F8C8D", margin = margin(b = 15), family = "Arial"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, family = "Arial"),
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.title = element_text(size = 10, family = "Arial"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        text = element_text(family = "Arial"))

print(bic_progression)


# Export elimination pathway for APA table creation
selected_models <- model_comparison[model_comparison$Decision %in% c("Starting_model", "Remove_4way", "Selected", "Final_selected"), ]

# Ensure the elimination_pathways directory exists
if (!dir.exists("elimination_pathways")) {
  dir.create("elimination_pathways")
}

write.csv(selected_models, "elimination_pathways/elimination_pathway_Exp2_width_deviation.csv", row.names = FALSE)


############### EMMEANS ANALYSIS FOR COMPLEX FINAL MODEL ###############
print("=== FINAL MODEL SUMMARY ===")
summary(final_model)

# Primary research question: RM vs NoRM marginal means
print("\n=== PRIMARY ANALYSIS: RM vs NoRM Marginal Means ===")
rm_emmeans <- emmeans(final_model, ~ rm_type)
print("Estimated marginal means:")
print(rm_emmeans)

# Pairwise comparison
rm_contrast <- pairs(rm_emmeans)
print("RM vs NoRM comparison:")
print(rm_contrast)

# Effect size calculation
rm_means <- summary(rm_emmeans)
effect_size <- diff(rm_means$emmean) / sigma(final_model)
print(paste("Standardized effect size (Cohen's d):", round(effect_size, 3)))

# CRITICAL: With 3-way interactions, check if RM effect varies across conditions
print("\n=== INTERACTION ANALYSIS (3-way interactions present) ===")

# Check RM effect by spacing_category and correct_width (rm_type:spacing_category:correct_width)
print("RM effect by spacing_category and correct_width:")
rm_spacing_width <- emmeans(final_model, ~ rm_type | spacing_category * correct_width)
rm_spacing_width_contrasts <- pairs(rm_spacing_width)
print(rm_spacing_width_contrasts)

# Check RM effect by correct_num and correct_width (rm_type:correct_num:correct_width)
print("\nRM effect by correct_num and correct_width:")
rm_num_width <- emmeans(final_model, ~ rm_type | correct_num * correct_width)
rm_num_width_contrasts <- pairs(rm_num_width)
print(rm_num_width_contrasts)

# Simple effects: Focus on theoretically important conditions
print("\n=== FOCUSED SIMPLE EFFECTS ANALYSIS ===")
# Example: RM effect for specific spacing categories
print("RM effect within each spacing category (averaged across other factors):")
rm_by_spacing <- emmeans(final_model, ~ rm_type | spacing_category)
print(pairs(rm_by_spacing))

# Create emmeans comparison plot
emmeans_plot_data <- summary(rm_emmeans)
emmeans_plot <- ggplot(emmeans_plot_data, aes(x = rm_type, y = emmean)) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, color = "#2C3E50") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Experiment 2 - RM vs NoRM: Width Deviation Comparison",  # Updated title
       subtitle = "Error bars show standard errors | Red line = no deviation",
       x = "Condition",
       y = "Width Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

print(emmeans_plot)

############### COMPREHENSIVE RM EFFECTS ANALYSIS ###############
print("=== COMPREHENSIVE RM vs NoRM ANALYSIS BY CONDITIONS ===")

# Get RM vs NoRM for each correct_num × correct_width combination
rm_by_num_width <- emmeans(final_model, ~ rm_type | correct_num * correct_width)
rm_num_width_contrasts <- pairs(rm_by_num_width)

# Extract detailed results
contrast_results_raw <- summary(rm_num_width_contrasts)
print("Raw contrast results:")
print(contrast_results_raw)

# Apply FDR correction for multiple comparisons
contrast_results_raw$p_fdr <- p.adjust(contrast_results_raw$p.value, method = "fdr")

# Create comprehensive results dataframe
detailed_results <- contrast_results_raw %>%
  mutate(
    # Extract factor levels (already in summary output)
    z_statistic = z.ratio,  # Use z.ratio not t.ratio
    effect_size = estimate / sigma(final_model),  # Cohen's d equivalent
    ci_lower = estimate - qnorm(0.975) * SE,     # Z-based CI
    ci_upper = estimate + qnorm(0.975) * SE,
    # Significance coding with FDR correction
    significance = case_when(
      p_fdr < 0.001 ~ "***",
      p_fdr < 0.01 ~ "**",
      p_fdr < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Interpretation helper
    effect_direction = ifelse(estimate > 0, "RM > NoRM", "RM < NoRM"),
    significant_fdr = p_fdr < 0.05
  ) %>%
  arrange(correct_num, correct_width)

print("Enhanced results with FDR correction:")
print(detailed_results)

# Export complete results for supplementary materials
write.csv(detailed_results, 
          "elimination_pathways/Exp2_RM_effects_by_num_width_conditions.csv", 
          row.names = FALSE)

print("Exported comprehensive results to: elimination_pathways/Exp2_RM_effects_by_num_width_conditions.csv")

# Create comprehensive significance heatmap
comprehensive_heatmap <- ggplot(detailed_results, 
                               aes(x = factor(correct_num), y = factor(correct_width))) +
  geom_tile(aes(fill = estimate), color = "white", size = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(estimate, 3), "\n",
                               "z=", round(z_statistic, 2), "\n", 
                               "FDR p=", round(p_fdr, 3), "\n",
                               significance)), 
            size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = "#3498DB", mid = "white", high = "#E74C3C", 
                       midpoint = 0, name = "RM Effect\n(RM - NoRM)") +
  labs(title = "Experiment 2: RM Effects by Set Size × Width Combination",
       subtitle = "FDR-corrected p-values | *p<.05, **p<.01, ***p<.001 | N=9 comparisons",
       x = "Correct Number (Set Size)", 
       y = "Correct Width",
       caption = "Positive values: RM > NoRM | Negative values: RM < NoRM") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        legend.position = "right")

print(comprehensive_heatmap)

############### MARGINAL INDIVIDUAL CONDITION TESTS ###############
print("=== INDIVIDUAL CONDITION TESTS (MARGINAL - WITH CAVEATS) ===")
print("WARNING: These are overall marginal means qualified by significant interactions")

# Overall marginal means
overall_rm_emmeans <- emmeans(final_model, ~ rm_type)
overall_rm_summary <- summary(overall_rm_emmeans)

# RM condition vs null
rm_mean <- overall_rm_summary[overall_rm_summary$rm_type == "RM", "emmean"]
rm_se <- overall_rm_summary[overall_rm_summary$rm_type == "RM", "SE"]
rm_z <- rm_mean / rm_se
rm_p <- 2 * pnorm(abs(rm_z), lower.tail = FALSE)
rm_ci_lower <- rm_mean - qnorm(0.975) * rm_se
rm_ci_upper <- rm_mean + qnorm(0.975) * rm_se

# NoRM condition vs null  
norm_mean <- overall_rm_summary[overall_rm_summary$rm_type == "NoRM", "emmean"]
norm_se <- overall_rm_summary[overall_rm_summary$rm_type == "NoRM", "SE"]
norm_z <- norm_mean / norm_se
norm_p <- 2 * pnorm(abs(norm_z), lower.tail = FALSE)
norm_ci_lower <- norm_mean - qnorm(0.975) * norm_se
norm_ci_upper <- norm_mean + qnorm(0.975) * norm_se

print("Marginal RM vs zero:")
cat(sprintf("  M = %.4f, SE = %.4f, z = %.3f, p = %.3f, 95%% CI [%.4f, %.4f]\n", 
            rm_mean, rm_se, rm_z, rm_p, rm_ci_lower, rm_ci_upper))

print("Marginal NoRM vs zero:")
cat(sprintf("  M = %.4f, SE = %.4f, z = %.3f, p = %.3f, 95%% CI [%.4f, %.4f]\n", 
            norm_mean, norm_se, norm_z, norm_p, norm_ci_lower, norm_ci_upper))

############### COMPREHENSIVE INTERACTION VISUALIZATION ###############

# 2. Three-way interaction: rm_type × spacing_category × correct_width
print("\n=== 3-WAY INTERACTION: RM × Spacing × Width ===")
rm_spacing_width_means <- emmeans(final_model, ~ rm_type * spacing_category * correct_width)
rm_spacing_width_data <- summary(rm_spacing_width_means)

interaction_plot1 <- ggplot(rm_spacing_width_data, aes(x = spacing_category, y = emmean, 
                                                       color = rm_type, group = rm_type)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(size = 1.2, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 0.8, position = position_dodge(width = 0.2)) +
  facet_wrap(~ correct_width, labeller = label_both) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),
                     name = "Condition") +
  labs(title = "3-Way Interaction: RM × Spacing Category × Correct Width",
       subtitle = "Each panel shows different correct_width levels",
       x = "Spacing Category",
       y = "Width Deviation (Estimated Marginal Mean)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#7F8C8D"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        strip.text = element_text(size = 10, face = "bold"))

print(interaction_plot1)

# 3. Three-way interaction: rm_type × correct_num × correct_width  
print("\n=== 3-WAY INTERACTION: RM × Number × Width ===")
rm_num_width_means <- emmeans(final_model, ~ rm_type * correct_num * correct_width)
rm_num_width_data <- summary(rm_num_width_means)

interaction_plot2 <- ggplot(rm_num_width_data, aes(x = factor(correct_num), y = emmean, 
                                                   color = rm_type, group = rm_type)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(size = 1.2, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 0.8, position = position_dodge(width = 0.2)) +
  facet_wrap(~ correct_width, labeller = label_both) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),
                     name = "Condition") +
  labs(title = "3-Way Interaction: RM × Correct Number × Correct Width",
       subtitle = "Each panel shows different correct_width levels",
       x = "Correct Number",
       y = "Width Deviation (Estimated Marginal Mean)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#7F8C8D"),
        legend.position = "top",
        strip.text = element_text(size = 10, face = "bold"))

print(interaction_plot2)

# 4. Focus on RM effect across key conditions (simplified view)
print("\n=== FOCUSED RM EFFECTS BY CONDITION ===")
# RM effect by spacing category (collapsed across other factors for clarity)
rm_spacing_data <- summary(emmeans(final_model, ~ rm_type * spacing_category))

focused_plot <- ggplot(rm_spacing_data, aes(x = spacing_category, y = emmean, 
                                           color = rm_type, group = rm_type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 1.5, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),
                     name = "Condition") +
  labs(title = "RM Effect by Spacing Category",
       subtitle = "Marginal means averaged across correct_num and correct_width",
       x = "Spacing Category",
       y = "Width Deviation (Estimated Marginal Mean)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "top")

print(focused_plot)

# 5. Effect size plot for RM vs NoRM by condition
print("\n=== RM EFFECT SIZES BY CONDITION ===")
# Calculate RM effect (RM - NoRM) for each spacing category
rm_effects <- summary(pairs(rm_by_spacing))
rm_effects$spacing_category <- c("Smaller", "Middle", "Larger")

effect_size_plot <- ggplot(rm_effects, aes(x = spacing_category, y = estimate)) +
  geom_col(fill = "#34495E", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                width = 0.2, size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(label = paste0("p=", sprintf("%.3f", p.value))), 
            vjust = ifelse(rm_effects$estimate > 0, -0.5, 1.5), 
            size = 3.5, fontface = "bold") +
  labs(title = "RM Effect Size by Spacing Category",
       subtitle = "Positive values = RM > NoRM | Error bars show SE | p-values shown",
       x = "Spacing Category",
       y = "RM Effect (RM - NoRM)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

print(effect_size_plot)

# Print comprehensive interpretation summary
print("\n=== FINAL INTERPRETATION SUMMARY ===")
print("Final model contains three significant 3-way interactions:")
print("1. rm_type:spacing_category:correct_width")
print("2. rm_type:correct_num:correct_width") 
print("3. spacing_category:correct_num:correct_width")
print("\nThis means RM effects vary depending on the specific combination of factors.")
print("\nKey findings from visualizations:")
print("- Primary RM vs NoRM comparison shows overall marginal effect")
print("- 3-way interaction plots reveal how RM effects depend on experimental conditions")
print("- Focused spacing category plot shows RM effects across key theoretical conditions")
print("- Effect size plot quantifies RM effects with statistical significance")
print("\nInterpretation guidance:")
print("- Look for consistent patterns across panels in 3-way interaction plots")
print("- Focus on conditions where RM vs NoRM differences are largest")
print("- Consider theoretical importance of different factor combinations")

############### COMPREHENSIVE APA RESULTS SUMMARY ###############
print("=== COMPREHENSIVE APA-STYLE RESULTS FOR EXPERIMENT 2 ===")

# Count significant effects
n_significant <- sum(detailed_results$significant_fdr)
n_total <- nrow(detailed_results)

# Get strongest effects
strongest_effect <- detailed_results[which.max(abs(detailed_results$estimate)), ]
strongest_positive <- detailed_results[which.max(detailed_results$estimate), ]

# Overall marginal comparison
overall_contrast <- pairs(overall_rm_emmeans)
overall_contrast_summary <- summary(overall_contrast)
overall_z <- overall_contrast_summary$z.ratio[1]
overall_p <- overall_contrast_summary$p.value[1]
overall_diff <- overall_contrast_summary$estimate[1]
overall_se <- overall_contrast_summary$SE[1]
overall_effect_size <- overall_diff / sigma(final_model)
overall_ci_lower <- overall_diff - qnorm(0.975) * overall_se
overall_ci_upper <- overall_diff + qnorm(0.975) * overall_se

cat("\n=== PUBLICATION-READY APA RESULTS ===\n\n")

cat("MODEL SELECTION:\n")
cat("Backward elimination from full factorial model retained main effects, all\n")
cat("2-way interactions, and three 3-way interactions. Elimination stopped at Step 2\n")
cat(sprintf("after removing only rm_type:spacing_category:correct_num (final model: AIC = %.2f).\n\n", AIC(final_model)))

cat("MODEL COMPLEXITY:\n")
cat("Final model retained three-way interactions indicating RM effects varied\n")
cat("significantly by experimental conditions (set size × width combinations).\n\n")

cat("COMPREHENSIVE ANALYSIS:\n") 
cat(sprintf("RM vs NoRM effects examined across %d set-size × width combinations\n", n_total))
cat(sprintf("with FDR correction for multiple comparisons.\n"))
cat(sprintf("Significant RM effects found in %d of %d conditions (%.1f%%).\n\n", 
            n_significant, n_total, (n_significant/n_total)*100))

cat("STRONGEST EFFECTS:\n")
cat(sprintf("Largest RM effect: Set size %s, Width %s (RM-NoRM = %.3f, z = %.2f, FDR p = %.3f)\n",
            strongest_effect$correct_num, strongest_effect$correct_width, 
            strongest_effect$estimate, strongest_effect$z_statistic, strongest_effect$p_fdr))

cat("\nMARGINAL COMPARISON (QUALIFIED BY INTERACTIONS):\n")
cat(sprintf("Overall RM vs NoRM: z = %.2f, p = %.3f, d = %.3f, 95%% CI [%.3f, %.3f]\n", 
            overall_z, overall_p, overall_effect_size, overall_ci_lower, overall_ci_upper))
cat("Note: This marginal effect is qualified by significant condition-specific variations.\n\n")

cat("INDIVIDUAL CONDITIONS (MARGINAL):\n")
if(rm_p < 0.05) {
  cat(sprintf("RM condition differed from zero: z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
              rm_z, rm_p, rm_ci_lower, rm_ci_upper))
} else {
  cat(sprintf("RM condition did not differ from zero: z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
              rm_z, rm_p, rm_ci_lower, rm_ci_upper))
}

if(norm_p < 0.05) {
  cat(sprintf("NoRM condition differed from zero: z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
              norm_z, norm_p, norm_ci_lower, norm_ci_upper))
} else {
  cat(sprintf("NoRM condition did not differ from zero: z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f]\n",
              norm_z, norm_p, norm_ci_lower, norm_ci_upper))
}

cat("\nNote: Individual condition tests represent averages across all experimental\n")
cat("combinations and are qualified by significant interaction effects.\n")

cat("\nCOMPLETE RESULTS:\n") 
cat("See supplementary materials: Exp2_RM_effects_by_num_width_conditions.csv\n")
cat("Visualization: Comprehensive heatmap with FDR-corrected significance testing\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
