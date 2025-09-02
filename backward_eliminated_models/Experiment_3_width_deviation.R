library(tidyverse)
library(lme4)
library(emmeans)
library(ggrepel)

df <- read.csv("datasets/processed.csv")

# Prepare the data with proper factorization for Experiment 1C (Experiment 3)
df_filtered <- df %>%
  filter(exp_version == "Exp1C") %>%  # Changed to Exp1C for Experiment 3
  filter(number_deviation %in% c(-1, 0))

df_exp <- df_filtered %>%
  mutate(
    # Factor all categorical variables with proper contrasts
    rm_type = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                     levels = c("NoRM", "RM")),  # NoRM as reference
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    subID = factor(subID),
    
    # Check which width deviation measure to use
    width_deviation_available = !is.na(width_deviation),
    width_deviation_ratio_available = !is.na(width_deviation_ration)  # Note: typo in original data
  )

# Data exploration for Experiment 3 (3 factors only)
print("=== EXPERIMENT 3 DATA EXPLORATION ===")
print("Factor distributions:")
table(df_exp$rm_type)
table(df_exp$correct_num)
table(df_exp$correct_width)
print("Cross-tabulation of experimental factors:")
table(df_exp$rm_type, df_exp$correct_num)
table(df_exp$rm_type, df_exp$correct_width)
table(df_exp$correct_num, df_exp$correct_width)

#### RM vs Non-RM Analysis for Experiment 3
summary(df_exp$width_deviation)

# Simplified factorial model for 3 factors - starting point for backward elimination
full_model <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                     rm_type:correct_num + rm_type:correct_width + correct_num:correct_width +
                     rm_type:correct_num:correct_width +
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

# Backward elimination steps for 3-factor design
# Step 1: Test 3-way interaction (rm_type:correct_num:correct_width)
model_no_3way <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                        rm_type:correct_num + rm_type:correct_width + correct_num:correct_width +
                        (1 | subID), 
                      data = df_exp, REML = FALSE)

step1_test <- anova(model_no_3way, full_model)
print("Step 1: Test removal of 3-way interaction")
print(step1_test)

# Record Step 1 model
model_comparison <- rbind(model_comparison, data.frame(
  Step = "1",
  Model_Name = "model_no_3way",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_no_3way),
  BIC = BIC(model_no_3way),
  LogLik = as.numeric(logLik(model_no_3way)),
  p_value = step1_test$`Pr(>Chisq)`[2],
  Decision = "Remove_3way",
  stringsAsFactors = FALSE
))

# Based on the result, select working model for next step
# If 3-way not significant (p > 0.05), use model_no_3way
# If 3-way significant (p < 0.05), keep full_model
working_model <- model_no_3way  # Change this based on results above

# Step 2: Test each 2-way interaction - remove the one with highest p-value

# Test removal of rm_type:correct_num
model_2A <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                   rm_type:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step2A_test <- anova(model_2A, working_model)
print("2A: Test removal of rm_type:correct_num")
print(step2A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2A",
  Model_Name = "model_2A", 
  Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_2A), BIC = BIC(model_2A), LogLik = as.numeric(logLik(model_2A)),
  p_value = step2A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_width
model_2B <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                   rm_type:correct_num + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step2B_test <- anova(model_2B, working_model)
print("2B: Test removal of rm_type:correct_width")
print(step2B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2B",
  Model_Name = "model_2B",
  Interaction_Removed = "rm_type:correct_width", 
  AIC = AIC(model_2B), BIC = BIC(model_2B), LogLik = as.numeric(logLik(model_2B)),
  p_value = step2B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_2C <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step2C_test <- anova(model_2C, working_model)
print("2C: Test removal of correct_num:correct_width")
print(step2C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2C",
  Model_Name = "model_2C",
  Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_2C), BIC = BIC(model_2C), LogLik = as.numeric(logLik(model_2C)),
  p_value = step2C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step2 to whichever model (2A, 2B, or 2C) had highest p-value
model_step2 <- model_2A  # Update this based on anova results above

# Record Step 2 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2_Selected",
  Model_Name = "model_step2",
  Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_step2), BIC = BIC(model_step2), LogLik = as.numeric(logLik(model_step2)),
  p_value = step2A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 3: Test removal of remaining 2-way interactions from model_step2

# Test removal of rm_type:correct_width
model_3A <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                   correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step3A_test <- anova(model_3A, model_step2)
print("3A: Test removal of rm_type:correct_width")
print(step3A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3A", Model_Name = "model_3A", 
  Interaction_Removed = "rm_type:correct_width",
  AIC = AIC(model_3A), BIC = BIC(model_3A), LogLik = as.numeric(logLik(model_3A)),
  p_value = step3A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_3B <- lmer(width_deviation ~ rm_type + correct_num + correct_width +
                   rm_type:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step3B_test <- anova(model_3B, model_step2)
print("3B: Test removal of correct_num:correct_width")
print(step3B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3B", Model_Name = "model_3B",
  Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_3B), BIC = BIC(model_3B), LogLik = as.numeric(logLik(model_3B)),
  p_value = step3B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set final model based on anova result above
final_model <- model_step2  # Update this based on elimination results

# Record final model selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "Final",
  Model_Name = "final_model",
  Interaction_Removed = "Complete_elimination",
  AIC = AIC(final_model), BIC = BIC(final_model), LogLik = as.numeric(logLik(final_model)),
  p_value = NA, Decision = "Final_selected", stringsAsFactors = FALSE
))


# Create comprehensive elimination visualization
plot_data <- model_comparison
plot_data$Step_Clean <- gsub("_Selected|_Final", "", plot_data$Step)

# Extract step numbers properly
plot_data$Step_Number <- ifelse(plot_data$Step_Clean == "0_Full", 0,
                                ifelse(plot_data$Step_Clean == "Final", 99,
                                       as.numeric(gsub("[A-Z]", "", plot_data$Step_Clean))))

# Handle cases where step number extraction fails
plot_data$Step_Number[is.na(plot_data$Step_Number)] <- 
  as.numeric(gsub("[A-Z].*", "", plot_data$Step_Clean[is.na(plot_data$Step_Number)]))

# Create step labels and mark selected models
plot_data$Selected <- plot_data$Decision %in% c("Starting_model", "Remove_3way", "Selected", "Final_selected")
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
  labs(title = "Experiment 3 - Model Selection: Systematic Interaction Elimination",
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
  labs(title = "Experiment 3 - Model Selection: BIC Progression",
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
selected_models <- model_comparison[model_comparison$Decision %in% c("Starting_model", "Remove_3way", "Selected", "Final_selected"), ]


write.csv(selected_models, "elimination_pathways/elimination_pathway_Exp3_width_deviation.csv", row.names = FALSE)

############### EMMEANS ANALYSIS FOR 3-FACTOR FINAL MODEL ###############
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

# Check for interactions in final model
print("\n=== INTERACTION ANALYSIS ===")

# RM effect by correct_width  
print("\nRM effect by correct_width:")
rm_by_width <- emmeans(final_model, ~ rm_type | correct_width)
rm_by_width_contrasts <- pairs(rm_by_width)
print(rm_by_width_contrasts)

############### INDIVIDUAL CONDITION TESTS ###############
print("=== INDIVIDUAL CONDITION TESTS AGAINST NULL ===")

# Test each condition against null hypothesis of zero deviation
rm_means_summary <- summary(rm_emmeans)

# RM condition vs null (μ = 0)
print("RM condition vs null (μ = 0):")
rm_mean <- rm_means_summary[rm_means_summary$rm_type == "RM", "emmean"]
rm_se <- rm_means_summary[rm_means_summary$rm_type == "RM", "SE"]
rm_z <- rm_mean / rm_se
rm_p <- 2 * pnorm(abs(rm_z), lower.tail = FALSE)
rm_ci_lower <- rm_mean - qnorm(0.975) * rm_se
rm_ci_upper <- rm_mean + qnorm(0.975) * rm_se

cat(sprintf("  M = %.4f, SE = %.4f\n", rm_mean, rm_se))
cat(sprintf("  z = %.3f, p = %.3f\n", rm_z, rm_p))
cat(sprintf("  95%% CI [%.4f, %.4f]\n\n", rm_ci_lower, rm_ci_upper))

# NoRM condition vs null (μ = 0)
print("NoRM condition vs null (μ = 0):")
norm_mean <- rm_means_summary[rm_means_summary$rm_type == "NoRM", "emmean"]
norm_se <- rm_means_summary[rm_means_summary$rm_type == "NoRM", "SE"]
norm_z <- norm_mean / norm_se
norm_p <- 2 * pnorm(abs(norm_z), lower.tail = FALSE)
norm_ci_lower <- norm_mean - qnorm(0.975) * norm_se
norm_ci_upper <- norm_mean + qnorm(0.975) * norm_se

cat(sprintf("  M = %.4f, SE = %.4f\n", norm_mean, norm_se))
cat(sprintf("  z = %.3f, p = %.3f\n", norm_z, norm_p))
cat(sprintf("  95%% CI [%.4f, %.4f]\n\n", norm_ci_lower, norm_ci_upper))

############### ENHANCED RM EFFECTS BY WIDTH ANALYSIS ###############
print("=== ENHANCED RM vs NoRM ANALYSIS BY WIDTH ===")

# Based on final model assumption: rm_type:correct_width interaction
# Get detailed results for each width level
rm_width_detailed <- summary(rm_by_width_contrasts)

# Add effect sizes and confidence intervals
rm_width_enhanced <- rm_width_detailed %>%
  mutate(
    z_statistic = z.ratio,  # Use z.ratio for consistency
    effect_size = estimate / sigma(final_model),  # Cohen's d equivalent
    ci_lower = estimate - qnorm(0.975) * SE,     # Z-based CI
    ci_upper = estimate + qnorm(0.975) * SE,
    # Interpretation helper
    effect_direction = ifelse(estimate > 0, "RM > NoRM", "RM < NoRM"),
    significant = p.value < 0.05
  )

print("Enhanced RM effects by width level:")
print(rm_width_enhanced)

# Export detailed results for supplementary materials
write.csv(rm_width_enhanced, 
          "elimination_pathways/Exp3_RM_effects_by_width_conditions.csv", 
          row.names = FALSE)

print("Exported detailed results to: elimination_pathways/Exp3_RM_effects_by_width_conditions.csv")

############### COMPREHENSIVE INTERACTION VISUALIZATION ###############

# 1. Primary RM vs NoRM comparison (marginal means)
emmeans_plot_data <- summary(rm_emmeans)
emmeans_plot <- ggplot(emmeans_plot_data, aes(x = rm_type, y = emmean)) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, color = "#2C3E50") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Experiment 3 - RM vs NoRM: Primary Comparison (Marginal Means)",
       subtitle = "Error bars show standard errors | Red line = no deviation",
       x = "Condition",
       y = "Width Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

print(emmeans_plot)

# 2. RM effect by correct_num
print("\n=== RM EFFECTS BY CORRECT NUMBER ===")
rm_num_data <- summary(emmeans(final_model, ~ rm_type * correct_num))

rm_by_num_plot <- ggplot(rm_num_data, aes(x = factor(correct_num), y = emmean, 
                                          color = rm_type, group = rm_type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 1.5, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),
                     name = "Condition") +
  labs(title = "RM Effect by Correct Number",
       subtitle = "Interaction plot showing RM effects across set sizes",
       x = "Correct Number (Set Size)",
       y = "Width Deviation (Estimated Marginal Mean)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "top")

print(rm_by_num_plot)

# 3. RM effect by correct_width
print("\n=== RM EFFECTS BY CORRECT WIDTH ===")
rm_width_data <- summary(emmeans(final_model, ~ rm_type * correct_width))

rm_by_width_plot <- ggplot(rm_width_data, aes(x = factor(correct_width), y = emmean, 
                                              color = rm_type, group = rm_type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 1.5, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),
                     name = "Condition") +
  labs(title = "RM Effect by Correct Width",
       subtitle = "Interaction plot showing RM effects across width conditions",
       x = "Correct Width",
       y = "Width Deviation (Estimated Marginal Mean)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "top")

print(rm_by_width_plot)

# 4. Effect size plots
print("\n=== RM EFFECT SIZES BY CONDITION ===")

# Effect sizes by correct_width
rm_width_effects <- summary(rm_by_width_contrasts)
rm_width_effects$correct_width <- unique(df_exp$correct_width)[order(unique(df_exp$correct_width))]

effect_width_plot <- ggplot(rm_width_effects, aes(x = factor(correct_width), y = estimate)) +
  geom_col(fill = "#34495E", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                width = 0.2, size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(label = paste0("p=", sprintf("%.3f", p.value))), 
            vjust = ifelse(rm_width_effects$estimate > 0, -0.5, 1.5), 
            size = 3.5, fontface = "bold") +
  labs(title = "RM Effect Size by Correct Width",
       subtitle = "Positive values = RM > NoRM | Error bars show SE | p-values shown",
       x = "Correct Width",
       y = "RM Effect (RM - NoRM)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

print(effect_width_plot)

# Print comprehensive interpretation summary
print("\n=== FINAL INTERPRETATION SUMMARY FOR EXPERIMENT 3 ===")
print("Experiment 3 design: 3 factors (rm_type × correct_num × correct_width)")
print("Simplified model structure without spacing variable")
print("\nFinal model interpretation depends on elimination results:")
print("- If 3-way interaction significant: RM effects depend on both num and width")
print("- If 2-way interactions remain: RM effects vary by individual factors")
print("- If main effects only: Overall RM vs NoRM difference")
print("\nKey theoretical focus:")
print("- rm_type × correct_num: Redundancy masking effects by set size")
print("- rm_type × correct_width: Redundancy masking effects by width")
print("\nInterpretation guidance:")
print("- Focus on rm_type × correct_num for theoretical implications")
print("- Consider practical significance alongside statistical significance")
print("- Compare effect sizes across different factor levels")

############### COMPREHENSIVE APA RESULTS SUMMARY ###############
print("=== COMPREHENSIVE APA-STYLE RESULTS FOR EXPERIMENT 3 ===")

# Extract key statistics for APA reporting based on current model structure
contrast_results <- summary(rm_contrast)
contrast_z <- contrast_results$z.ratio[1]
contrast_df <- contrast_results$df[1]
contrast_p <- contrast_results$p.value[1] 
contrast_diff <- contrast_results$estimate[1]
contrast_se <- contrast_results$SE[1]
contrast_ci_lower <- contrast_diff - qnorm(0.975) * contrast_se
contrast_ci_upper <- contrast_diff + qnorm(0.975) * contrast_se

# Count width conditions and significant effects
n_width_conditions <- nrow(rm_width_enhanced)
n_significant_width <- sum(rm_width_enhanced$significant)

# Get strongest effect by width
if(nrow(rm_width_enhanced) > 0) {
  strongest_width_effect <- rm_width_enhanced[which.max(abs(rm_width_enhanced$estimate)), ]
}

cat("\n=== PUBLICATION-READY APA RESULTS ===\n\n")

cat("MODEL SELECTION:\n")
cat("Backward elimination from full 3-factor model ")
selected_models_final <- model_comparison[model_comparison$Decision %in% c("Starting_model", "Remove_3way", "Selected", "Final_selected"), ]
final_model_aic <- AIC(final_model)
cat(sprintf("(final model: AIC = %.2f).\n", final_model_aic))

# Check what interactions are in final model to customize reporting
final_model_formula <- formula(final_model)
has_3way <- grepl("rm_type:correct_num:correct_width", as.character(final_model_formula)[3])
has_rm_width <- grepl("rm_type:correct_width", as.character(final_model_formula)[3])
has_rm_num <- grepl("rm_type:correct_num", as.character(final_model_formula)[3])
has_num_width <- grepl("correct_num:correct_width", as.character(final_model_formula)[3])

if(has_3way) {
  cat("Final model retained 3-way interaction indicating RM effects vary by both\n")
  cat("set size and width combinations.\n\n")
} else if(has_rm_width || has_rm_num) {
  cat("Final model retained 2-way interactions indicating RM effects vary by\n")
  if(has_rm_width && has_rm_num) {
    cat("both set size and width conditions.\n\n")
  } else if(has_rm_width) {
    cat("width conditions.\n\n") 
  } else {
    cat("set size conditions.\n\n")
  }
} else {
  cat("Final model retained only main effects indicating overall RM vs NoRM difference.\n\n")
}

cat("PRIMARY HYPOTHESIS TEST (RM vs NoRM):\n")
cat(sprintf("RM trials (M = %.3f, SE = %.3f) ", rm_mean, rm_se))
cat(sprintf("compared to NoRM trials (M = %.3f, SE = %.3f),\n", norm_mean, norm_se))
cat(sprintf("z = %.2f, p = %.3f, d = %.3f, 95%% CI [%.3f, %.3f].\n\n",
            contrast_z, contrast_p, effect_size, 
            contrast_ci_lower, contrast_ci_upper))

# Conditional reporting based on model complexity
if(has_rm_width && nrow(rm_width_enhanced) > 0) {
  cat("WIDTH-SPECIFIC ANALYSIS:\n")
  cat(sprintf("RM vs NoRM effects examined across %d width conditions.\n", n_width_conditions))
  if(n_significant_width > 0) {
    cat(sprintf("Significant RM effects found in %d of %d width conditions.\n", 
                n_significant_width, n_width_conditions))
    cat(sprintf("Strongest effect: Width %s (RM-NoRM = %.3f, z = %.2f, p = %.3f)\n\n",
                strongest_width_effect$correct_width, strongest_width_effect$estimate,
                strongest_width_effect$z_statistic, strongest_width_effect$p.value))
  } else {
    cat("No significant RM effects found in individual width conditions.\n\n")
  }
}

cat("INDIVIDUAL CONDITION TESTS:\n")
if(rm_p < 0.05) {
  cat(sprintf("RM trials differed significantly from zero, z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f].\n", 
              rm_z, rm_p, rm_ci_lower, rm_ci_upper))
} else {
  cat(sprintf("RM trials did not differ significantly from zero, z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f].\n", 
              rm_z, rm_p, rm_ci_lower, rm_ci_upper))
}

if(norm_p < 0.05) {
  cat(sprintf("NoRM trials differed significantly from zero, z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f].\n", 
              norm_z, norm_p, norm_ci_lower, norm_ci_upper))
} else {
  cat(sprintf("NoRM trials did not differ significantly from zero, z = %.2f, p = %.3f, 95%% CI [%.3f, %.3f].\n", 
              norm_z, norm_p, norm_ci_lower, norm_ci_upper))
}

if(has_rm_width || has_rm_num || has_3way) {
  cat("\nNote: Individual condition tests represent marginal averages qualified by\n")
  cat("significant interaction effects in the final model.\n")
}

cat("\nSUPPLEMENTARY MATERIALS:\n")
cat("Complete elimination pathway: elimination_pathway_Exp3_width_deviation.csv\n")
if(has_rm_width) {
  cat("Detailed width analysis: Exp3_RM_effects_by_width_conditions.csv\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")

