library(tidyverse)
library(lme4)
library(emmeans)
library(performance)
library(ggrepel)

df <- read.csv("datasets/processed.csv")

# Prepare the data with proper factorization
df_filtered <- df %>%
  filter(exp_version == "Exp1A") %>%
  filter(number_deviation %in% c(-1, 0))

# Calculate quantiles for later use in plotting
quantiles <- quantile(df_filtered$correct_space, probs = c(0, 1/3, 2/3, 1))

df_exp <- df_filtered %>%
  mutate(
    # Create equal quantile splits
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
      correct_space <= quantile(correct_space, 2/3) ~ "Middle",
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

# Spacing distrubtions
ggplot(df_exp, aes(x = correct_space_factor, color = correct_space_factor)) +
  geom_bar(alpha = 0.8, color = "white", size = 0.3) +
  geom_vline(xintercept = which(levels(df_exp$correct_space_factor) == 
                                  as.character(round(quantiles[2], 3))) + 0.5, 
             color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = which(levels(df_exp$correct_space_factor) == 
                                  as.character(round(quantiles[3], 3))) + 0.5, 
             color = "red", linetype = "dashed", size = 1.2) +
  scale_color_manual(
    values = c("Smaller" = "#2E86C1", "Middle" = "#F39C12", "Larger" = "#28B463"),
    name = "Spacing Category"
  ) +
  labs(
    title = "Spacing Value Distribution with Equal Quantile Splits",
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

#### RM vs Non-RM Analysis
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

print("Full model summary:")
summary(full_model)

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

print("Test removal of 4-way interaction:")
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

print("Working model after 4-way test:")
summary(working_model)

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
model_step2 <- model_2D  # Update this based on anova results above

# Record Step 2 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "2_Selected",
  Model_Name = "model_step2",
  Interaction_Removed = "spacing_category:correct_num:correct_width",
  AIC = AIC(model_step2), BIC = BIC(model_step2), LogLik = as.numeric(logLik(model_step2)),
  p_value = step2D_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 3: Test remaining RM 3-way interactions (assuming model_2D was selected)

# Test removal of rm_type:spacing_category:correct_num
model_3A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_width + rm_type:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step3A_test <- anova(model_3A, model_step2)
print("3A: Test removal of rm_type:spacing_category:correct_num")
print(step3A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3A", Model_Name = "model_3A", 
  Interaction_Removed = "rm_type:spacing_category:correct_num",
  AIC = AIC(model_3A), BIC = BIC(model_3A), LogLik = as.numeric(logLik(model_3A)),
  p_value = step3A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:spacing_category:correct_width
model_3B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_num + rm_type:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step3B_test <- anova(model_3B, model_step2)
print("3B: Test removal of rm_type:spacing_category:correct_width")
print(step3B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3B", Model_Name = "model_3B",
  Interaction_Removed = "rm_type:spacing_category:correct_width",
  AIC = AIC(model_3B), BIC = BIC(model_3B), LogLik = as.numeric(logLik(model_3B)),
  p_value = step3B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_num:correct_width
model_3C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_num + rm_type:spacing_category:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step3C_test <- anova(model_3C, model_step2)
print("3C: Test removal of rm_type:correct_num:correct_width")
print(step3C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3C", Model_Name = "model_3C",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_3C), BIC = BIC(model_3C), LogLik = as.numeric(logLik(model_3C)),
  p_value = step3C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step3 to whichever model (3A, 3B, or 3C) had highest p-value
model_step3 <- model_3A  # Update based on anova results above

# Record Step 3 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "3_Selected", Model_Name = "model_step3",
  Interaction_Removed = "rm_type:spacing_category:correct_num",
  AIC = AIC(model_step3), BIC = BIC(model_step3), LogLik = as.numeric(logLik(model_step3)),
  p_value = step3A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# NOTE: Steps 4-11 follow the same pattern - add comprehensive tracking as needed
# The framework is established for complete elimination documentation

# OPTIONAL: To track ALL steps comprehensively, apply this pattern:
# For each step X with models XA, XB, XC, etc.:
# 1. Create stepX_test variables for each anova() call
# 2. Add print() statements with clear labels  
# 3. Add model_comparison <- rbind() calls for each test
# 4. Add selection recording with "X_Selected" step name

# Step 4: Test remaining 3-way interactions from model_step3

# Test removal of rm_type:spacing_category:correct_width
model_4A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:correct_num:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step4A_test <- anova(model_4A, model_step3)
print("4A: Test removal of rm_type:spacing_category:correct_width")
print(step4A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "4A", Model_Name = "model_4A",
  Interaction_Removed = "rm_type:spacing_category:correct_width",
  AIC = AIC(model_4A), BIC = BIC(model_4A), LogLik = as.numeric(logLik(model_4A)),
  p_value = step4A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_num:correct_width  
model_4B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   rm_type:spacing_category:correct_width + 
                   (1 | subID), data = df_exp, REML = FALSE)
step4B_test <- anova(model_4B, model_step3)
print("4B: Test removal of rm_type:correct_num:correct_width")
print(step4B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "4B", Model_Name = "model_4B",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_4B), BIC = BIC(model_4B), LogLik = as.numeric(logLik(model_4B)),
  p_value = step4B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step4 to whichever model (4A or 4B) had highest p-value
model_step4 <- model_4A  # Update based on anova results above

# Record Step 4 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "4_Selected", Model_Name = "model_step4",
  Interaction_Removed = "rm_type:spacing_category:correct_width",
  AIC = AIC(model_step4), BIC = BIC(model_step4), LogLik = as.numeric(logLik(model_step4)),
  p_value = step4A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 5: Test removal of last 3-way interaction from model_step4
model_5A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step5A_test <- anova(model_5A, model_step4)
print("5A: Test removal of rm_type:correct_num:correct_width")
print(step5A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "5A", Model_Name = "model_5A",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_5A), BIC = BIC(model_5A), LogLik = as.numeric(logLik(model_5A)),
  p_value = step5A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step5 based on anova result above
model_step5 <- model_5A  # Update based on anova results above

# Record Step 5 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "5_Selected", Model_Name = "model_step5",
  Interaction_Removed = "rm_type:correct_num:correct_width",
  AIC = AIC(model_step5), BIC = BIC(model_step5), LogLik = as.numeric(logLik(model_step5)),
  p_value = step5A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 6: Test removal of each 2-way interaction

# Test removal of rm_type:spacing_category
model_6A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6A_test <- anova(model_6A, model_step5)
print("6A: Test removal of rm_type:spacing_category")
print(step6A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6A", Model_Name = "model_6A", Interaction_Removed = "rm_type:spacing_category",
  AIC = AIC(model_6A), BIC = BIC(model_6A), LogLik = as.numeric(logLik(model_6A)),
  p_value = step6A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_num
model_6B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6B_test <- anova(model_6B, model_step5)
print("6B: Test removal of rm_type:correct_num")
print(step6B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6B", Model_Name = "model_6B", Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_6B), BIC = BIC(model_6B), LogLik = as.numeric(logLik(model_6B)),
  p_value = step6B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_width
model_6C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6C_test <- anova(model_6C, model_step5)
print("6C: Test removal of rm_type:correct_width")
print(step6C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6C", Model_Name = "model_6C", Interaction_Removed = "rm_type:correct_width",
  AIC = AIC(model_6C), BIC = BIC(model_6C), LogLik = as.numeric(logLik(model_6C)),
  p_value = step6C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_num
model_6D <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6D_test <- anova(model_6D, model_step5)
print("6D: Test removal of spacing_category:correct_num")
print(step6D_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6D", Model_Name = "model_6D", Interaction_Removed = "spacing_category:correct_num",
  AIC = AIC(model_6D), BIC = BIC(model_6D), LogLik = as.numeric(logLik(model_6D)),
  p_value = step6D_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_width
model_6E <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6E_test <- anova(model_6E, model_step5)
print("6E: Test removal of spacing_category:correct_width")
print(step6E_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6E", Model_Name = "model_6E", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_6E), BIC = BIC(model_6E), LogLik = as.numeric(logLik(model_6E)),
  p_value = step6E_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_6F <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:spacing_category + rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step6F_test <- anova(model_6F, model_step5)
print("6F: Test removal of correct_num:correct_width")
print(step6F_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6F", Model_Name = "model_6F", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_6F), BIC = BIC(model_6F), LogLik = as.numeric(logLik(model_6F)),
  p_value = step6F_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step6 to whichever model (6A-6F) had highest p-value
model_step6 <- model_6A  # Update based on anova results above

# Record Step 6 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "6_Selected", Model_Name = "model_step6", Interaction_Removed = "rm_type:spacing_category",
  AIC = AIC(model_step6), BIC = BIC(model_step6), LogLik = as.numeric(logLik(model_step6)),
  p_value = step6A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 7: Test removal of remaining 2-way interactions from model_step6

# Test removal of rm_type:correct_num
model_7A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step7A_test <- anova(model_7A, model_step6)
print("7A: Test removal of rm_type:correct_num")
print(step7A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7A", Model_Name = "model_7A", Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_7A), BIC = BIC(model_7A), LogLik = as.numeric(logLik(model_7A)),
  p_value = step7A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_width
model_7B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num +
                   spacing_category:correct_num + spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step7B_test <- anova(model_7B, model_step6)
print("7B: Test removal of rm_type:correct_width")
print(step7B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7B", Model_Name = "model_7B", Interaction_Removed = "rm_type:correct_width",
  AIC = AIC(model_7B), BIC = BIC(model_7B), LogLik = as.numeric(logLik(model_7B)),
  p_value = step7B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_num
model_7C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step7C_test <- anova(model_7C, model_step6)
print("7C: Test removal of spacing_category:correct_num")
print(step7C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7C", Model_Name = "model_7C", Interaction_Removed = "spacing_category:correct_num",
  AIC = AIC(model_7C), BIC = BIC(model_7C), LogLik = as.numeric(logLik(model_7C)),
  p_value = step7C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_width
model_7D <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step7D_test <- anova(model_7D, model_step6)
print("7D: Test removal of spacing_category:correct_width")
print(step7D_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7D", Model_Name = "model_7D", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_7D), BIC = BIC(model_7D), LogLik = as.numeric(logLik(model_7D)),
  p_value = step7D_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_7E <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_num + spacing_category:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step7E_test <- anova(model_7E, model_step6)
print("7E: Test removal of correct_num:correct_width")
print(step7E_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7E", Model_Name = "model_7E", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_7E), BIC = BIC(model_7E), LogLik = as.numeric(logLik(model_7E)),
  p_value = step7E_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step7 to whichever model (7A-7E) had highest p-value
model_step7 <- model_7C  # Update based on anova results above

# Record Step 7 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "7_Selected", Model_Name = "model_step7", Interaction_Removed = "spacing_category:correct_num",
  AIC = AIC(model_step7), BIC = BIC(model_step7), LogLik = as.numeric(logLik(model_step7)),
  p_value = step7C_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 8: Test removal of remaining 2-way interactions from model_step7

# Test removal of rm_type:correct_num
model_8A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_width +
                   spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step8A_test <- anova(model_8A, model_step7)
print("8A: Test removal of rm_type:correct_num")
print(step8A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "8A", Model_Name = "model_8A", Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_8A), BIC = BIC(model_8A), LogLik = as.numeric(logLik(model_8A)),
  p_value = step8A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of rm_type:correct_width
model_8B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num +
                   spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step8B_test <- anova(model_8B, model_step7)
print("8B: Test removal of rm_type:correct_width")
print(step8B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "8B", Model_Name = "model_8B", Interaction_Removed = "rm_type:correct_width",
  AIC = AIC(model_8B), BIC = BIC(model_8B), LogLik = as.numeric(logLik(model_8B)),
  p_value = step8B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_width
model_8C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step8C_test <- anova(model_8C, model_step7)
print("8C: Test removal of spacing_category:correct_width")
print(step8C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "8C", Model_Name = "model_8C", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_8C), BIC = BIC(model_8C), LogLik = as.numeric(logLik(model_8C)),
  p_value = step8C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_8D <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + rm_type:correct_width +
                   spacing_category:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step8D_test <- anova(model_8D, model_step7)
print("8D: Test removal of correct_num:correct_width")
print(step8D_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "8D", Model_Name = "model_8D", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_8D), BIC = BIC(model_8D), LogLik = as.numeric(logLik(model_8D)),
  p_value = step8D_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step8 to whichever model (8A-8D) had highest p-value
model_step8 <- model_8B  # Update based on anova results above

# Record Step 8 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "8_Selected", Model_Name = "model_step8", Interaction_Removed = "rm_type:correct_width",
  AIC = AIC(model_step8), BIC = BIC(model_step8), LogLik = as.numeric(logLik(model_step8)),
  p_value = step8B_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 9: Test removal of remaining 2-way interactions from model_step8

# Test removal of rm_type:correct_num
model_9A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   spacing_category:correct_width + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step9A_test <- anova(model_9A, model_step8)
print("9A: Test removal of rm_type:correct_num")
print(step9A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "9A", Model_Name = "model_9A", Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_9A), BIC = BIC(model_9A), LogLik = as.numeric(logLik(model_9A)),
  p_value = step9A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of spacing_category:correct_width
model_9B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + correct_num:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step9B_test <- anova(model_9B, model_step8)
print("9B: Test removal of spacing_category:correct_width")
print(step9B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "9B", Model_Name = "model_9B", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_9B), BIC = BIC(model_9B), LogLik = as.numeric(logLik(model_9B)),
  p_value = step9B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_9C <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                   rm_type:correct_num + spacing_category:correct_width +
                   (1 | subID), data = df_exp, REML = FALSE)
step9C_test <- anova(model_9C, model_step8)
print("9C: Test removal of correct_num:correct_width")
print(step9C_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "9C", Model_Name = "model_9C", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_9C), BIC = BIC(model_9C), LogLik = as.numeric(logLik(model_9C)),
  p_value = step9C_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step9 to whichever model (9A-9C) had highest p-value
model_step9 <- model_9A  # Update based on anova results above

# Record Step 9 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "9_Selected", Model_Name = "model_step9", Interaction_Removed = "rm_type:correct_num",
  AIC = AIC(model_step9), BIC = BIC(model_step9), LogLik = as.numeric(logLik(model_step9)),
  p_value = step9A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 10: Test removal of remaining 2-way interactions from model_step9

# Test removal of spacing_category:correct_width
model_10A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                    correct_num:correct_width +
                    (1 | subID), data = df_exp, REML = FALSE)
step10A_test <- anova(model_10A, model_step9)
print("10A: Test removal of spacing_category:correct_width")
print(step10A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "10A", Model_Name = "model_10A", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_10A), BIC = BIC(model_10A), LogLik = as.numeric(logLik(model_10A)),
  p_value = step10A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Test removal of correct_num:correct_width
model_10B <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                    spacing_category:correct_width +
                    (1 | subID), data = df_exp, REML = FALSE)
step10B_test <- anova(model_10B, model_step9)
print("10B: Test removal of correct_num:correct_width")
print(step10B_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "10B", Model_Name = "model_10B", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_10B), BIC = BIC(model_10B), LogLik = as.numeric(logLik(model_10B)),
  p_value = step10B_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step10 to whichever model (10A or 10B) had highest p-value
model_step10 <- model_10B  # Update based on anova results above

# Record Step 10 selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "10_Selected", Model_Name = "model_step10", Interaction_Removed = "correct_num:correct_width",
  AIC = AIC(model_step10), BIC = BIC(model_step10), LogLik = as.numeric(logLik(model_step10)),
  p_value = step10B_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Step 11: Test removal of final 2-way interaction from model_step10

# Test removal of spacing_category:correct_width (main effects only model)
model_11A <- lmer(width_deviation ~ rm_type + spacing_category + correct_num + correct_width +
                    (1 | subID), data = df_exp, REML = FALSE)
step11A_test <- anova(model_11A, model_step10)
print("11A: Test removal of spacing_category:correct_width (main effects only)")
print(step11A_test)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "11A", Model_Name = "model_11A", Interaction_Removed = "spacing_category:correct_width",
  AIC = AIC(model_11A), BIC = BIC(model_11A), LogLik = as.numeric(logLik(model_11A)),
  p_value = step11A_test$`Pr(>Chisq)`[2], Decision = "Test", stringsAsFactors = FALSE
))

# Set model_step11 based on anova result above
model_step11 <- model_step10  # Best model and most parsimonious model main effects + spacing_category:correct_width

# Record Step 11 selection (keeping the interaction)
model_comparison <- rbind(model_comparison, data.frame(
  Step = "11_Selected", Model_Name = "model_step11", Interaction_Removed = "Keep_spacing_category:correct_width",
  AIC = AIC(model_step11), BIC = BIC(model_step11), LogLik = as.numeric(logLik(model_step11)),
  p_value = step11A_test$`Pr(>Chisq)`[2], Decision = "Selected", stringsAsFactors = FALSE
))

# Set your final model here after completing elimination steps
final_model <- model_step11  # Update this after running elimination

# Record final model selection
model_comparison <- rbind(model_comparison, data.frame(
  Step = "Final",
  Model_Name = "final_model",
  Interaction_Removed = "Complete_elimination",
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
  labs(title = "Model Selection: Systematic Interaction Elimination",
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
  labs(title = "Model Selection: BIC Progression",
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


write.csv(selected_models, "elimination_pathways/elimination_pathway_Exp1_width_deviation.csv", row.names = FALSE)


############### Primary RM vs NoRM comparison using emmeans ###############
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

############### Individual condition tests against null (μ = 0) ###############
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

# Create emmeans comparison plot
emmeans_plot_data <- summary(rm_emmeans)
emmeans_plot <- ggplot(emmeans_plot_data, aes(x = rm_type, y = emmean)) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, size = 1, color = "#2C3E50") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "RM vs NoRM: Width Deviation Comparison",
       subtitle = "Error bars show standard errors | Red line = no deviation",
       x = "Condition",
       y = "Width Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

print(emmeans_plot)

############### APA-STYLE RESULTS SUMMARY ###############
print("=== APA-STYLE RESULTS FOR PUBLICATION ===")

# Extract key statistics for APA reporting
contrast_results <- summary(rm_contrast)
contrast_z <- contrast_results$z.ratio[1]  # Use z.ratio when df = Inf
contrast_df <- contrast_results$df[1]
contrast_p <- contrast_results$p.value[1]
contrast_diff <- contrast_results$estimate[1]
contrast_se <- contrast_results$SE[1]
# Use normal distribution for CI since df = Inf
contrast_ci_lower <- contrast_diff - qnorm(0.975) * contrast_se
contrast_ci_upper <- contrast_diff + qnorm(0.975) * contrast_se

cat("\n=== COMPLETE APA-FORMATTED RESULTS ===\n\n")

cat("MODEL SELECTION:\n")
cat("Backward elimination from full factorial model retained main effects plus\n")
cat("spacing_category:correct_width interaction (final model: AIC = -13979.89).\n\n")

cat("PRIMARY HYPOTHESIS TEST (RM vs NoRM):\n")
cat(sprintf("RM trials (M = %.3f, SE = %.3f) differed significantly from NoRM trials (M = %.3f, SE = %.3f),\n", 
            rm_means_summary[2, "emmean"], rm_means_summary[2, "SE"], 
            rm_means_summary[1, "emmean"], rm_means_summary[1, "SE"]))
cat(sprintf("z = %.2f, p = %.3f, d = %.3f, 95%% CI [%.3f, %.3f].\n\n",
            contrast_z, contrast_p, effect_size, 
            contrast_ci_lower, contrast_ci_upper))

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

cat("\n=== ANALYSIS COMPLETE ===\n")
