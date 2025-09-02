# Full Factorial Model Analysis - Experiment 1A
# Redundancy Masking (RM) vs NoRM Analysis

# Libraries
library(tidyverse)
library(lme4)
library(emmeans)

# Data Preparation
df <- read.csv("G:/My Drive/Projects/RM/RM_loss_n_gain/datasets/processed.csv")

df_exp <- df %>%
  filter(exp_version == "Exp1A") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    spacing_category = case_when(
      correct_space <= quantile(correct_space, 1/3) ~ "Smaller",
      correct_space <= quantile(correct_space, 2/3) ~ "Middle",
      TRUE ~ "Larger"
    ),
    rm_type = factor(ifelse(number_deviation == -1, "RM", "NoRM"), 
                     levels = c("NoRM", "RM")),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(spacing_category, 
                              levels = c("Smaller", "Middle", "Larger")),
    subID = factor(subID)
  )

# Full Factorial Model
full_model <- lmer(width_deviation ~ rm_type * spacing_category * correct_num * correct_width +
                   (1 | subID), 
                   data = df_exp, REML = FALSE)

print("=== FULL MODEL SUMMARY ===")
summary(full_model)

# Primary Analysis: RM vs NoRM Marginal Means
print("\n=== PRIMARY ANALYSIS: RM vs NoRM ===")
rm_emmeans <- emmeans(full_model, ~ rm_type)
print(rm_emmeans)

rm_contrast <- pairs(rm_emmeans)
print(rm_contrast)

# Effect size
rm_means <- summary(rm_emmeans)
effect_size <- diff(rm_means$emmean) / sigma(full_model)
print(paste("Cohen's d:", round(effect_size, 3)))

# Individual condition tests vs zero
print("\n=== INDIVIDUAL CONDITIONS vs ZERO ===")
rm_means_summary <- summary(rm_emmeans)

# RM vs zero
rm_mean <- rm_means_summary[2, "emmean"]
rm_se <- rm_means_summary[2, "SE"]
rm_z <- rm_mean / rm_se
rm_p <- 2 * pnorm(abs(rm_z), lower.tail = FALSE)
print(sprintf("RM vs zero: M=%.4f, SE=%.4f, z=%.3f, p=%.3f", rm_mean, rm_se, rm_z, rm_p))

# NoRM vs zero
norm_mean <- rm_means_summary[1, "emmean"]
norm_se <- rm_means_summary[1, "SE"]
norm_z <- norm_mean / norm_se
norm_p <- 2 * pnorm(abs(norm_z), lower.tail = FALSE)
print(sprintf("NoRM vs zero: M=%.4f, SE=%.4f, z=%.3f, p=%.3f", norm_mean, norm_se, norm_z, norm_p))

# RM Effects by Key Factors
print("\n=== RM EFFECTS BY FACTORS ===")

# By spacing category
print("RM by spacing category:")
rm_by_spacing <- emmeans(full_model, ~ rm_type | spacing_category)
print(pairs(rm_by_spacing))

# By correct number
print("\nRM by correct number:")
rm_by_num <- emmeans(full_model, ~ rm_type | correct_num)
print(pairs(rm_by_num))

# By correct width
print("\nRM by correct width:")
rm_by_width <- emmeans(full_model, ~ rm_type | correct_width)
print(pairs(rm_by_width))

# Complex interactions
print("\n=== COMPLEX INTERACTIONS ===")

# RM by spacing + width
print("RM by spacing category x correct width:")
rm_spacing_width <- emmeans(full_model, ~ rm_type | spacing_category * correct_width)
rm_spacing_width_contrasts <- pairs(rm_spacing_width)
print(rm_spacing_width_contrasts)

# RM by number + width  
print("\nRM by correct number x correct width:")
rm_num_width <- emmeans(full_model, ~ rm_type | correct_num * correct_width)
rm_num_width_contrasts <- pairs(rm_num_width)
print(rm_num_width_contrasts)

# Export Results
if (!dir.exists("full_models/analysis_results")) {
  dir.create("full_models/analysis_results", recursive = TRUE)
}

# Primary results
write.csv(summary(rm_contrast), "full_models/analysis_results/Exp1A_primary_RM_contrast.csv", row.names = FALSE)

# Complex interaction results
write.csv(summary(rm_spacing_width_contrasts), "full_models/analysis_results/Exp1A_RM_by_spacing_width.csv", row.names = FALSE)
write.csv(summary(rm_num_width_contrasts), "full_models/analysis_results/Exp1A_RM_by_num_width.csv", row.names = FALSE)

print("\n=== ANALYSIS COMPLETE ===")
print("Results exported to full_models/analysis_results/ folder")