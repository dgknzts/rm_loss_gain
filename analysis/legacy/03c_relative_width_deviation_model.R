# ==============================================================================
# Bayesian Analysis of Width Deviation in Redundancy Masking - Using brms
# ==============================================================================

# --- Setup ---
library(tidyverse)
library(brms)
library(emmeans)
library(bayestestR)
library(ggplot2)

# --- Data Import and Preprocessing ---
df <- read.csv('data/processed.csv')
spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'NoRM'), levels = c('NoRM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  filter(is.finite(width_deviation_relative))

# --- Set Priors ---
# brms uses different prior syntax
priors <- c(
  # Fixed effects (all regression coefficients)
  prior(student_t(3, 0, 0.707), class = b),
  # Random effects SD
  prior(student_t(3, 0, 2.5), class = sd),
  # Residual SD
  prior(student_t(3, 0, 2.5), class = sigma)
)

# --- Bayesian Model Fitting ---
model <- brm(
  width_deviation_relative ~ 0 + rm_type * spacing_category * correct_num * correct_width + (1 | subID),
  data = analysis_data,
  prior = priors,
  family = gaussian(),
  sample_prior = "yes",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 11,
  control = list(adapt_delta = 0.95),
  seed = 2025
)

# Check model convergence
summary(model)

# Plot diagnostics
plot(model)

# Posterior predictive checks
brms::pp_check(model, ndraws=100) + theme_minimal()

# Posterior predictive checks for groups
brms::pp_check(model, 
               type = "dens_overlay_grouped", 
               group = "rm_type",
               ndraws = 50) + theme_minimal()

# --- Extract Posterior Results ---
# Overall marginal means for RM vs NoRM
emm_overall <- emmeans(model, ~ rm_type)

emm_posteriors <- emmeans(model, ~ rm_type) %>%
  gather_emmeans_draws()

# Get posterior summaries
posterior_means <- summary(emm_overall, point.est = 'median', type = 'HPD', level = 0.95) %>%
  as_tibble() %>%
  rename(median = emmean, lower = lower.HPD, upper = upper.HPD)

# --- Bayes Factors --
# Using brms built-in hypothesis testing
# You can  test specific hypotheses directly
h1 <- hypothesis(model, "rm_typeRM = 0")
h1


h2 <- hypothesis(model, "rm_typeNoRM = 0")  # This tests NoRM vs 0 since NoRM is reference level
h2


# Extract BF from hypothesis tests
bf_rm_vs_zero <- h1$hypothesis$Evid.Ratio
bf_rm_vs_zero
bf_norm_vs_zero <- h2$hypothesis$Evid.Ratio
bf_norm_vs_zero

# Create BF summary
bf_summary <- tibble(
  condition = c("NoRM", "RM"),
  BF_method2 = c(bf_norm_vs_zero, bf_rm_vs_zero)
)

# --- Create Output Directory ---
if (!dir.exists('outputs/tables')) {
  dir.create('outputs/tables', recursive = TRUE)
}
if (!dir.exists('outputs/plots')) {
  dir.create('outputs/plots', recursive = TRUE)
}

# --- Save Results ---
write.csv(posterior_means, 'outputs/tables/width_deviation_posterior_means.csv', row.names = FALSE)
write.csv(bf_means, 'outputs/tables/width_deviation_bayes_vs_zero.csv', row.names = FALSE)
write.csv(bf_summary, 'outputs/tables/width_deviation_bayes_hypothesis.csv', row.names = FALSE)

# --- Professional Bayesian Plots ---
# Plot data preparation
plot_data <- posterior_means %>%
  left_join(bf_means %>% select(condition, BF), by = c("rm_type" = "condition")) %>%
  mutate(
    bf_interpretation = case_when(
      BF < 1 ~ "Evidence for H₀",
      BF < 3 ~ "Weak evidence",
      BF < 10 ~ "Moderate evidence", 
      BF < 30 ~ "Strong evidence",
      BF < 100 ~ "Very strong evidence",
      TRUE ~ "Extreme evidence"
    )
  )

# Plot 1: Forest plot showing both conditions
p1 <- plot_data %>%
  ggplot(aes(y = rm_type, x = median, color = rm_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6, size = 0.8) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 1.5, linewidth = 1.5) +
  geom_text(aes(label = sprintf("BF = %.1f", BF)), 
            x = 0.05, hjust = 0, vjust = -0.5, 
            size = 4, fontface = "bold") +
  scale_color_manual(values = c("NoRM" = "#2E86AB", "RM" = "#A23B72")) +
  scale_y_discrete(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title = "Width Deviation by Condition",
    subtitle = "Median estimates with 95% HDI and Bayes factors vs. zero",
    x = "Width Deviation (Relative to True Width)",
    y = "Condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# Individual condition plots
p2_norm <- plot_data %>%
  filter(rm_type == "NoRM") %>%
  ggplot(aes(x = median, y = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6, size = 1) +
  geom_point(size = 4, color = "#2E86AB") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, size = 2, color = "#2E86AB") +
  geom_text(aes(label = sprintf("BF = %.1f\n%s", BF, bf_interpretation)), 
            x = Inf, y = 1, hjust = 1.1, vjust = 0.5, 
            fontface = "bold", size = 4) +
  ylim(0.5, 1.5) +
  labs(
    title = "NoRM Condition",
    subtitle = "Width perception without redundancy masking",
    x = "Width Deviation (Relative to True Width)",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#2E86AB"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p2_rm <- plot_data %>%
  filter(rm_type == "RM") %>%
  ggplot(aes(x = median, y = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6, size = 1) +
  geom_point(size = 4, color = "#A23B72") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, size = 2, color = "#A23B72") +
  geom_text(aes(label = sprintf("BF = %.1f\n%s", BF, bf_interpretation)), 
            x = Inf, y = 1, hjust = 1.1, vjust = 0.5, 
            fontface = "bold", size = 4) +
  ylim(0.5, 1.5) +
  labs(
    title = "RM Condition", 
    subtitle = "Width perception with redundancy masking",
    x = "Width Deviation (Relative to True Width)",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#A23B72"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Save plots
ggsave('outputs/plots/forest_plot.pdf', p1, width = 10, height = 6, dpi = 300)
ggsave('outputs/plots/forest_plot.png', p1, width = 10, height = 6, dpi = 300)

ggsave('outputs/plots/norm_condition.pdf', p2_norm, width = 8, height = 4, dpi = 300)
ggsave('outputs/plots/norm_condition.png', p2_norm, width = 8, height = 4, dpi = 300)

ggsave('outputs/plots/rm_condition.pdf', p2_rm, width = 8, height = 4, dpi = 300)
ggsave('outputs/plots/rm_condition.png', p2_rm, width = 8, height = 4, dpi = 300)

# --- Print Summary ---
cat("\nBAYESIAN ANALYSIS SUMMARY (brms)\n")
cat("="*35 + "\n")

cat("\nModel Summary:\n")
print(model, digits = 4)

cat("\nPosterior Means (95% HDI):\n")
for(i in 1:nrow(posterior_means)) {
  row <- posterior_means[i,]
  cat(sprintf("%s: %.4f [%.4f, %.4f]\n", 
              row$rm_type, row$median, row$lower, row$upper))
}

cat("\nBayes Factors (vs. zero):\n")
for(i in 1:nrow(bf_means)) {
  row <- bf_means[i,]
  cat(sprintf("%s: BF = %.2f (%s)\n", row$condition, row$BF, 
              case_when(
                row$BF < 1 ~ "Evidence for H₀",
                row$BF < 3 ~ "Weak evidence",
                row$BF < 10 ~ "Moderate evidence", 
                row$BF < 30 ~ "Strong evidence",
                row$BF < 100 ~ "Very strong evidence",
                TRUE ~ "Extreme evidence"
              )))
}

cat("\nAnalysis complete!\n")