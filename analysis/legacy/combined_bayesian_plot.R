# Combined Bayesian Density Plot - Exp1 and Exp2
# Panel A: Experiment 1 | Panel B: Experiment 2
# Fixed axes for comparison

library(tidyverse)
library(ggplot2)
library(patchwork)
library(BayesFactor)

source("analysis/functions/themes.R")

rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# =============================================================================
# EXPERIMENT 1: BAYESIAN ANALYSIS
# =============================================================================

df_exp1 <- read.csv("data/exp1/processed.csv")

exp1_data <- df_exp1 %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM'))
  ) %>%
  drop_na(width_deviation_relative)

# Subject means for exp1
exp1_subject_means <- exp1_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_val = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop")

exp1_rm_means <- exp1_subject_means %>% filter(rm_type == "RM") %>% pull(mean_val)
exp1_norm_means <- exp1_subject_means %>% filter(rm_type == "Non-RM") %>% pull(mean_val)

# Bayesian t-tests exp1
bf_exp1_rm <- ttestBF(x = exp1_rm_means, mu = 0)
bf_exp1_norm <- ttestBF(x = exp1_norm_means, mu = 0)

# Posterior samples exp1
post_exp1_rm <- posterior(bf_exp1_rm, iterations = 10000)
post_exp1_norm <- posterior(bf_exp1_norm, iterations = 10000)

delta_exp1_rm <- as.numeric(post_exp1_rm[, "delta"])
delta_exp1_norm <- as.numeric(post_exp1_norm[, "delta"])

# Stats exp1
median_exp1_rm <- median(delta_exp1_rm)
median_exp1_norm <- median(delta_exp1_norm)
bf01_exp1_rm <- 1 / exp(bf_exp1_rm@bayesFactor$bf)
bf01_exp1_norm <- 1 / exp(bf_exp1_norm@bayesFactor$bf)

# Density at zero exp1
dens_exp1_rm <- density(delta_exp1_rm, adjust = 1.2)
dens_exp1_norm <- density(delta_exp1_norm, adjust = 1.2)
dens_exp1_rm_at_zero <- dens_exp1_rm$y[which.min(abs(dens_exp1_rm$x - 0))]
dens_exp1_norm_at_zero <- dens_exp1_norm$y[which.min(abs(dens_exp1_norm$x - 0))]

cat("=== Experiment 1 ===\n")
cat(sprintf("RM: BF01 = %.4f, median delta = %.3f\n", bf01_exp1_rm, median_exp1_rm))
cat(sprintf("Non-RM: BF01 = %.4f, median delta = %.3f\n", bf01_exp1_norm, median_exp1_norm))

# =============================================================================
# EXPERIMENT 2: BAYESIAN ANALYSIS
# =============================================================================

df_exp2 <- read.csv("data/exp2/processed.csv")

exp2_data <- df_exp2 %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM'))
  ) %>%
  drop_na(width_deviation_relative)

# Subject means for exp2
exp2_subject_means <- exp2_data %>%
  group_by(subID, rm_type) %>%
  summarise(mean_val = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop")

exp2_rm_means <- exp2_subject_means %>% filter(rm_type == "RM") %>% pull(mean_val)
exp2_norm_means <- exp2_subject_means %>% filter(rm_type == "Non-RM") %>% pull(mean_val)

# Bayesian t-tests exp2
bf_exp2_rm <- ttestBF(x = exp2_rm_means, mu = 0)
bf_exp2_norm <- ttestBF(x = exp2_norm_means, mu = 0)

# Posterior samples exp2
post_exp2_rm <- posterior(bf_exp2_rm, iterations = 10000)
post_exp2_norm <- posterior(bf_exp2_norm, iterations = 10000)

delta_exp2_rm <- as.numeric(post_exp2_rm[, "delta"])
delta_exp2_norm <- as.numeric(post_exp2_norm[, "delta"])

# Stats exp2
median_exp2_rm <- median(delta_exp2_rm)
median_exp2_norm <- median(delta_exp2_norm)
bf01_exp2_rm <- 1 / exp(bf_exp2_rm@bayesFactor$bf)
bf01_exp2_norm <- 1 / exp(bf_exp2_norm@bayesFactor$bf)

# Density at zero exp2
dens_exp2_rm <- density(delta_exp2_rm, adjust = 1.2)
dens_exp2_norm <- density(delta_exp2_norm, adjust = 1.2)
dens_exp2_rm_at_zero <- dens_exp2_rm$y[which.min(abs(dens_exp2_rm$x - 0))]
dens_exp2_norm_at_zero <- dens_exp2_norm$y[which.min(abs(dens_exp2_norm$x - 0))]

cat("\n=== Experiment 2 ===\n")
cat(sprintf("RM: BF01 = %.4f, median delta = %.3f\n", bf01_exp2_rm, median_exp2_rm))
cat(sprintf("Non-RM: BF01 = %.4f, median delta = %.3f\n", bf01_exp2_norm, median_exp2_norm))

# =============================================================================
# CALCULATE GLOBAL AXIS LIMITS
# =============================================================================

all_deltas <- c(delta_exp1_rm, delta_exp1_norm, delta_exp2_rm, delta_exp2_norm)
x_range <- c(min(all_deltas), max(all_deltas))
x_lim <- c(floor(x_range[1] * 2) / 2, ceiling(x_range[2] * 2) / 2)  # Round to 0.5

# For y-axis, get max density
all_densities <- c(
  max(dens_exp1_rm$y), max(dens_exp1_norm$y),
  max(dens_exp2_rm$y), max(dens_exp2_norm$y),
  dcauchy(0, 0, 0.707)  # Prior peak
)
y_lim <- c(0, ceiling(max(all_densities) * 10) / 10 + 0.1)

cat(sprintf("\nGlobal X limits: [%.2f, %.2f]\n", x_lim[1], x_lim[2]))
cat(sprintf("Global Y limits: [%.2f, %.2f]\n", y_lim[1], y_lim[2]))

# =============================================================================
# PREPARE PLOT DATA
# =============================================================================

plot_data_exp1 <- data.frame(
  delta = c(delta_exp1_rm, delta_exp1_norm),
  condition = factor(rep(c("RM", "Non-RM"), c(length(delta_exp1_rm), length(delta_exp1_norm))),
                     levels = c("Non-RM", "RM"))
)

plot_data_exp2 <- data.frame(
  delta = c(delta_exp2_rm, delta_exp2_norm),
  condition = factor(rep(c("RM", "Non-RM"), c(length(delta_exp2_rm), length(delta_exp2_norm))),
                     levels = c("Non-RM", "RM"))
)

prior_density_at_zero <- dcauchy(0, location = 0, scale = 0.707)

# =============================================================================
# PANEL A: EXPERIMENT 1
# =============================================================================

panel_a <- ggplot(plot_data_exp1, aes(x = delta)) +
  # Prior distribution
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area", fill = "grey40", alpha = 0.3, n = 1000
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey30", linetype = "dashed", linewidth = 1, n = 1000
  ) +
  # Posterior densities
  geom_density(aes(color = condition), linewidth = 1.2, adjust = 1.2) +
  # Points at zero
  geom_point(
    data = tibble(x = 0, y = dens_exp1_rm_at_zero, condition = factor("RM")),
    aes(x = x, y = y, color = condition),
    size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = dens_exp1_norm_at_zero, condition = factor("Non-RM")),
    aes(x = x, y = y, color = condition),
    size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = prior_density_at_zero),
    aes(x = x, y = y),
    size = 3, shape = 21, fill = "white", color = "grey40", stroke = 1.5
  ) +
  # Reference lines
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = median_exp1_rm, color = rm_colors["RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  geom_vline(xintercept = median_exp1_norm, color = rm_colors["Non-RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  # BF annotations
 annotate("text", x = median_exp1_rm + 0.1, y = y_lim[2] * 0.85,
           label = sprintf("BF01 = %.3f", bf01_exp1_rm),
           color = rm_colors["RM"], fontface = "bold", size = 3.5, hjust = 0) +
  annotate("text", x = median_exp1_norm - 0.1, y = y_lim[2] * 0.75,
           label = sprintf("BF01 = %.4f", bf01_exp1_norm),
           color = rm_colors["Non-RM"], fontface = "bold", size = 3.5, hjust = 1) +
  # Scales and labels
  scale_color_manual(values = rm_colors, guide = "none") +
  scale_x_continuous(limits = x_lim) +
  scale_y_continuous(limits = y_lim) +
  labs(
    title = "Experiment 1",
    subtitle = sprintf("N = %d participants", length(unique(exp1_data$subID))),
    x = "Effect size (δ)",
    y = "Density"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================================
# PANEL B: EXPERIMENT 2
# =============================================================================

panel_b <- ggplot(plot_data_exp2, aes(x = delta)) +
  # Prior distribution
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    geom = "area", fill = "grey40", alpha = 0.3, n = 1000
  ) +
  stat_function(
    fun = function(x) dcauchy(x, location = 0, scale = 0.707),
    color = "grey30", linetype = "dashed", linewidth = 1, n = 1000
  ) +
  # Posterior densities
  geom_density(aes(color = condition), linewidth = 1.2, adjust = 1.2) +
  # Points at zero
  geom_point(
    data = tibble(x = 0, y = dens_exp2_rm_at_zero, condition = factor("RM")),
    aes(x = x, y = y, color = condition),
    size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = dens_exp2_norm_at_zero, condition = factor("Non-RM")),
    aes(x = x, y = y, color = condition),
    size = 3, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_point(
    data = tibble(x = 0, y = prior_density_at_zero),
    aes(x = x, y = y),
    size = 3, shape = 21, fill = "white", color = "grey40", stroke = 1.5
  ) +
  # Reference lines
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = median_exp2_rm, color = rm_colors["RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  geom_vline(xintercept = median_exp2_norm, color = rm_colors["Non-RM"],
             linetype = "dotted", linewidth = 1.2, alpha = 0.8) +
  # BF annotations
  annotate("text", x = median_exp2_rm + 0.1, y = y_lim[2] * 0.85,
           label = sprintf("BF01 = %.3f", bf01_exp2_rm),
           color = rm_colors["RM"], fontface = "bold", size = 3.5, hjust = 0) +
  annotate("text", x = median_exp2_norm - 0.1, y = y_lim[2] * 0.75,
           label = sprintf("BF01 = %.4f", bf01_exp2_norm),
           color = rm_colors["Non-RM"], fontface = "bold", size = 3.5, hjust = 1) +
  # Scales and labels
  scale_color_manual(values = rm_colors, guide = "none") +
  scale_x_continuous(limits = x_lim) +
  scale_y_continuous(limits = y_lim) +
  labs(
    title = "Experiment 2",
    subtitle = sprintf("N = %d participants", length(unique(exp2_data$subID))),
    x = "Effect size (δ)",
    y = "Density"
  ) +
  theme_scientific(base_size = 11) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# =============================================================================
# COMBINE PANELS
# =============================================================================

combined_plot <- (panel_a / panel_b) +
  plot_annotation(
    title = "Bayesian One-Sample T-Tests: Relative Width Deviation",
    subtitle = "Grey = Prior | Green = Non-RM | Red = RM",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0.5),
      plot.tag = element_text(size = 12, face = "bold")
    )
  )

print(combined_plot)

# =============================================================================
# SAVE OUTPUT
# =============================================================================

if (!dir.exists('outputs/combined')) {
  dir.create('outputs/combined', recursive = TRUE)
}

ggsave("outputs/combined/bayesian_exp1_exp2_combined.png",
       combined_plot, width = 11, height = 8, dpi = 300, bg = "white")

cat("\nSaved: outputs/combined/bayesian_exp1_exp2_combined.png\n")
