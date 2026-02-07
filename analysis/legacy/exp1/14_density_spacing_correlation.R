# Width Density-Spacing Correlation Analysis - Participant Level Only
# Examines relationship between participant-level width density and spacing deviation means

library(tidyverse)
library(ggplot2)

source("analysis/functions/themes.R")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

calc_corr_stats <- function(data, x_var, y_var, label = "") {
  x <- data[[x_var]]
  y <- data[[y_var]]

  valid_idx <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  x <- x[valid_idx]
  y <- y[valid_idx]

  if (length(x) < 3) {
    return(list(r = NA, p = NA, n = length(x), ci_lower = NA, ci_upper = NA, label = label))
  }

  test <- cor.test(x, y, method = "pearson")

  list(
    r = test$estimate,
    p = test$p.value,
    n = length(x),
    ci_lower = test$conf.int[1],
    ci_upper = test$conf.int[2],
    label = label
  )
}

format_corr_label <- function(stats) {
  if (is.na(stats$r)) return("r = NA")
  sprintf("r = %.3f, p %s\nn = %d",
          stats$r,
          if (stats$p < 0.001) "< .001" else sprintf("= %.3f", stats$p),
          stats$n)
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

df <- read.csv('data/exp1/processed.csv')

spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(width_density_deviation, spacing_deviation) %>%
  filter(is.finite(width_density_deviation))

cat(sprintf("Density-Spacing Correlation analysis: %d trials from %d participants\n",
            nrow(analysis_data), length(unique(analysis_data$subID))))

# Colors
rm_colors <- c("Non-RM" = "#2E8B57", "RM" = "#DC143C")

# Create output directory
if (!dir.exists('outputs/exp1/figures')) {
  dir.create('outputs/exp1/figures', recursive = TRUE)
}

# =============================================================================
# FIGURE 1: OVERALL PARTICIPANT-LEVEL CORRELATION
# =============================================================================

cat("\n=== FIGURE 1: OVERALL DENSITY-SPACING CORRELATION ===\n")

participant_means_overall <- analysis_data %>%
  group_by(subID, rm_type) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

stats_overall_rm <- calc_corr_stats(
  participant_means_overall %>% filter(rm_type == "RM"),
  "mean_density_dev", "mean_spacing_dev", "RM"
)

stats_overall_norm <- calc_corr_stats(
  participant_means_overall %>% filter(rm_type == "Non-RM"),
  "mean_density_dev", "mean_spacing_dev", "Non-RM"
)

cat(sprintf("RM: %s\n", format_corr_label(stats_overall_rm)))
cat(sprintf("Non-RM: %s\n", format_corr_label(stats_overall_norm)))

plot1 <- ggplot(participant_means_overall, aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 2,
           label = format_corr_label(stats_overall_rm),
           color = rm_colors["RM"], size = 3.5, fontface = "bold") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 5,
           label = format_corr_label(stats_overall_norm),
           color = rm_colors["Non-RM"], size = 3.5, fontface = "bold") +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation: Overall Participant Means",
    subtitle = "Each point = one participant's mean across all conditions",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_blank()
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_overall.png",
       plot1, width = 7, height = 6, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_overall.png\n")

# =============================================================================
# FIGURE 2: PARTICIPANT-LEVEL CORRELATION BY SET SIZE
# =============================================================================

cat("\n=== FIGURE 2: DENSITY-SPACING CORRELATION BY SET SIZE ===\n")

participant_means_setsize <- analysis_data %>%
  group_by(subID, rm_type, correct_num) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_setsize <- participant_means_setsize %>%
  group_by(correct_num, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_setsize)) {
  cat(sprintf("Set size %s, %s: %s\n",
              corr_setsize$correct_num[i],
              corr_setsize$rm_type[i],
              gsub("\n", ", ", corr_setsize$label[i])))
}

plot2 <- ggplot(participant_means_setsize, aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_text(
    data = corr_setsize %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.8, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_setsize %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.8, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_wrap(~correct_num, labeller = as_labeller(function(x) paste("Set Size:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by Set Size (Participant Means)",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_setsize.png",
       plot2, width = 11, height = 4, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_setsize.png\n")

# =============================================================================
# FIGURE 3: PARTICIPANT-LEVEL CORRELATION BY TRUE WIDTH
# =============================================================================

cat("\n=== FIGURE 3: DENSITY-SPACING CORRELATION BY TRUE WIDTH ===\n")

participant_means_width <- analysis_data %>%
  group_by(subID, rm_type, correct_width) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_width <- participant_means_width %>%
  group_by(correct_width, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_width)) {
  cat(sprintf("Width %s, %s: %s\n",
              corr_width$correct_width[i],
              corr_width$rm_type[i],
              gsub("\n", ", ", corr_width$label[i])))
}

plot3 <- ggplot(participant_means_width, aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_text(
    data = corr_width %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.8, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_width %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.8, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "°"))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by True Width (Participant Means)",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_width.png",
       plot3, width = 11, height = 4, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_width.png\n")

# =============================================================================
# FIGURE 4: PARTICIPANT-LEVEL CORRELATION BY SPACING CATEGORY
# =============================================================================

cat("\n=== FIGURE 4: DENSITY-SPACING CORRELATION BY SPACING CATEGORY ===\n")

participant_means_spacing <- analysis_data %>%
  group_by(subID, rm_type, spacing_category) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_spacing <- participant_means_spacing %>%
  group_by(spacing_category, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_spacing)) {
  cat(sprintf("Spacing %s, %s: %s\n",
              corr_spacing$spacing_category[i],
              corr_spacing$rm_type[i],
              gsub("\n", ", ", corr_spacing$label[i])))
}

plot4 <- ggplot(participant_means_spacing, aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_text(
    data = corr_spacing %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.8, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_spacing %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.8, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_wrap(~spacing_category, labeller = as_labeller(function(x) paste("Spacing:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by Spacing Category (Participant Means)",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 12, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_spacing.png",
       plot4, width = 11, height = 4, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_spacing.png\n")

# =============================================================================
# FIGURE 5: PARTICIPANT-LEVEL CORRELATION BY WIDTH × SET SIZE
# =============================================================================

cat("\n=== FIGURE 5: DENSITY-SPACING CORRELATION BY WIDTH × SET SIZE ===\n")

participant_means_width_setsize <- analysis_data %>%
  group_by(subID, rm_type, correct_width, correct_num) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_width_setsize <- participant_means_width_setsize %>%
  group_by(correct_width, correct_num, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_width_setsize)) {
  cat(sprintf("Width %s, Set size %s, %s: %s\n",
              corr_width_setsize$correct_width[i],
              corr_width_setsize$correct_num[i],
              corr_width_setsize$rm_type[i],
              gsub("\n", ", ", corr_width_setsize$label[i])))
}

plot5 <- ggplot(participant_means_width_setsize,
                aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  geom_text(
    data = corr_width_setsize %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.3, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_width_setsize %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.3, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_grid(correct_width ~ correct_num,
             labeller = labeller(
               correct_width = function(x) paste("Width:", x, "°"),
               correct_num = function(x) paste("Set Size:", x)
             )) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by True Width × Set Size",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_width_setsize.png",
       plot5, width = 11, height = 9, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_width_setsize.png\n")

# =============================================================================
# FIGURE 6: PARTICIPANT-LEVEL CORRELATION BY WIDTH × SPACING
# =============================================================================

cat("\n=== FIGURE 6: DENSITY-SPACING CORRELATION BY WIDTH × SPACING ===\n")

participant_means_width_spacing <- analysis_data %>%
  group_by(subID, rm_type, correct_width, spacing_category) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_width_spacing <- participant_means_width_spacing %>%
  group_by(correct_width, spacing_category, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_width_spacing)) {
  cat(sprintf("Width %s, Spacing %s, %s: %s\n",
              corr_width_spacing$correct_width[i],
              corr_width_spacing$spacing_category[i],
              corr_width_spacing$rm_type[i],
              gsub("\n", ", ", corr_width_spacing$label[i])))
}

plot6 <- ggplot(participant_means_width_spacing,
                aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  geom_text(
    data = corr_width_spacing %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.3, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_width_spacing %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.3, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_grid(correct_width ~ spacing_category,
             labeller = labeller(
               correct_width = function(x) paste("Width:", x, "°"),
               spacing_category = function(x) paste("Spacing:", x)
             )) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by True Width × Spacing Category",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_width_spacing.png",
       plot6, width = 11, height = 9, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_width_spacing.png\n")

# =============================================================================
# FIGURE 7: PARTICIPANT-LEVEL CORRELATION BY SPACING × SET SIZE
# =============================================================================

cat("\n=== FIGURE 7: DENSITY-SPACING CORRELATION BY SPACING × SET SIZE ===\n")

participant_means_spacing_setsize <- analysis_data %>%
  group_by(subID, rm_type, spacing_category, correct_num) %>%
  summarise(
    mean_density_dev = mean(width_density_deviation, na.rm = TRUE),
    mean_spacing_dev = mean(spacing_deviation, na.rm = TRUE),
    .groups = "drop"
  )

corr_spacing_setsize <- participant_means_spacing_setsize %>%
  group_by(spacing_category, correct_num, rm_type) %>%
  group_modify(~{
    stats <- calc_corr_stats(.x, "mean_density_dev", "mean_spacing_dev")
    tibble(
      r = stats$r, p = stats$p, n = stats$n,
      label = format_corr_label(stats)
    )
  }) %>%
  ungroup()

for (i in 1:nrow(corr_spacing_setsize)) {
  cat(sprintf("Spacing %s, Set size %s, %s: %s\n",
              corr_spacing_setsize$spacing_category[i],
              corr_spacing_setsize$correct_num[i],
              corr_spacing_setsize$rm_type[i],
              gsub("\n", ", ", corr_spacing_setsize$label[i])))
}

plot7 <- ggplot(participant_means_spacing_setsize,
                aes(x = mean_density_dev, y = mean_spacing_dev, color = rm_type)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  geom_text(
    data = corr_spacing_setsize %>% filter(rm_type == "RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5, size = 2.3, fontface = "bold",
    color = rm_colors["RM"], inherit.aes = FALSE
  ) +
  geom_text(
    data = corr_spacing_setsize %>% filter(rm_type == "Non-RM"),
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 4, size = 2.3, fontface = "bold",
    color = rm_colors["Non-RM"], inherit.aes = FALSE
  ) +
  facet_grid(spacing_category ~ correct_num,
             labeller = labeller(
               spacing_category = function(x) paste("Spacing:", x),
               correct_num = function(x) paste("Set Size:", x)
             )) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(
    title = "Width Density-Spacing Correlation by Spacing Category × Set Size",
    x = "Mean Width Density Deviation",
    y = "Mean Spacing Deviation (°)"
  ) +
  theme_scientific(base_size = 11, base_family = "Arial") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/exp1/figures/density_spacing_correlation_participant_by_spacing_setsize.png",
       plot7, width = 11, height = 9, dpi = 300, bg = "white")
cat("Saved: density_spacing_correlation_participant_by_spacing_setsize.png\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("7 PNG figures saved to outputs/exp1/figures/\n")
