# Density Category Histogram - Experiment 1
# Showing how actual density values are split into Low/Medium/High categories

library(tidyverse)

source("analysis/functions/themes.R")

density_colors <- c("Low" = "#6BAED6", "Medium" = "#4292C6", "High" = "#2171B5")

# =============================================================================
# LOAD DATA
# =============================================================================

df <- read.csv("data/exp1/processed.csv") %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  drop_na(actual_width_density)

# Get tertile cutoffs
tertiles <- quantile(df$actual_width_density, probs = c(1/3, 2/3))
cat("Tertile cutoffs:", round(tertiles, 3), "\n")

# Assign categories
df <- df %>%
  mutate(
    density_category = case_when(
      actual_width_density <= tertiles[1] ~ "Low",
      actual_width_density <= tertiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    density_category = factor(density_category, levels = c("Low", "Medium", "High"))
  )

# Count trials per category
trial_counts <- df %>%
  count(density_category)
print(trial_counts)

# Create legend labels with counts
legend_labels <- c(
  "Low" = sprintf("Low (n = %d)", trial_counts$n[trial_counts$density_category == "Low"]),
  "Medium" = sprintf("Medium (n = %d)", trial_counts$n[trial_counts$density_category == "Medium"]),
  "High" = sprintf("High (n = %d)", trial_counts$n[trial_counts$density_category == "High"])
)

# =============================================================================
# HISTOGRAM PLOT
# =============================================================================

p <- ggplot(df, aes(x = actual_width_density, fill = density_category)) +
  geom_histogram(binwidth = 0.02, color = "grey30", linewidth = 0.3, alpha = 0.8) +
  geom_vline(xintercept = tertiles[1], linetype = "dashed", linewidth = 0.8, color = "black") +
  geom_vline(xintercept = tertiles[2], linetype = "dashed", linewidth = 0.8, color = "black") +
  scale_fill_manual(values = density_colors, labels = legend_labels, name = "Category") +
  labs(
    x = "Actual Density",
    y = "Count"
  ) +
  theme_scientific() +
  theme(
    legend.position = c(0.85, 0.85),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

print(p)

ggsave("outputs/exp1/figures/density_category_histogram.png", p,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave("outputs/exp1/figures/density_category_histogram.svg", p,
       width = 6, height = 4, bg = "white")

cat("Saved: density_category_histogram.png\n")
