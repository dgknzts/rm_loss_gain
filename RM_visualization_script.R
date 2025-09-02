# Soft-Coded RM Visualization Script

# PARAMETERS ================================================================

# Experiment selection (easily modifiable)
EXPERIMENT_VERSION <- "Exp1A"  # Change to "Exp1B" or "Exp1C" as needed

# Outcome variables (easily modifiable)
OUTCOME_VARS <- c(
  "relative_width_deviation",
  "relative_spacing_deviation"
)

# Outcome variable labels for plots
OUTCOME_LABELS <- c(
  "relative_width_deviation" = "Relative Width Deviation",
  "relative_spacing_deviation" = "Relative Spacing Deviation"
)

X_AXIS_VAR <- "correct_width"
SECONDARY_CONDITION <- "correct_num"    # Change to "spacing" if needed

INDIVIDUAL_DOT_ALPHA <- 0.15
INDIVIDUAL_DOT_SIZE_RANGE <- c(1, 4)
GRAND_MEAN_SIZE <- 5
ERROR_BAR_WIDTH <- 0
RM_COLOR <- "#E74C3C"
NORM_COLOR <- "#3498DB"

# Soft-coded output filename based on primary outcome and experiment
PRIMARY_OUTCOME <- OUTCOME_VARS[1]  # Uses first outcome variable
OUTPUT_FILENAME <- paste0(gsub("_", "", PRIMARY_OUTCOME), "_", EXPERIMENT_VERSION)
OUTPUT_WIDTH <- 12
OUTPUT_HEIGHT <- 8
OUTPUT_DPI <- 300

# LIBRARIES =================================================================

library(tidyverse)
library(patchwork)
source("analysis/helpers/theme_scientific.R")

# DATA PREPARATION ==========================================================

df_raw <- read.csv("datasets/processed.csv")

df_filtered <- df_raw %>%
  filter(exp_version == EXPERIMENT_VERSION) %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_condition = factor(
      ifelse(number_deviation == -1, "RM", "NoRM"),
      levels = c("NoRM", "RM")
    ),
    # Keep numeric versions for calculations
    correct_width_num = as.numeric(correct_width),
    correct_space_num = as.numeric(correct_space),
    # Calculate relative deviations using numeric versions
    relative_width_deviation = width_deviation / correct_width_num,
    relative_spacing_deviation = spacing_deviation / correct_space_num,
    # Convert to factors after calculations
    !!X_AXIS_VAR := factor(get(X_AXIS_VAR)),
    !!SECONDARY_CONDITION := factor(get(SECONDARY_CONDITION)),
    subID = factor(subID)
  ) %>%
  filter(
    !is.na(relative_width_deviation) & 
    !is.na(relative_spacing_deviation) &
    is.finite(relative_width_deviation) &
    is.finite(relative_spacing_deviation) &
    correct_width_num != 0 & 
    correct_space_num != 0
  )

# Check if data exists after filtering
if(nrow(df_filtered) == 0) {
  stop("No data remaining after filtering. Check experiment version and data availability.")
}

# CALCULATION FUNCTIONS =====================================================

calculate_trial_percentages <- function(data) {
  data %>%
    group_by(subID, !!sym(X_AXIS_VAR), !!sym(SECONDARY_CONDITION)) %>%
    summarise(
      total_trials = n(),
      rm_trials = sum(rm_condition == "RM"),
      norm_trials = sum(rm_condition == "NoRM"),
      rm_percentage = rm_trials / total_trials,
      norm_percentage = norm_trials / total_trials,
      .groups = "drop"
    )
}

trial_percentages <- calculate_trial_percentages(df_filtered)

calculate_individual_means <- function(data, outcome_var) {
  data %>%
    group_by(subID, rm_condition, !!sym(X_AXIS_VAR), !!sym(SECONDARY_CONDITION)) %>%
    summarise(
      mean_outcome = mean(get(outcome_var), na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    left_join(
      trial_percentages,
      by = c("subID", X_AXIS_VAR, SECONDARY_CONDITION)
    ) %>%
    mutate(
      dot_size = case_when(
        rm_condition == "RM" ~ scales::rescale(rm_percentage, to = INDIVIDUAL_DOT_SIZE_RANGE),
        rm_condition == "NoRM" ~ scales::rescale(norm_percentage, to = INDIVIDUAL_DOT_SIZE_RANGE)
      )
    )
}

calculate_grand_means <- function(data, outcome_var) {
  data %>%
    group_by(rm_condition, !!sym(X_AXIS_VAR), !!sym(SECONDARY_CONDITION)) %>%
    summarise(
      n_participants = n_distinct(subID),
      mean_outcome = mean(get(outcome_var), na.rm = TRUE),
      sd_outcome = sd(get(outcome_var), na.rm = TRUE),
      se_outcome = sd_outcome / sqrt(n_participants),
      ci_lower = mean_outcome - (1.96 * se_outcome),
      ci_upper = mean_outcome + (1.96 * se_outcome),
      .groups = "drop"
    )
}

# PLOTTING FUNCTION =========================================================

create_rm_plot <- function(outcome_var, outcome_label) {
  individual_data <- calculate_individual_means(df_filtered, outcome_var)
  grand_means_data <- calculate_grand_means(df_filtered, outcome_var)
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
    geom_point(
      data = individual_data,
      aes(x = get(X_AXIS_VAR), y = mean_outcome, 
          color = rm_condition, size = dot_size),
      alpha = INDIVIDUAL_DOT_ALPHA,
      position = position_jitter(width = 0.1, height = 0, seed = 42)
    ) +
    geom_errorbar(
      data = grand_means_data,
      aes(x = get(X_AXIS_VAR), ymin = ci_lower, ymax = ci_upper, 
          color = rm_condition),
      width = ERROR_BAR_WIDTH,
      size = 1,
      position = position_dodge(width = 0.3)
    ) +
    geom_point(
      data = grand_means_data,
      aes(x = get(X_AXIS_VAR), y = mean_outcome, color = rm_condition),
      size = GRAND_MEAN_SIZE,
      position = position_dodge(width = 0.3)
    ) +
    facet_wrap(as.formula(paste("~", SECONDARY_CONDITION)), 
               scales = "free_x", 
               labeller = label_both) +
    scale_color_manual(
      values = c("NoRM" = NORM_COLOR, "RM" = RM_COLOR),
      name = "Condition"
    ) +
    scale_size_identity() +
    labs(
      x = paste(str_to_title(str_replace_all(X_AXIS_VAR, "_", " "))),
      y = outcome_label
    ) +
    theme_scientific(base_size = 12, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      panel.spacing = unit(1, "lines")
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 3, alpha = 1)),
      size = "none"
    )
}

# GENERATE PLOTS ============================================================

plot_list <- map2(
  OUTCOME_VARS, 
  OUTCOME_LABELS[OUTCOME_VARS],
  ~ create_rm_plot(.x, .y)
)

names(plot_list) <- OUTCOME_VARS

# COMBINE AND SAVE ==========================================================

combined_plot <- wrap_plots(
  plot_list, 
  ncol = 1,
  guides = "collect"
) &
  theme(legend.position = "bottom")

# Create dynamic title showing interactions
x_axis_label <- str_to_title(str_replace_all(X_AXIS_VAR, "_", " "))
secondary_condition_label <- ifelse(SECONDARY_CONDITION == "correct_num", "Set Size", 
                                  str_to_title(str_replace_all(SECONDARY_CONDITION, "_", " ")))

plot_title <- paste0(EXPERIMENT_VERSION, ": RM × ", x_axis_label, " × ", secondary_condition_label)

final_plot <- combined_plot +
  plot_annotation(
    title = plot_title,
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))
    )
  )

# print(final_plot)


output_path <- file.path("figures", paste0(OUTPUT_FILENAME, ".png"))

ggsave(
  filename = output_path,
  plot = final_plot,
  width = OUTPUT_WIDTH,
  height = OUTPUT_HEIGHT,
  dpi = OUTPUT_DPI,
  units = "in",
  bg = "white"
)