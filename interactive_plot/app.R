library(shiny)
library(shinythemes)
library(tidyverse)
library(lme4)
library(emmeans)
library(ggplot2)
library(magrittr)
library(DT)

# Silence NSE notes for linters
utils::globalVariables(c(
  "number_deviation", "spacing", "correct_num", "correct_width", "width_deviation",
  "correct_space", "spacing_deviation", "width_deviation_relative", "spacing_deviation_relative",
  "pooled_width_deviation", "edge_to_edge_spacing_deviation", "correct_space_category",
  "exp_version", "rm_type", "emmean", "lower.CL", "upper.CL", "n",
  "subID", "mean_outcome", "ci_lower", "ci_upper", "trial_type"
))

# Constants
RM_COLOR <- "#E74C3C"
NORM_COLOR <- "#3498DB"

# Robust data path resolver (works from project root or interactive_plot/)
find_data_file <- function(rel_path) {
  # For deployment, also check local directory
  filename <- basename(rel_path)
  candidates <- c(
    filename,                    # Local directory (for deployment)
    rel_path,                   # Relative path as given
    file.path("..", rel_path),  # One level up
    file.path("..", "..", rel_path)  # Two levels up
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(sprintf("Missing data file: %s. Tried: %s", rel_path, paste(candidates, collapse = "; ")))
  }
  existing[[1]]
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Redundancy Masking Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("EXPERIMENT SELECTION"),
      checkboxGroupInput(
        inputId = "exp_select",
        label = NULL,
        choices = c("Exp1A", "Exp1B", "Exp1C"),
        selected = "Exp1A"
      ),
      radioButtons(
        inputId = "viz_mode",
        label = "Visualization Mode",
        choices = c(
          "Single Experiment" = "single",
          "Overlay Experiments" = "overlay", 
          "Facet by Experiment" = "facet"
        ),
        selected = "single"
      ),
      tags$hr(),
      h4("TRIAL TYPE"),
      radioButtons(
        inputId = "trial_type",
        label = NULL,
        choices = c(
          "All Trials" = "all",
          "Initial Trials Only" = "initial"
        ),
        selected = "all"
      ),
      conditionalPanel(
        condition = "input.trial_type == 'initial'",
        p(style = "font-size: 11px; color: #666; margin-top: -5px; margin-bottom: 10px;", 
          "Uses correct_space as spacing factor with experiment-specific levels")
      ),
      tags$hr(),
      h4("OUTCOME VARIABLE"),
      p(style = "font-size: 12px; color: #666; margin-bottom: 10px;", 
        "Select the dependent variable to analyze."),
      selectInput(
        inputId = "outcome",
        label = NULL,
        choices = c(
          "Width Deviation (response - correct width)" = "width_deviation",
          "Absolute Width Deviation (|response - correct width|)" = "absolute_width_deviation",
          "Spacing Deviation (response - correct spacing)" = "spacing_deviation",
          "Relative Width Deviation (width error / correct width)" = "width_deviation_relative",
          "Relative Spacing Deviation (spacing error / correct spacing)" = "spacing_deviation_relative",
          "Pooled Width Deviation (total width area error)" = "pooled_width_deviation",
          "Width Density Deviation (response - correct density)" = "width_density_deviation",
          "Edge-to-Edge Spacing Deviation (gap between elements error)" = "edge_to_edge_spacing_deviation"
        ),
        selected = "width_deviation"
      ),
      tags$hr(),
      h4("FACTORS FOR PLOTTING"),
      checkboxGroupInput(
        inputId = "factors",
        label = NULL,
        choices = c(
          spacing_category = "spacing_category",
          correct_num = "correct_num",
          correct_width = "correct_width"
        ),
        selected = c("correct_width")
      ),
      selectInput(
        inputId = "x_axis",
        label = "X-Axis Variable",
        choices = c("None", "spacing_category", "correct_num", "correct_width"),
        selected = "correct_width"
      ),
      tags$hr(),
      h4("MODEL VARIABLES"),
      checkboxGroupInput(
        inputId = "model_vars",
        label = NULL,
        choices = c(
          spacing_category = "spacing_category",
          correct_num = "correct_num",
          correct_width = "correct_width"
        ),
        selected = c("spacing_category", "correct_num", "correct_width")
      ),
      checkboxInput("include_interactions", "Include interactions (\"*\")", value = TRUE),
      tags$hr(),
      h4("DISPLAY OPTIONS"),
      tags$hr(),
      h4("Y-AXIS LIMITS (optional)"),
      checkboxInput("fix_y_limits", "Fix y-axis limits", value = FALSE),
      fluidRow(
        column(6, numericInput("y_min", "Y min", value = NA, step = 0.1)),
        column(6, numericInput("y_max", "Y max", value = NA, step = 0.1))
      )
    ),
    mainPanel(
      width = 9,
      uiOutput("plot_title"),
      plotOutput("main_plot", height = "600px"),
      uiOutput("sample_info"),
      uiOutput("model_formula"),
      uiOutput("outcome_note"),
      downloadButton("download_plot", "Download Plot"),
      br(),
      checkboxInput("show_emm", "Show emmeans table", value = FALSE),
      conditionalPanel(
        condition = "input.show_emm == true",
        uiOutput("emmeans_warning"),
        h4("EMMEANS OPTIONS"),
        checkboxGroupInput(
          inputId = "emm_vars",
          label = "Condition by variables (independent of model)",
          choices = c("spacing_category", "correct_num", "correct_width"),
          selected = c()
        ),
        radioButtons(
          inputId = "emm_comparison_type",
          label = "Comparison Type",
          choices = c(
            "Pairwise comparisons (RM vs NoRM)" = "pairwise",
            "One-sample t-tests (vs 0)" = "onesample"
          ),
          selected = "pairwise"
        )
      ),
      conditionalPanel(
        condition = "input.show_emm == true",
        dataTableOutput("emm_table")
      ),
      br(),
      uiOutput("warnings")
    )
  )
)

server <- function(input, output, session) {
  # Theme (optional)
  if (file.exists("preAnalysis/helpers/theme_scientific.R")) {
    source("preAnalysis/helpers/theme_scientific.R")
  }

  # Keep default emmeans/lmerTest settings to avoid long runtimes

  # Load and prepare data once
  processed_path <- find_data_file("datasets/processed.csv")
  df_raw <- read.csv(processed_path)

  # Precompute helper columns
  df_raw <- df_raw %>%
    dplyr::mutate(
      rm_type = factor(dplyr::if_else(number_deviation == -1, "RM", "NoRM"), levels = c("NoRM", "RM")),
      spacing_category = factor(dplyr::if_else(!is.na(spacing), spacing, NA_character_),
                                levels = c("small", "middle", "large")),
      correct_num = factor(correct_num),
      correct_width = factor(correct_width)
    )
  
  # Create correct_space_category factor with experiment-specific levels for initial trials
  df_raw <- df_raw %>%
    dplyr::mutate(
      correct_space_category = dplyr::case_when(
        exp_version == "Exp1A" ~ factor(as.character(correct_space), 
                                      levels = c("0.5", "0.7", "0.9"),
                                      labels = c("0.5", "0.7", "0.9")),
        exp_version == "Exp1B" ~ factor(as.character(correct_space), 
                                      levels = c("0.7", "0.9", "1.1"),
                                      labels = c("0.7", "0.9", "1.1")),
        exp_version == "Exp1C" ~ factor(as.character(correct_space)),
        TRUE ~ as.factor(NA)
      )
    )

  # If relative measures are missing (older processed files), compute them safely using numerics
  if (!"width_deviation_relative" %in% names(df_raw) || !"spacing_deviation_relative" %in% names(df_raw)) {
    df_raw <- df_raw %>%
      dplyr::mutate(
        correct_width_num = as.numeric(as.character(correct_width)),
        correct_space_num = as.numeric(as.character(correct_space)),
        width_deviation_relative = dplyr::if_else(correct_width_num != 0, width_deviation / correct_width_num, NA_real_),
        spacing_deviation_relative = dplyr::if_else(correct_space_num != 0, spacing_deviation / correct_space_num, NA_real_)
      ) %>%
      dplyr::select(-correct_width_num, -correct_space_num)
  }
  
  # Add absolute width deviation if missing
  if (!"absolute_width_deviation" %in% names(df_raw)) {
    df_raw <- df_raw %>%
      dplyr::mutate(absolute_width_deviation = abs(width_deviation))
  }
  
  # Check if trial_type column exists, create placeholder if missing
  if (!"trial_type" %in% names(df_raw)) {
    df_raw$trial_type <- "all"  # Default all trials to "all" type
    warning("trial_type column not found in data. All trials treated as 'all' type.")
  }

  # Experiments available (multi select)
  observe({
    updateCheckboxGroupInput(session, "exp_select", choices = sort(unique(df_raw$exp_version)), selected = sort(unique(df_raw$exp_version))[1])
  })

  # Update factor choices based on selected experiments and trial type with availability indicators
  observe({
    req(input$exp_select, input$trial_type)
    
    # Capture current selections before updating
    current_factors <- input$factors
    current_model_vars <- input$model_vars
    current_x_axis <- input$x_axis
    current_emm_vars <- input$emm_vars
    
    selected_exps <- input$exp_select
    
    # Determine spacing variable based on trial type
    spacing_var <- if (input$trial_type == "initial") "correct_space_category" else "spacing_category"
    spacing_label <- if (input$trial_type == "initial") "correct_space_category" else "spacing_category"
    
    # For initial trials, all experiments have correct_space_category
    # For all trials, Exp1C lacks spacing_category
    if (input$trial_type == "initial") {
      # Initial trials: all experiments have correct_space_category
      has_spacing <- TRUE
      has_spacing_partial <- FALSE
    } else {
      # All trials: Exp1C lacks spacing_category  
      has_spacing <- !("Exp1C" %in% selected_exps) || length(selected_exps) == 0
      has_spacing_partial <- any(c("Exp1A", "Exp1B") %in% selected_exps) && ("Exp1C" %in% selected_exps)
    }
    
    # Build factor choices with availability indicators
    if (has_spacing && !has_spacing_partial) {
      # All selected experiments have the spacing variable
      factor_choices <- c(
        "correct_num" = "correct_num", 
        "correct_width" = "correct_width"
      )
      # Add spacing variable with appropriate name
      spacing_choice <- setNames(spacing_var, spacing_label)
      factor_choices <- c(spacing_choice, factor_choices)
      
      x_axis_choices <- c("None", spacing_var, "correct_num", "correct_width")
      
      model_choices <- c(
        "correct_num" = "correct_num",
        "correct_width" = "correct_width"
      )
      model_choices <- c(spacing_choice, model_choices)
      
      emm_choices <- c(spacing_var, "correct_num", "correct_width")
    } else if (has_spacing_partial) {
      # Mixed experiments - show availability indicators (only applies to all trials mode)
      available_for_spacing <- intersect(selected_exps, c("Exp1A", "Exp1B"))
      partial_spacing_label <- paste0(spacing_label, " (", paste(available_for_spacing, collapse = ", "), " only)")
      
      factor_choices <- c("correct_num", "correct_width")
      names(factor_choices) <- c("correct_num", "correct_width")
      factor_choices <- c(setNames(spacing_var, partial_spacing_label), factor_choices)
      
      x_axis_choices <- c("None", spacing_var, "correct_num", "correct_width")
      
      model_choices <- c("correct_num", "correct_width")
      names(model_choices) <- c("correct_num", "correct_width")
      model_choices <- c(setNames(spacing_var, partial_spacing_label), model_choices)
      
      emm_choices <- c(spacing_var, "correct_num", "correct_width")
    } else {
      # No spacing variable available (only Exp1C selected in "all trials" mode)
      factor_choices <- c(
        "correct_num" = "correct_num",
        "correct_width" = "correct_width"
      )
      x_axis_choices <- c("None", "correct_num", "correct_width")
      model_choices <- c(
        "correct_num" = "correct_num",
        "correct_width" = "correct_width"
      )
      emm_choices <- c("correct_num", "correct_width")
    }
    
    # Preserve selections where possible (use the actual factor names, not labels)
    available_factor_values <- unname(factor_choices)
    available_model_values <- unname(model_choices)
    
    preserved_factors <- intersect(current_factors, available_factor_values)
    preserved_model_vars <- intersect(current_model_vars, available_model_values)
    preserved_emm_vars <- intersect(current_emm_vars, emm_choices)
    
    # For x_axis, check if current selection is still valid
    # If not valid, reset to "None" to be safe
    preserved_x_axis <- if (current_x_axis %in% x_axis_choices) current_x_axis else "None"
    
    # Update all relevant inputs with preserved selections
    updateCheckboxGroupInput(session, "factors", choices = factor_choices, 
                           selected = if(length(preserved_factors) > 0) preserved_factors else unname(factor_choices)[length(factor_choices)])
    updateCheckboxGroupInput(session, "model_vars", choices = model_choices, 
                           selected = if(length(preserved_model_vars) > 0) preserved_model_vars else unname(model_choices))
    updateSelectInput(session, "x_axis", choices = x_axis_choices, 
                     selected = preserved_x_axis)
    updateCheckboxGroupInput(session, "emm_vars", choices = emm_choices, 
                           selected = preserved_emm_vars)
  })

  # Filtered data according to selections
  data_filtered <- reactive({
    req(input$exp_select, input$outcome, input$trial_type)
    dat <- df_raw %>%
      dplyr::filter(exp_version %in% input$exp_select) %>%
      dplyr::filter(number_deviation %in% c(-1, 0)) %>%
      dplyr::filter(!is.na(rm_type))
    
    # Apply trial type filtering
    if (input$trial_type == "initial") {
      # Filter for initial trials only, remove match trials
      dat <- dat %>%
        dplyr::filter(trial_type == "initial") %>%
        dplyr::filter(trial_type != "match" | is.na(trial_type))
    }
    # For "all" trials, no additional filtering needed

    # Relative outcome switch only if selected outcome is one of core deviations
    outcome_var <- input$outcome
    # Clean rows for relative denominators
    if (outcome_var == "width_deviation" && any(is.infinite(dat$width_deviation_relative) | is.na(dat$width_deviation_relative))) {
      dat <- dat %>% dplyr::filter(!is.na(width_deviation_relative), is.finite(width_deviation_relative))
    }
    if (outcome_var == "spacing_deviation" && any(is.infinite(dat$spacing_deviation_relative) | is.na(dat$spacing_deviation_relative))) {
      dat <- dat %>% dplyr::filter(!is.na(spacing_deviation_relative), is.finite(spacing_deviation_relative))
    }
    dat
  })

  # Build model formula dynamically
  model_formula_reactive <- reactive({
    req(input$outcome, input$model_vars, input$exp_select, input$trial_type)
    # Always include rm_type; use model variables selection
    selected <- input$model_vars
    # Include both possible spacing variables for dynamic selection
    selected <- selected[selected %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Include experiment as fixed effect if multiple experiments selected
    base_vars <- if (length(input$exp_select) > 1) {
      c("exp_version", "rm_type")
    } else {
      c("rm_type")
    }
    
    rhs <- if (length(selected) == 0) {
      paste(base_vars, collapse = " * ")
    } else if (isTRUE(input$include_interactions)) {
      paste(c(base_vars, selected), collapse = " * ")
    } else {
      paste(c(base_vars, selected), collapse = " + ")
    }
    as.formula(paste(input$outcome, "~", rhs, "+ (1|subID)"))
  })

  # Compute emmeans for plotting
  emm_results <- reactive({
    req(input$show_emm)
    
    # Skip emmeans computation if multiple experiments selected to avoid convergence issues
    if (length(input$exp_select) > 1) {
      return(data.frame(Message = "Emmeans disabled for multiple experiments"))
    }
    
    dat <- data_filtered()
    req(nrow(dat) > 0)
    fml <- model_formula_reactive()

    dat <- droplevels(dat)
    
    # Try to fit model safely
    mdl <- tryCatch({
      lme4::lmer(fml, data = dat, control = lme4::lmerControl(check.rankX = "ignore"))
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(mdl)) {
      return(data.frame(Message = "Model convergence failed"))
    }

    # Use independent emmeans conditioning vars (emm_vars)
    cond_vars <- input$emm_vars
    if (is.null(cond_vars)) cond_vars <- character(0)

    # Single experiment mode - use simple rm_type specification
    spec_rhs <- if (length(cond_vars) > 0) paste(cond_vars, collapse = " * ") else "1"
    spec_formula <- stats::as.formula(paste("~ rm_type |", spec_rhs))
    
    em <- tryCatch({
      emmeans::emmeans(mdl, specs = spec_formula)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(em)) {
      return(data.frame(Message = "Emmeans computation failed"))
    }
    
    as.data.frame(stats::confint(em, level = 0.95, adjust = "none"))
  })

  # Title reflecting selection
  output$plot_title <- renderUI({
    req(input$outcome, input$x_axis, input$exp_select)
    pretty_outcome <- switch(input$outcome,
                             width_deviation = "Width Deviation",
                             absolute_width_deviation = "Absolute Width Deviation",
                             spacing_deviation = "Spacing Deviation",
                             width_deviation_relative = "Relative Width Deviation",
                             spacing_deviation_relative = "Relative Spacing Deviation",
                             pooled_width_deviation = "Pooled Width Deviation",
                             width_density_deviation = "Width Density Deviation",
                             edge_to_edge_spacing_deviation = "Edge-to-Edge Spacing Deviation",
                             input$outcome)
    x_part <- if (input$x_axis == "None") "" else paste0(" by ", gsub("_", " ", input$x_axis))
    
    # Handle multiple experiments in title
    exp_part <- if (length(input$exp_select) == 1) {
      input$exp_select
    } else {
      paste0(paste(input$exp_select, collapse = " + "), " (", input$viz_mode, ")")
    }
    
    h3(paste0(exp_part, " — ", pretty_outcome, ": RM vs NoRM", x_part))
  })

  # Short note explaining the selected outcome computation
  output$outcome_note <- renderUI({
    req(input$outcome)
    note <- switch(input$outcome,
      width_deviation = "width_deviation = response_width - correct_width",
      absolute_width_deviation = "absolute_width_deviation = abs(response_width - correct_width)",
      spacing_deviation = "spacing_deviation = response_space - correct_space",
      width_deviation_relative = "width_deviation_relative = (response_width - correct_width) / correct_width",
      spacing_deviation_relative = "spacing_deviation_relative = (response_space - correct_space) / correct_space",
      pooled_width_deviation = "pooled_width_deviation = (response_width × response_num) - (correct_width × correct_num)",
      width_density_deviation = "width_density_deviation = response_width_density - actual_width_density<br/>where width_density = (width × number) / stimulus_length",
      edge_to_edge_spacing_deviation = "edge_to_edge_spacing_deviation = (response_space - response_width) - (correct_space - correct_width)<br/>measuring the gap between elements",
      paste0("Outcome: ", input$outcome)
    )
    HTML(paste0("<i><b>Calculation:</b> ", note, "</i>"))
  })

  # Plot
  plot_obj <- reactive({
    dat <- data_filtered()
    req(nrow(dat) > 0, input$viz_mode, input$exp_select, input$trial_type)
    xvar <- input$x_axis
    
    # Validate that x-axis variable exists in the data when not "None"
    if (xvar != "None" && !xvar %in% names(dat)) {
      # Fallback to "None" if the selected x-axis variable doesn't exist
      xvar <- "None"
    }
    
    selected <- unique(c(input$factors, xvar))
    # Include both possible spacing variables for dynamic selection
    selected <- selected[selected %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure all selected factors exist in the data
    selected <- selected[selected %in% names(dat)]

    # Determine if we're in multi-experiment mode
    multi_exp <- length(input$exp_select) > 1 && input$viz_mode != "single"
    
    # Compute participant-level means then grand means with 95% CI
    outcome_col <- input$outcome

    # Group variables - add exp_version for multi-experiment modes
    group_vars <- c("rm_type", input$factors)
    if (multi_exp) {
      group_vars <- c("exp_version", group_vars)
    }
    group_vars <- group_vars[group_vars %in% c("exp_version", "rm_type", "spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure all group variables exist in the actual data
    group_vars <- group_vars[group_vars %in% names(dat)]

    subject_means <- dat %>%
      dplyr::group_by(subID, dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(mean_outcome = mean(.data[[outcome_col]], na.rm = TRUE), n_trials = dplyr::n(), .groups = "drop")

    # Group by condition variables only (not subID)
    condition_vars <- c("rm_type", input$factors)
    if (multi_exp) {
      condition_vars <- c("exp_version", condition_vars)
    }
    condition_vars <- condition_vars[condition_vars %in% names(subject_means)]
    
    # Ensure at least rm_type exists
    if (!"rm_type" %in% condition_vars) {
      condition_vars <- c("rm_type", condition_vars)
    }
    
    grand <- subject_means %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(condition_vars))) %>%
      dplyr::summarise(
        n_participants = dplyr::n_distinct(subID),
        sd_outcome = stats::sd(mean_outcome, na.rm = TRUE),
        mean_outcome = mean(mean_outcome, na.rm = TRUE),
        se_outcome = sd_outcome / sqrt(n_participants),
        n_trials = sum(n_trials),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        tcrit = dplyr::if_else(n_participants > 1, stats::qt(0.975, df = n_participants - 1), NA_real_),
        ci_lower = mean_outcome - tcrit * se_outcome,
        ci_upper = mean_outcome + tcrit * se_outcome
      )

    # Build faceting variables based on visualization mode
    if (input$viz_mode == "facet" && multi_exp) {
      # Facet mode: include experiment in faceting
      if (input$x_axis == "None") {
        facet_vars <- c("exp_version", input$factors)
      } else {
        facet_vars <- c("exp_version", setdiff(input$factors, input$x_axis))
      }
    } else {
      # Single or overlay mode: standard faceting
      if (input$x_axis == "None") {
        facet_vars <- input$factors
      } else {
        facet_vars <- setdiff(input$factors, input$x_axis)
      }
    }
    facet_vars <- facet_vars[facet_vars %in% c("exp_version", "spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure facet variables exist in the grand data
    facet_vars <- facet_vars[facet_vars %in% names(grand)]
    
    if (length(facet_vars) > 0) {
      grand$facet_label <- do.call(interaction, c(grand[facet_vars], list(drop = TRUE, sep = " • ")))
    }

    # Build aesthetic mappings based on visualization mode  
    # Use validated xvar (which may have been reset to "None" if invalid)
    x_aesthetic <- if (xvar == "None") rlang::sym("rm_type") else rlang::sym(xvar)
    x_label <- if (input$x_axis == "None") "Condition" else gsub("_", " ", xvar)
    
    if (input$viz_mode == "overlay" && multi_exp) {
      # Overlay mode: use experiment for shape, rm_type for color
      p <- ggplot2::ggplot(grand, ggplot2::aes(x = !!x_aesthetic, y = mean_outcome, color = rm_type, shape = exp_version)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.0, size = 1.2, position = ggplot2::position_dodge(width = 0.7), alpha = 0.8) +
        ggplot2::geom_point(size = 5, alpha = 0.9, position = ggplot2::position_dodge(width = 0.7)) +
        ggplot2::scale_color_manual(values = c("RM" = RM_COLOR, "NoRM" = NORM_COLOR)) +
        ggplot2::scale_shape_manual(values = c("Exp1A" = 16, "Exp1B" = 17, "Exp1C" = 15)) +
        ggplot2::labs(x = x_label, y = input$outcome, color = "RM Type", shape = "Experiment")
    } else {
      # Single or facet mode: standard rm_type coloring
      p <- ggplot2::ggplot(grand, ggplot2::aes(x = !!x_aesthetic, y = mean_outcome, color = rm_type)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.0, size = 1.2, position = ggplot2::position_dodge(width = 0.5), alpha = 0.8) +
        ggplot2::geom_point(size = 5, alpha = 0.9, position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::scale_color_manual(values = c("RM" = RM_COLOR, "NoRM" = NORM_COLOR)) +
        ggplot2::labs(x = x_label, y = input$outcome, color = "Condition")
    }

    # Optional fixed y-limits without dropping data
    if (isTRUE(input$fix_y_limits) && is.finite(as.numeric(input$y_min)) && is.finite(as.numeric(input$y_max))) {
      p <- p + ggplot2::coord_cartesian(ylim = c(as.numeric(input$y_min), as.numeric(input$y_max)))
    }

    if (exists("theme_scientific")) {
      p <- p + get("theme_scientific")()
    } else {
      p <- p + ggplot2::theme_bw()
    }

    # Faceting
    if (length(facet_vars) > 0) {
      p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x")
    }
    p
  })

  output$main_plot <- renderPlot({
    plot_obj()
  })

  # Sample info and counts
  output$sample_info <- renderUI({
    dat <- data_filtered()
    req(nrow(dat) > 0, input$trial_type)
    
    # Add trial type information
    trial_type_text <- if (input$trial_type == "initial") " (Initial trials only)" else ""
    
    if (length(input$exp_select) == 1) {
      # Single experiment
      n_trials <- nrow(dat)
      n_participants <- dplyr::n_distinct(dat$subID)
      HTML(sprintf("<b>Sample size:</b> %s - %s trials, %s participants%s", input$exp_select, n_trials, n_participants, trial_type_text))
    } else {
      # Multiple experiments - show breakdown
      total_trials <- nrow(dat)
      total_participants <- dplyr::n_distinct(dat$subID)
      
      exp_breakdown <- dat %>%
        dplyr::group_by(exp_version) %>%
        dplyr::summarise(
          n_trials = dplyr::n(),
          n_participants = dplyr::n_distinct(subID),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          exp_info = paste0(exp_version, ": ", n_trials, " trials, ", n_participants, " participants")
        )
      
      breakdown_text <- paste(exp_breakdown$exp_info, collapse = " | ")
      HTML(sprintf("<b>Sample size:</b> Total - %s trials, %s participants%s<br/><small>%s</small>", 
                   total_trials, total_participants, trial_type_text, breakdown_text))
    }
  })

  # Model formula display
  output$model_formula <- renderUI({
    fml <- model_formula_reactive()
    HTML(paste0("<b>Model:</b> ", rlang::expr_text(fml)))
  })

  # Emmeans warning for multiple experiments
  output$emmeans_warning <- renderUI({
    req(input$exp_select)
    if (length(input$exp_select) > 1) {
      HTML("<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; padding: 10px; margin-bottom: 10px;'>
            <strong style='color: #856404;'>⚠️ Warning:</strong> 
            <span style='color: #856404;'>Emmeans analysis with multiple experiments may cause model convergence issues. 
            For reliable emmeans results, please select only <strong>one experiment</strong> at a time.</span>
            </div>")
    } else {
      HTML("")
    }
  })

  # Data availability / warnings
  output$warnings <- renderUI({
    dat <- data_filtered()
    req(nrow(dat) > 0, input$trial_type)
    xvar <- input$x_axis
    selected <- unique(c(input$factors, xvar))
    selected <- selected[selected %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    by_vars <- unique(c("rm_type", selected))
    
    # Validate that selected variables exist in data
    missing_vars <- by_vars[!by_vars %in% names(dat)]
    by_vars <- by_vars[by_vars %in% names(dat)]
    
    warnings_html <- c()
    
    # Warning for missing variables
    if (length(missing_vars) > 0) {
      warnings_html <- c(warnings_html, 
        sprintf("<span style='color:#E74C3C'><b>Warning:</b> Variables not available in current data: %s</span>", 
                paste(missing_vars, collapse = ", ")))
    }
    
    # Count combinations for existing variables
    if (length(by_vars) > 0) {
      counts <- dat %>% dplyr::count(dplyr::across(dplyr::all_of(by_vars)), name = "n")
      empty <- counts %>% dplyr::filter(is.na(n) | n == 0)
      if (nrow(empty) > 0) {
        warnings_html <- c(warnings_html,
          "<span style='color:#E74C3C'><b>Warning:</b> Some factor combinations have zero trials and are excluded.</span>")
      }
    }
    
    if (length(warnings_html) > 0) {
      HTML(paste(warnings_html, collapse = "<br/>"))
    } else {
      HTML("")
    }
  })

  # Download current plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("RM_plot_", input$outcome, "_by_", input$x_axis, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(filename = file, plot = plot_obj(), width = 10, height = 7, dpi = 300, units = "in", bg = "white")
    }
  )

  # Optional emmeans table
  output$emm_table <- renderDataTable({
    req(input$show_emm)
    
    # Check if multiple experiments are selected
    if (length(input$exp_select) > 1) {
      # Return a message table instead of attempting analysis
      warning_table <- data.frame(
        Message = "Emmeans analysis is disabled for multiple experiments due to potential model convergence issues. Please select only one experiment for emmeans analysis."
      )
      return(DT::datatable(warning_table, options = list(
        pageLength = 5,
        dom = 't',
        ordering = FALSE,
        searching = FALSE
      )))
    }
    
    df <- emm_results()
    # Build model and contrast table using independent emmeans conditioning
    dat <- data_filtered()
    fml <- model_formula_reactive()
    dat <- droplevels(dat)
    
    # Try to fit the model with error handling
    mdl <- tryCatch({
      lme4::lmer(fml, data = dat, control = lme4::lmerControl(check.rankX = "ignore"))
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(mdl)) {
      # Return error message if model fails
      error_table <- data.frame(
        Message = paste("Model convergence failed. This may be due to insufficient data or overly complex model specifications.",
                       "Try reducing model complexity or selecting different factors.")
      )
      return(DT::datatable(error_table, options = list(
        pageLength = 5,
        dom = 't',
        ordering = FALSE,
        searching = FALSE
      )))
    }

    by_vars <- input$emm_vars
    if (is.null(by_vars)) by_vars <- character(0)
    spec_rhs <- if (length(by_vars) > 0) paste(by_vars, collapse = " * ") else "1"
    
    # Single experiment mode - use simple rm_type specification
    spec_formula <- stats::as.formula(paste("~ rm_type |", spec_rhs))

    # Get emmeans using consistent formula for both comparison types
    emm_obj <- emmeans::emmeans(mdl, specs = spec_formula)
    
    if (input$emm_comparison_type == "onesample") {
      # One-sample tests vs 0 for each emmean
      null_sum <- try(emmeans::test(emm_obj, null = 0, adjust = "bonferroni"), silent = TRUE)
      if (!inherits(null_sum, "try-error")) {
        null_df <- as.data.frame(null_sum)
        if ("p.value" %in% names(null_df)) {
          null_df <- null_df %>%
            dplyr::mutate(
              p_formatted = dplyr::case_when(
                p.value < 1e-4 ~ "< 0.0001",
                p.value < 0.001 ~ "< 0.001",
                TRUE ~ as.character(round(p.value, 3))
              ),
              significance = dplyr::case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01 ~ "**",
                p.value < 0.05 ~ "*",
                p.value < 0.1 ~ ".",
                TRUE ~ ""
              ),
              p_with_sig = paste0(p_formatted, " ", significance)
            ) %>%
            dplyr::select(-p.value, -p_formatted, -significance) %>%
            dplyr::rename(p_value = p_with_sig) %>%
            dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
        }
        res_table <- null_df
      } else {
        res_table <- data.frame(message = "No results; emmeans test failed")
      }
      
      # Ensure consistent column structure for DataTables
      if (!"contrast" %in% names(res_table) && !"message" %in% names(res_table)) {
        # Add a contrast-like column for one-sample tests
        if ("rm_type" %in% names(res_table)) {
          res_table$contrast <- paste(res_table$rm_type, "vs 0")
        }
      }
    } else {
      # Default: RM vs NoRM pairwise contrasts
      contr <- try(emmeans::contrast(emm_obj, method = "revpairwise", by = by_vars, adjust = "bonferroni"), silent = TRUE)
      if (!inherits(contr, "try-error")) {
        contr_df <- as.data.frame(contr)
        if ("p.value" %in% names(contr_df)) {
          contr_df <- contr_df %>%
            dplyr::mutate(
              p_formatted = dplyr::case_when(
                p.value < 1e-4 ~ "< 0.0001",
                p.value < 0.001 ~ "< 0.001",
                TRUE ~ as.character(round(p.value, 3))
              ),
              significance = dplyr::case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01 ~ "**",
                p.value < 0.05 ~ "*",
                p.value < 0.1 ~ ".",
                TRUE ~ ""
              ),
              p_with_sig = paste0(p_formatted, " ", significance)
            ) %>%
            dplyr::select(-p.value, -p_formatted, -significance) %>%
            dplyr::rename(p_value = p_with_sig) %>%
            dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
        } else {
          contr_df <- contr_df %>% dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
        }
        res_table <- contr_df
      } else {
        res_table <- if (nrow(df)) df %>% dplyr::mutate(across(where(is.numeric), ~ round(., 3))) else data.frame(message = "No pairwise results")
      }
    }

    # Suppress DataTables warnings and render table
    suppressWarnings({
      DT::datatable(res_table, options = list(
        pageLength = 10,
        columnDefs = list(list(targets = "_all", className = "dt-center")),
        dom = 'tip',
        scrollX = TRUE,
        language = list(emptyTable = "No data available")
      ))
    })
  })
}

shinyApp(ui, server)


