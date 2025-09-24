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
  "corrected_width_deviation", "corrected_width_deviation_relative",
  "pooled_width_deviation", "edge_to_edge_spacing_deviation", "correct_space_category",
  "exp_version", "rm_type", "emmean", "lower.CL", "upper.CL", "n",
  "subID", "mean_outcome", "ci_lower", "ci_upper", "trial_type"
))
setwd("G:/My Drive/Projects/RM/RM_loss_n_gain")
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
  titlePanel("Redundancy Masking Explorer - Exp1"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
          "Baseline-Corrected Width Deviation (removes individual bias)" = "corrected_width_deviation",
          "Baseline-Corrected Relative Width Deviation (bias-corrected relative)" = "corrected_width_deviation_relative",
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
        choices = c("None"),
        selected = "None"
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
      uiOutput("x_axis_feedback"),
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
  processed_path <- find_data_file("data/processed.csv")
  df_raw <- read.csv(processed_path) #%>% filter(correct_space %in% c(0.7, 0.9))

  # Precompute helper columns
  df_raw <- df_raw %>%
    dplyr::mutate(
      rm_type = factor(dplyr::if_else(number_deviation == -1, "RM", "NoRM"), levels = c("NoRM", "RM")),
      spacing_category = factor(dplyr::if_else(!is.na(spacing), spacing, NA_character_),
                                levels = c("small", "middle", "large")),
      correct_num = factor(correct_num),
      correct_width = factor(correct_width)
    )
  
  # Create correct_space_category factor for exp1 (formerly Exp1B)
  df_raw <- df_raw %>%
    dplyr::mutate(
      correct_space_category = factor(as.character(correct_space), 
                                    levels = c("0.7", "0.9", "1.1"),
                                    labels = c("0.7", "0.9", "1.1"))
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
  
  # Add baseline-corrected variables if missing
  if (!"corrected_width_deviation" %in% names(df_raw)) {
    df_raw <- df_raw %>%
      dplyr::mutate(corrected_width_deviation = NA_real_)
  }
  if (!"corrected_width_deviation_relative" %in% names(df_raw)) {
    df_raw <- df_raw %>%
      dplyr::mutate(corrected_width_deviation_relative = NA_real_)
  }
  
  # Check if trial_type column exists, create placeholder if missing
  if (!"trial_type" %in% names(df_raw)) {
    df_raw$trial_type <- "all"  # Default all trials to "all" type
    warning("trial_type column not found in data. All trials treated as 'all' type.")
  }


  # Update factor choices based on trial type for single experiment
  observe({
    req(input$trial_type)
    
    # Capture current selections before updating
    current_factors <- input$factors
    current_model_vars <- input$model_vars
    
    # Determine spacing variable based on trial type
    spacing_var <- if (input$trial_type == "initial") "correct_space_category" else "spacing_category"
    spacing_label <- if (input$trial_type == "initial") "correct_space_category" else "spacing_category"
    
    # For exp1 (formerly Exp1B), both spacing variables are available
    factor_choices <- c(
      "correct_num" = "correct_num", 
      "correct_width" = "correct_width"
    )
    spacing_choice <- setNames(spacing_var, spacing_label)
    factor_choices <- c(spacing_choice, factor_choices)
    
    model_choices <- c(
      "correct_num" = "correct_num",
      "correct_width" = "correct_width"
    )
    model_choices <- c(spacing_choice, model_choices)
    
    # Preserve selections where possible
    available_factor_values <- unname(factor_choices)
    available_model_values <- unname(model_choices)
    
    preserved_factors <- intersect(current_factors, available_factor_values)
    preserved_model_vars <- intersect(current_model_vars, available_model_values)
    
    # Update inputs with preserved selections
    updateCheckboxGroupInput(session, "factors", choices = factor_choices, 
                           selected = if(length(preserved_factors) > 0) preserved_factors else unname(factor_choices)[length(factor_choices)])
    updateCheckboxGroupInput(session, "model_vars", choices = model_choices, 
                           selected = if(length(preserved_model_vars) > 0) preserved_model_vars else unname(model_choices))
  })

  # Reactive value to track x-axis feedback messages
  x_axis_message <- reactiveVal("")
  
  # Observer to synchronize x-axis choices with selected factors
  observe({
    req(input$factors, input$trial_type)
    
    # Get current x-axis selection to preserve it if possible
    current_x_axis <- input$x_axis
    
    # Get currently selected factors
    selected_factors <- input$factors
    
    # For exp1 (single experiment), all factors are available
    available_factors <- selected_factors
    
    # Build x-axis choices: "None" + available selected factors
    x_axis_choices <- c("None", available_factors)
    names(x_axis_choices) <- x_axis_choices
    
    # Preserve current selection if still valid, otherwise reset to "None"
    new_selection <- if (current_x_axis %in% x_axis_choices) current_x_axis else "None"
    
    # Set feedback message if selection was changed
    if (current_x_axis != "None" && current_x_axis != new_selection) {
      if (!current_x_axis %in% selected_factors) {
        # Variable was unchecked from factors
        x_axis_message(paste0("ℹ️ X-axis reset: '", current_x_axis, "' was unchecked in factors"))
      } else {
        x_axis_message("")
      }
    } else {
      x_axis_message("")
    }
    
    # Update x-axis dropdown
    updateSelectInput(session, "x_axis", 
                     choices = x_axis_choices, 
                     selected = new_selection)
  })

  # Separate observer for emmeans choices - only update when trial type changes
  # This prevents interference with user emmeans interactions
  observe({
    req(input$trial_type)
    
    # Use isolate to prevent reactive cascade from current emmeans selections
    current_emm_vars <- isolate(input$emm_vars)
    
    spacing_var <- if (input$trial_type == "initial") "correct_space_category" else "spacing_category"
    
    # For exp1 (single experiment), all variables are available
    emm_choices <- c(spacing_var, "correct_num", "correct_width")
    
    # Only preserve selections that are still valid
    preserved_emm_vars <- if (is.null(current_emm_vars)) {
      character(0)
    } else {
      intersect(current_emm_vars, emm_choices)
    }
    
    # Update emmeans choices - this should be less disruptive now
    updateCheckboxGroupInput(session, "emm_vars", choices = emm_choices, 
                           selected = preserved_emm_vars)
  })

  # Filtered data according to selections
  data_filtered <- reactive({
    req(input$outcome, input$trial_type)
    dat <- df_raw %>%
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
    # Clean rows for baseline-corrected variables
    if (outcome_var == "corrected_width_deviation" && any(is.infinite(dat$corrected_width_deviation) | is.na(dat$corrected_width_deviation))) {
      dat <- dat %>% dplyr::filter(!is.na(corrected_width_deviation), is.finite(corrected_width_deviation))
    }
    if (outcome_var == "corrected_width_deviation_relative" && any(is.infinite(dat$corrected_width_deviation_relative) | is.na(dat$corrected_width_deviation_relative))) {
      dat <- dat %>% dplyr::filter(!is.na(corrected_width_deviation_relative), is.finite(corrected_width_deviation_relative))
    }
    dat
  })

  # Build model formula dynamically
  model_formula_reactive <- reactive({
    req(input$outcome, input$model_vars, input$trial_type)
    # Always include rm_type; use model variables selection
    selected <- input$model_vars
    # Include both possible spacing variables for dynamic selection
    selected <- selected[selected %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # For single experiment, only use rm_type as base
    base_vars <- c("rm_type")
    
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
    req(input$outcome, input$x_axis)
    pretty_outcome <- switch(input$outcome,
                             width_deviation = "Width Deviation",
                             absolute_width_deviation = "Absolute Width Deviation",
                             spacing_deviation = "Spacing Deviation",
                             width_deviation_relative = "Relative Width Deviation",
                             spacing_deviation_relative = "Relative Spacing Deviation",
                             corrected_width_deviation = "Baseline-Corrected Width Deviation",
                             corrected_width_deviation_relative = "Baseline-Corrected Relative Width Deviation",
                             pooled_width_deviation = "Pooled Width Deviation",
                             width_density_deviation = "Width Density Deviation",
                             edge_to_edge_spacing_deviation = "Edge-to-Edge Spacing Deviation",
                             input$outcome)
    x_part <- if (input$x_axis == "None") "" else paste0(" by ", gsub("_", " ", input$x_axis))
    
    h3(paste0("Exp1 — ", pretty_outcome, ": RM vs NoRM", x_part))
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
      corrected_width_deviation = "corrected_width_deviation = width_deviation - individual_baseline_bias<br/>removes systematic individual biases using one-bar baseline data",
      corrected_width_deviation_relative = "corrected_width_deviation_relative = corrected_width_deviation / correct_width<br/>baseline-corrected deviation normalized by correct width",
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
    req(nrow(dat) > 0, input$trial_type)
    xvar <- input$x_axis
    
    # Validate that x-axis variable exists in the data and is selected in factors when not "None"
    if (xvar != "None" && (!xvar %in% names(dat) || !xvar %in% input$factors)) {
      # This should now be prevented by the x-axis synchronization observer
      xvar <- "None"
    }
    
    selected <- unique(c(input$factors, xvar))
    # Include both possible spacing variables for dynamic selection
    selected <- selected[selected %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure all selected factors exist in the data
    selected <- selected[selected %in% names(dat)]

    # Single experiment mode (no multi-experiment logic needed)
    multi_exp <- FALSE
    
    # Compute participant-level means then grand means with 95% CI
    outcome_col <- input$outcome

    # Group variables for single experiment
    group_vars <- c("rm_type", input$factors)
    group_vars <- group_vars[group_vars %in% c("rm_type", "spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure all group variables exist in the actual data
    group_vars <- group_vars[group_vars %in% names(dat)]

    subject_means <- dat %>%
      dplyr::group_by(subID, dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(mean_outcome = mean(.data[[outcome_col]], na.rm = TRUE), n_trials = dplyr::n(), .groups = "drop")

    # Group by condition variables only (not subID)
    condition_vars <- c("rm_type", input$factors)
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

    # Build faceting variables for single experiment
    if (input$x_axis == "None") {
      facet_vars <- input$factors
    } else {
      facet_vars <- setdiff(input$factors, input$x_axis)
    }
    facet_vars <- facet_vars[facet_vars %in% c("spacing_category", "correct_space_category", "correct_num", "correct_width")]
    
    # Additional validation: ensure facet variables exist in the grand data
    facet_vars <- facet_vars[facet_vars %in% names(grand)]
    
    if (length(facet_vars) > 0) {
      grand$facet_label <- do.call(interaction, c(grand[facet_vars], list(drop = TRUE, sep = " • ")))
    }

    # Build aesthetic mappings based on visualization mode  
    # Use validated xvar (which may have been reset to "None" if invalid)
    x_aesthetic <- if (xvar == "None") rlang::sym("rm_type") else rlang::sym(xvar)
    x_label <- if (input$x_axis == "None") "Condition" else gsub("_", " ", xvar)
    
    # Single experiment plotting
    p <- ggplot2::ggplot(grand, ggplot2::aes(x = !!x_aesthetic, y = mean_outcome, color = rm_type)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.0, size = 1.2, position = ggplot2::position_dodge(width = 0.5), alpha = 0.8) +
      ggplot2::geom_point(size = 5, alpha = 0.9, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::scale_color_manual(values = c("RM" = RM_COLOR, "NoRM" = NORM_COLOR)) +
      ggplot2::labs(x = x_label, y = input$outcome, color = "Condition")

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
    
    # Single experiment info
    n_trials <- nrow(dat)
    n_participants <- dplyr::n_distinct(dat$subID)
    HTML(sprintf("<b>Sample size:</b> Exp1 - %s trials, %s participants%s", n_trials, n_participants, trial_type_text))
  })

  # Model formula display
  output$model_formula <- renderUI({
    fml <- model_formula_reactive()
    HTML(paste0("<b>Model:</b> ", rlang::expr_text(fml)))
  })

  # No emmeans warning needed for single experiment
  output$emmeans_warning <- renderUI({
    HTML("")
  })

  # X-axis feedback messages
  output$x_axis_feedback <- renderUI({
    msg <- x_axis_message()
    if (msg != "") {
      HTML(paste0("<div style='background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 8px; margin-bottom: 10px; color: #0c5460; font-size: 14px;'>", msg, "</div>"))
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


