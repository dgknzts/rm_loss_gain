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
  "exp_version", "rm_type", "emmean", "lower.CL", "upper.CL", "n",
  "subID", "mean_outcome", "ci_lower", "ci_upper"
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
      radioButtons(
        inputId = "exp_select",
        label = NULL,
        choices = c("Exp1A", "Exp1B", "Exp1C"),
        selected = "Exp1A"
      ),
      tags$hr(),
      h4("OUTCOME VARIABLE"),
      selectInput(
        inputId = "outcome",
        label = NULL,
        choices = c(
          "width_deviation" = "width_deviation",
          "spacing_deviation" = "spacing_deviation",
          "width_density_deviation" = "width_density_deviation",
          "relative_width_deviation" = "width_deviation_relative",
          "relative_spacing_deviation" = "spacing_deviation_relative"
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

  # Experiments available (single select)
  observe({
    updateRadioButtons(session, "exp_select", choices = sort(unique(df_raw$exp_version)), selected = sort(unique(df_raw$exp_version))[1])
  })

  # Update factor choices based on experiment - exclude spacing_category for Exp1C
  observe({
    if (input$exp_select == "Exp1C") {
      # Exp1C has fixed spacing, so exclude spacing_category
      factor_choices <- c("correct_num", "correct_width")
      x_axis_choices <- c("None", "correct_num", "correct_width")
      model_choices <- c("correct_num", "correct_width")
      emm_choices <- c("correct_num", "correct_width")
    } else {
      # Other experiments include all factors
      factor_choices <- c("spacing_category", "correct_num", "correct_width")
      x_axis_choices <- c("None", "spacing_category", "correct_num", "correct_width")
      model_choices <- c("spacing_category", "correct_num", "correct_width")
      emm_choices <- c("spacing_category", "correct_num", "correct_width")
    }
    
    # Update all relevant inputs
    updateCheckboxGroupInput(session, "factors", choices = factor_choices)
    updateCheckboxGroupInput(session, "model_vars", choices = model_choices, 
                           selected = model_choices)  # Keep all available selected
    updateSelectInput(session, "x_axis", choices = x_axis_choices)
    updateCheckboxGroupInput(session, "emm_vars", choices = emm_choices)
  })

  # Filtered data according to selections
  data_filtered <- reactive({
    req(input$exp_select, input$outcome)
    dat <- df_raw %>%
      dplyr::filter(exp_version == input$exp_select) %>%
      dplyr::filter(number_deviation %in% c(-1, 0)) %>%
      dplyr::filter(!is.na(rm_type))

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
    req(input$outcome, input$model_vars)
    # Always include rm_type; use model variables selection
    selected <- input$model_vars
    selected <- selected[selected %in% c("spacing_category", "correct_num", "correct_width")]
    rhs <- if (length(selected) == 0) {
      "rm_type"
    } else if (isTRUE(input$include_interactions)) {
      paste(c("rm_type", selected), collapse = " * ")
    } else {
      paste(c("rm_type", selected), collapse = " + ")
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
    mdl <- lme4::lmer(fml, data = dat, control = lme4::lmerControl(check.rankX = "ignore"))

    # Use independent emmeans conditioning vars (emm_vars)
    cond_vars <- input$emm_vars
    if (is.null(cond_vars)) cond_vars <- character(0)

    # Handle None: only rm_type
    spec_rhs <- if (length(cond_vars) > 0) paste(cond_vars, collapse = " * ") else "1"
    spec_formula <- stats::as.formula(paste("~ rm_type |", spec_rhs))
    em <- emmeans::emmeans(mdl, specs = spec_formula)
    as.data.frame(stats::confint(em, level = 0.95, adjust = "none"))
  })

  # Title reflecting selection
  output$plot_title <- renderUI({
    req(input$outcome, input$x_axis)
    pretty_outcome <- switch(input$outcome,
                             width_deviation = "Width Deviation",
                             spacing_deviation = "Spacing Deviation",
                             width_density_deviation = "Density Deviation",
                             input$outcome)
    x_part <- if (input$x_axis == "None") "" else paste0(" by ", gsub("_", " ", input$x_axis))
    h3(paste0(input$exp_select, " — ", pretty_outcome, ": RM vs NoRM", x_part))
  })

  # Short note explaining the selected outcome computation
  output$outcome_note <- renderUI({
    req(input$outcome)
    note <- switch(input$outcome,
      width_deviation = "Outcome: width_deviation = response_width - correct_width",
      spacing_deviation = "Outcome: spacing_deviation = response_space - correct_space",
      width_density_deviation = "Outcome: width_density_deviation = response_width_density - actual_width_density",
      width_deviation_relative = "Outcome: relative_width_deviation = (response_width - correct_width) / correct_width",
      spacing_deviation_relative = "Outcome: relative_spacing_deviation = (response_space - correct_space) / correct_space",
      paste0("Outcome: ", input$outcome)
    )
    HTML(paste0("<i>", note, "</i>"))
  })

  # Plot
  plot_obj <- reactive({
    dat <- data_filtered()
    req(nrow(dat) > 0)
    xvar <- input$x_axis
    selected <- unique(c(input$factors, xvar))
    selected <- selected[selected %in% c("spacing_category", "correct_num", "correct_width")]

    # Ensure factor types for ggplot ordering consistency
    # Ensure factors already set upstream

    # Compute participant-level means then grand means with 95% CI
    outcome_col <- input$outcome

    # Always group by rm_type plus all selected plotting factors for CI calculation
    group_vars <- c("rm_type", input$factors)
    group_vars <- group_vars[group_vars %in% c("rm_type", "spacing_category", "correct_num", "correct_width")]

    subject_means <- dat %>%
      dplyr::group_by(subID, dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(mean_outcome = mean(.data[[outcome_col]], na.rm = TRUE), n_trials = dplyr::n(), .groups = "drop")

    # Group by condition variables only (not subID)
    condition_vars <- c("rm_type", input$factors)
    condition_vars <- condition_vars[condition_vars %in% names(subject_means)]
    
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

    # Build facet label early so all layers can reference it
    if (input$x_axis == "None") {
      facet_vars <- input$factors
    } else {
      facet_vars <- setdiff(input$factors, input$x_axis)
    }
    facet_vars <- facet_vars[facet_vars %in% c("spacing_category", "correct_num", "correct_width")]
    
    if (length(facet_vars) > 0) {
      grand$facet_label <- do.call(interaction, c(grand[facet_vars], list(drop = TRUE, sep = " • ")))
    }

    aes_x <- rlang::sym(xvar)

    # Base ggplot (x is dummy when None)
    p <- ggplot2::ggplot(grand, ggplot2::aes(x = if (input$x_axis == "None") rm_type else !!aes_x, y = mean_outcome, color = rm_type)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.0, size = 1.2, position = ggplot2::position_dodge(width = 0.5), alpha = 0.8) +
      ggplot2::geom_point(size = 5, alpha = 0.9, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::scale_color_manual(values = c("RM" = RM_COLOR, "NoRM" = NORM_COLOR)) +
      ggplot2::labs(x = if (input$x_axis == "None") "Condition" else gsub("_", " ", xvar), y = input$outcome, color = "Condition")

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
    req(nrow(dat) > 0)
    n_trials <- nrow(dat)
    n_participants <- dplyr::n_distinct(dat$subID)
    HTML(sprintf("<b>Sample size:</b> %s %s trials, %s participants", input$exp_select, n_trials, n_participants))
  })

  # Model formula display
  output$model_formula <- renderUI({
    fml <- model_formula_reactive()
    HTML(paste0("<b>Model:</b> ", rlang::expr_text(fml)))
  })

  # Data availability / warnings
  output$warnings <- renderUI({
    dat <- data_filtered()
    req(nrow(dat) > 0)
    xvar <- input$x_axis
    selected <- unique(c(input$factors, xvar))
    selected <- selected[selected %in% c("spacing_category", "correct_num", "correct_width")]
    by_vars <- unique(c("rm_type", selected))
    # Count combinations
    counts <- dat %>% dplyr::count(dplyr::across(dplyr::all_of(by_vars)), name = "n")
    empty <- counts %>% dplyr::filter(is.na(n) | n == 0)
    if (nrow(empty) > 0) {
      HTML("<span style='color:#E74C3C'><b>Warning:</b> Some factor combinations have zero trials and are excluded.</span>")
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
    mdl <- lme4::lmer(fml, data = dat, control = lme4::lmerControl(check.rankX = "ignore"))

    by_vars <- input$emm_vars
    if (is.null(by_vars)) by_vars <- character(0)
    spec_rhs <- if (length(by_vars) > 0) paste(by_vars, collapse = " * ") else "1"
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


