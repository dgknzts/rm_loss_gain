# ==============================================================================
# SCIENTIFIC PLOTTING THEMES AND COLOR PALETTES
# ==============================================================================
# Publication-ready themes and color schemes for redundancy masking analysis
# ==============================================================================

library(ggplot2)
library(scales)

# ==============================================================================
# MAIN SCIENTIFIC THEME
# ==============================================================================

#' Publication-ready scientific theme
#' @param base_size Base font size
#' @param base_family Base font family
#' @return ggplot2 theme object
theme_scientific <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Panel and plot background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey92", size = 0.5),
      panel.grid.minor = element_blank(),
      
      # Axis elements
      axis.line = element_line(color = "grey20", size = 0.5),
      axis.ticks = element_line(color = "grey20", size = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text = element_text(color = "grey20", size = rel(0.9)),
      axis.title = element_text(color = "grey10", size = rel(1.1), face = "bold"),
      
      # Strip (facet) elements
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(color = "grey10", size = rel(1.0), face = "bold"),
      
      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(face = "bold", size = rel(1.0), color = "grey10"),
      legend.text = element_text(size = rel(0.9), color = "grey20"),
      legend.position = "bottom",
      
      # Plot title and subtitle
      plot.title = element_text(size = rel(1.3), face = "bold", hjust = 0.5, 
                                color = "grey10", margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0.5, 
                                   color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(size = rel(0.8), hjust = 1, 
                                  color = "grey40", margin = margin(t = 10)),
      
      # Spacing and margins
      panel.spacing = unit(1, "lines"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Minimal theme for simple plots
theme_minimal_scientific <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = rel(1.1), face = "bold"),
      axis.text = element_text(size = rel(0.9)),
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = rel(1.0), hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )
}

# ==============================================================================
# COLOR PALETTES
# ==============================================================================

#' Redundancy Masking color scheme
rm_color_palette <- list(
  # Primary RM vs NoRM colors
  primary = c("NoRM" = "#2E8B57", "RM" = "#DC143C"),  # Sea green and crimson
  
  # Alternative RM colors
  alternative = c("NoRM" = "#3498DB", "RM" = "#E74C3C"),  # Blue and red
  
  # Width condition colors (for 3 levels typically)
  width_conditions = c("#1B4F72", "#7D3C98", "#B7950B"),  # Deep blue, purple, gold
  
  # Spacing categories
  spacing_categories = c("Small" = "#2E8B57", "Medium" = "#FFD700", "Large" = "#DC143C"),
  
  # Viridis-based for continuous variables
  continuous = viridis::viridis(5, option = "plasma", begin = 0.2, end = 0.8),
  
  # Grayscale options
  grayscale = c("#2C3E50", "#5D6D7E", "#85929E", "#B2BEB5")
)

#' Get RM-specific colors
#' @param palette Name of color palette from rm_color_palette
#' @param n Number of colors to return (if applicable)
#' @return Vector of hex color codes
get_rm_colors <- function(palette = "primary", n = NULL) {
  colors <- rm_color_palette[[palette]]
  if (!is.null(n) && length(colors) >= n) {
    return(colors[1:n])
  }
  return(colors)
}

# ==============================================================================
# PLOT HELPER FUNCTIONS
# ==============================================================================

#' Add reference line at y = 0
#' @param linetype Line type for reference line
#' @param color Color for reference line  
#' @param alpha Transparency for reference line
add_zero_line <- function(linetype = "dashed", color = "grey50", alpha = 0.8) {
  geom_hline(yintercept = 0, linetype = linetype, color = color, alpha = alpha)
}

#' Format p-values for plot labels
#' @param p_values Vector of p-values
#' @return Formatted p-value strings
format_p_values <- function(p_values) {
  case_when(
    p_values < 0.001 ~ "p < .001",
    p_values < 0.01 ~ paste0("p = ", sprintf("%.3f", p_values)),
    p_values < 0.05 ~ paste0("p = ", sprintf("%.3f", p_values)),
    TRUE ~ paste0("p = ", sprintf("%.3f", p_values))
  )
}

#' Create publication-ready axis labels
#' @param variable_name Variable name to format
#' @return Formatted axis label
format_axis_label <- function(variable_name) {
  case_when(
    variable_name == "width_deviation" ~ "Width Deviation (°)",
    variable_name == "spacing_deviation" ~ "Spacing Deviation (°)",
    variable_name == "width_density_deviation" ~ "Width Density Deviation",
    variable_name == "pooled_width_deviation" ~ "Pooled Width Deviation (°)",
    variable_name == "edge_to_edge_spacing_deviation" ~ "Edge-to-Edge Spacing Deviation (°)",
    variable_name == "width_deviation_relative" ~ "Relative Width Deviation (%)",
    variable_name == "spacing_deviation_relative" ~ "Relative Spacing Deviation (%)",
    TRUE ~ str_to_title(str_replace_all(variable_name, "_", " "))
  )
}

# ==============================================================================
# SAVE PLOT FUNCTIONS
# ==============================================================================

#' Save publication-ready plot
#' @param plot ggplot object to save
#' @param filename Filename (without extension)
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in dots per inch
#' @param path Directory path to save plot
#' @param formats Vector of file formats to save
save_publication_plot <- function(plot, filename, width = 8, height = 6, 
                                  dpi = 300, path = "outputs/figures", 
                                  formats = c("png", "pdf")) {
  # Ensure directory exists
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Save in each requested format
  for (format in formats) {
    full_path <- file.path(path, paste0(filename, ".", format))
    
    ggsave(
      filename = full_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      units = "in",
      bg = "white"
    )
    
    cat("Plot saved to:", full_path, "\n")
  }
  
  invisible(plot)
}