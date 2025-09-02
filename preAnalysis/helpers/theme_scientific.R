# Scientific Theme for Consistent Plotting
# Save this as: analysis/helpers/theme_scientific.R

library(ggplot2)
library(scales)

# Main scientific theme
theme_scientific <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # Panel and plot background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      
      # Grid lines
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_line(color = "grey95", size = 0.2),
      
      # Axis elements
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text = element_text(color = "black", size = rel(0.9)),
      axis.title = element_text(color = "black", size = rel(1.1), face = "bold"),
      
      # Strip (facet) elements
      strip.background = element_rect(fill = "grey95", color = "black", size = 0.5),
      strip.text = element_text(color = "black", size = rel(1.0), face = "bold"),
      strip.text.x = element_text(margin = margin(t = 3, b = 3)),
      strip.text.y = element_text(margin = margin(l = 3, r = 3)),
      
      # Legend
      legend.background = element_rect(fill = "white", color = "black", size = 0.3),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "cm"),
      legend.title = element_text(face = "bold", size = rel(1.0)),
      legend.text = element_text(size = rel(0.9)),
      legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
      
      # Plot title and subtitle
      plot.title = element_text(size = rel(1.3), face = "bold", hjust = 0.5, 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0.5, 
                                   margin = margin(b = 15)),
      plot.caption = element_text(size = rel(0.8), hjust = 1, 
                                  margin = margin(t = 10)),
      
      # Margins
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Color palettes for different purposes
scientific_colors <- list(
  # For categorical data (up to 8 categories)
  categorical = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"),
  
  # For experiment versions (3 categories)
  exp_version = c("#2E86AB", "#A23B72", "#F18F01"),
  
  # For deviation comparisons (focused on -1, 0)
  deviation = c("#d73027", "#f46d43", "#fdae61", "#fee08b", 
                "#e6f598", "#abdda4", "#66c2a5", "#3288bd"),
  
  # For small sets of comparisons
  binary = c("#E74C3C", "#3498DB"),
  
  # Grayscale for when color isn't needed
  grayscale = c("#2C2C2C", "#5A5A5A", "#888888", "#B6B6B6")
)

# Helper function to get colors
get_scientific_colors <- function(palette = "categorical", n = NULL) {
  colors <- scientific_colors[[palette]]
  if (!is.null(n) && n <= length(colors)) {
    return(colors[1:n])
  }
  return(colors)
}

# Save function with high quality settings
save_scientific_plot <- function(plot, filename, width = 8, height = 6, 
                                 dpi = 300, path = "figures/drafts") {
  # Ensure directory exists
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Full file path
  full_path <- file.path(path, paste0(filename, ".png"))
  
  # Save with high quality settings
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
  return(full_path)
}