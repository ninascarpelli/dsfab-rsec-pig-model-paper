#' Save a ggplot with optional auto-sizing
#'
#' This function saves a ggplot object to a file. If width and height are not provided,
#' it estimates the size based on the plot's grob layout.
#'
#' @param plot A ggplot2 object to be saved.
#' @param filename Character. File path to save the plot (e.g., "results/figures/plot.png").
#' @param width Numeric. Width of the plot in units (default is NULL, auto-calculated).
#' @param height Numeric. Height of the plot in units (default is NULL, auto-calculated).
#' @param dpi Numeric. Resolution in dots per inch. Default is 300.
#' @param units Character. Units for width and height. One of "in", "cm", or "mm". Default is "in".
#' @param scale Numeric. Scaling factor for the plot size. Default is 1.
#' @param ... Additional arguments passed to ggplot2::ggsave().
#'
#' @return NULL. Saves the plot to disk.
#' @examples
#' save_plot(my_plot, "results/figures/my_plot.png")
#' save_plot(my_plot, "results/figures/my_plot.pdf", width = 10, height = 5)
save_plot <- function(plot, filename, width = NULL, height = NULL, dpi = 300, units = "in", scale = 1, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("The 'grid' package is required but not installed.")
  }
  
  # Auto-calculate width and height if not provided
  if (is.null(width) || is.null(height)) {
    g <- ggplot2::ggplotGrob(plot)
    width <- as.numeric(grid::convertWidth(sum(g$widths), units, TRUE)) * scale
    height <- as.numeric(grid::convertHeight(sum(g$heights), units, TRUE)) * scale
  }
  
  message(sprintf("Saving plot to: %s (%.2f x %.2f %s)", filename, width, height, units))
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    ...
  )
}


#' Save a data frame to CSV
#'
#' @param data A data frame to save.
#' @param filename File path to save the CSV.
#' @param row.names Logical. Whether to include row names. Default is FALSE.
#' @param ... Additional arguments passed to write.csv().
#' @return NULL
#' @examples
#' save_csv(my_data, "results/tables/my_data.csv")
save_csv <- function(data, filename, row.names = FALSE, ...) {
  message(paste("Saving data to:", filename))
  write.csv(data, file = filename, row.names = row.names, ...)
}
