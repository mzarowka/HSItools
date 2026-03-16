#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @family Plotting
#' @param x a SpatRaster with REFLECTANCE file. Used for correct placement.
#' @param plots a list of plots.
#' @param ... additional arguments.
#'
#' @return a plot.
#' @export
hsi_plot_composite <- function(
  x,
  plots,
  ...
) {
  # TODO: experimental fun, needs a proper refactor
  
  # Validate input
  check_spatraster(x)

  # Check if correct class is supplied.
  if (!inherits(plots, what = "list")) {
    rlang::abort(message = "Supplied data is not a list.")
  }

  # Create a plot composed from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect"
    )

  # Return plot as an object
  plot
}