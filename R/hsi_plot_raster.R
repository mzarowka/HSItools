#' Plot spatial map plots of calculated proxies, and optionally save to file
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param ... additional arguments.
#'
#' @importFrom ggplot2 theme
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
hsi_plot_raster <- function(
  x,
  index,
  calibration = NULL,
  ...
) {
  # Logic should be as follow:
  # Calculate index - user
  # Plot raster - user
  # # Validate input
  # check_spatraster(x)
  # if (!inherits(index, what = "character")) {
  #   rlang::abort(message = "Supplied index name is not a character.")
  # }
  # # Subset SpatRaster
  # hsi_layer <- x |>
  #   terra::subset(index)
  # if (is.null(calibration)) {
  #   # Plot SpatRaster
  #   plot <- ggplot2::ggplot() +
  #     # Add raster layer
  #     tidyterra::geom_spatraster(data = hsi_layer) +
  #     # Fix the coordinates
  #     ggplot2::coord_fixed() +
  #     # Modify theme
  #     ggplot2::theme(
  #       panel.background = ggplot2::element_blank(),
  #       axis.line.y.left = ggplot2::element_line(color = "black"),
  #       axis.text.x = ggplot2::element_blank(),
  #       axis.ticks.x = ggplot2::element_blank(),
  #       legend.position = "bottom"
  #     ) +
  #     ggplot2::labs(
  #       x = index,
  #       y = "Depth (px)",
  #       fill = "Value"
  #     )
  # } else {
  #   # Plot SpatRaster
  #   plot <- ggplot2::ggplot() +
  #     # Add raster layer
  #     tidyterra::geom_spatraster(data = hsi_layer) +
  #     # Fix the coordinates
  #     ggplot2::coord_fixed() +
  #     # Modify Y scale
  #     ggplot2::scale_y_continuous(
  #       labels = \(i) {
  #         format(
  #           terra::round(
  #             -1 *
  #               i *
  #               calibration$pixel_ratio +
  #               calibration$distance -
  #               calibration$point_zero
  #           )
  #         )
  #       },
  #       breaks = scales::breaks_pretty()
  #     ) +
  #     # Modify theme
  #     ggplot2::theme(
  #       panel.background = ggplot2::element_blank(),
  #       axis.line.y.left = ggplot2::element_line(color = "black"),
  #       axis.text.x = ggplot2::element_blank(),
  #       axis.ticks.x = ggplot2::element_blank(),
  #       legend.position = "bottom"
  #     ) +
  #     ggplot2::labs(
  #       x = index,
  #       y = "Depth (mm)",
  #       fill = "Value"
  #     )
  # }
  # # Return plot as an object
  # return(plot)
}
