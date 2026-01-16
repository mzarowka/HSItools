#' Line plots of calculated proxies series
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
hsi_plot_profile <- function(
  x,
  index,
  calibration = NULL,
  ...
) {
  # Logic should be as follow:
  # Calculate index - user
  # Extract profile - user
  # Plot profile - user

  # # Validate input
  # check_spatraster(x)

  # if (!inherits(index, what = "character")) {
  #   rlang::abort(message = "Supplied index name is not a character.")
  # }

  # # Subset SpatRaster
  # hsi_layer <- x |>
  #   terra::subset(index)

  # # Proxy name
  # proxy_name <- rlang::as_label(rlang::enquo(index))

  # if (is.null(calibration)) {
  #   # Create a plot
  #   plot <- data |>
  #     # Pass to plot
  #     ggplot2::ggplot() +
  #     # Add aes
  #     ggplot2::aes(
  #       x = .data$proxy,
  #       y = .data$y
  #     ) +
  #     # Add geom
  #     ggplot2::geom_path() +
  #     # Modify theme
  #     ggplot2::theme(
  #       panel.background = ggplot2::element_blank(),
  #       axis.line = ggplot2::element_line(color = "black"),
  #       panel.border = ggplot2::element_rect(color = "black", fill = NA),
  #       legend.text.position = "bottom"
  #     ) +
  #     # Add labels
  #     ggplot2::labs(
  #       x = proxy_name,
  #       y = "Depth (px)"
  #     )
  # } else {
  #   # Create a plot
  #   plot <- data |>
  #     # Pass to plot
  #     ggplot2::ggplot() +
  #     # Add aes
  #     ggplot2::aes(
  #       x = .data$proxy,
  #       y = .data$y
  #     ) +
  #     # Add geom
  #     ggplot2::geom_path() +
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
  #       axis.line = ggplot2::element_line(color = "black"),
  #       panel.border = ggplot2::element_rect(color = "black", fill = NA),
  #       legend.text.position = "bottom"
  #     ) +
  #     # Add labels
  #     ggplot2::labs(
  #       x = proxy_name,
  #       y = "Depth (mm)"
  #     )
  # }

  # # Return plot as an object
  # return(plot)
}