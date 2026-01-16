#' Overlay color plot of proxy on RGB
#'
#' @family Plotting
#' @param x raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param alpha a number in (0, 1) controlling transparency.
#' @param ... additional arguments.
#'
#' @return a plot with color map of selected hyperspectral index overlain on RGB image.
#' @export
plot_raster_overlay <- function(
  x,
  index,
  alpha = 0.5,
  ...
) {
  # Validate input
  check_spatraster(x)

  if (!inherits(index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Subset SpatRaster
  hsi_layer <- x |>
    terra::subset(index)

  x <- HSItools::wavelength_position(
    x = x,
    wavelength = c(650, 550, 450)
  ) |>
    HSItools::wavelength_sub(
      x = x,
      wavelength_tbl = _
    )

  # Stretch SpatRaster
  x <- terra::stretch(x)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add RGB raster layer
    tidyterra::geom_spatraster_rgb(
      data = x,
      r = 1,
      g = 2,
      b = 3,
      interpolate = TRUE
    ) +
    # Add raster layer
    tidyterra::geom_spatraster(
      data = hsi_layer,
      interpolate = TRUE
    ) +
    # Define fill colors
    ggplot2::scale_fill_viridis_c(
      alpha = alpha,
      guide = ggplot2::guide_colorbar(
        title = index,
        title.position = "bottom",
        ticks = FALSE
      )
    ) +
    # Fix the coordinates
    ggplot2::coord_fixed() +
    # Modify theme
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.line.y.left = ggplot2::element_line(color = "black"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "RGB",
      y = "Depth"
    )

  # Return plot as an object
  return(plot)
}