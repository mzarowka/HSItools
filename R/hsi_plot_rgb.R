#' Spatial map plots of RGB image
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers or just RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param ... additional arguments.
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_rgb <- function(
  x,
  calibration = NULL,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Check if there are values close to RGB, within the 25 nm.
  if (
    all(
      any(
        purrr::list_c(
          purrr::map(
            c(640, 545, 460),
            \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = 25)
          )
        )
      )
    ) ==
      TRUE
  ) {
    spectra <- c(640, 545, 460)
  } else {
    cli::cli_alert_warning(
      "No layers matching the RGB. Using the first, middle and last available layers."
    )
    wavelength <- c(
      min(1:terra::nlyr(x)),
      terra::median(1:terra::nlyr(x)),
      max(1:terra::nlyr(x))
    ) |>
      (\(i) as.numeric(terra::names(terra::subset(x, i))))()
  }

  # Prepare SpatRaster
  x <- HSItools::wavelength_position(
    x = x,
    wavelength = wavelength
  ) |>
    HSItools::wavelength_sub(
      x = x,
      wavelength_tbl = _
    )

  # Stretch SpatRaster
  x <- terra::stretch(x)

  if (is.null(calibration)) {
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
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "RGB",
        y = "Depth (px)"
      )
  } else {
    plot <- ggplot2::ggplot() +
      # Add RGB raster layer
      tidyterra::geom_spatraster_rgb(
        data = x,
        r = 1,
        g = 2,
        b = 3,
        interpolate = TRUE
      ) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) {
          format(
            terra::round(
              -1 *
                i *
                calibration$pixel_ratio +
                calibration$distance -
                calibration$point_zero
            )
          )
        },
        breaks = scales::breaks_pretty()
      ) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "RGB",
        y = "Depth (mm)"
      )
  }

  # Return plot as an object
  return(plot)
}