#' Plot a single-layer SpatRaster
#'
#' @family Plotting
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Must be single-layer.
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal raster map with a fixed aspect ratio and no axis
#' expansion. The returned ggplot carries no theme, colour scale, or axis
#' labels — add these with `+` using standard ggplot2 conventions.
#'
#' When raster unit metadata is present, the y-axis tick labels include the
#' unit suffix such as `0 cm` or `1.5 cm`. If no unit metadata exists, ggplot2
#' default labels are used, showing pixel coordinates.
#'
#' This is a temporary workaround that avoids tidyterra and plots the raster
#' directly with [`ggplot2::geom_raster()`].
#'
#' @seealso
#' [`hsi_plot_spatraster_rgb()`] for three-layer RGB plots.
#' [`hsi_plot_profile()`] for 1-D depth profiles.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif") |> terra::subset(1)
#'
#' # Quick pixel-space plot
#' x_spatraster <- hsi_plot_spatraster(x)
#'
#' # Physical-space plot after calibration
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
#' x_physical <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
#' x_spatraster <- hsi_plot_spatraster(x_physical)
#'
#' # Add labels and theme with ggplot2
#' x_spatraster +
#'   ggplot2::labs(x = "Width (cm)", y = "Depth (cm)", fill = "RABD") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @export
hsi_plot_spatraster <- function(x) {
  # Validate inputs
  check_spatraster(x)

  if (terra::nlyr(x) > 1) {
    cli::cli_abort(
      "{.arg x} has {.val {terra::nlyr(x)}} layers. Only one is accepted."
    )
  }

  check_crs_null(x)

  units <- hsi_get_units(x)
  label_fun <- if (!is.null(units)) hsi_unit_label_fun(units) else ggplot2::waiver()

  band_name <- terra::names(x)[1]
  raster_df <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
  names(raster_df)[names(raster_df) == band_name] <- "value"

  ggplot2::ggplot(raster_df) +
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::scale_y_reverse(labels = label_fun) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_fixed(expand = FALSE)
}
