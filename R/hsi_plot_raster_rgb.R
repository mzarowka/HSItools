#' Plot a three-layer pseudoRGB SpatRaster
#'
#' @family Plotting
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Must have exactly three layers.
#' @param stretch Character. Contrast stretch applied before rendering. One of
#'   `NULL` (no stretch), `"lin"` (linear stretch via `terra::stretch()`), or
#'   `"hist"` (histogram equalization via `terra::stretch()`). Default `NULL`.
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal RGB raster map with a fixed aspect ratio and no axis
#' expansion. The returned ggplot carries no theme or axis labels — add
#' these with `+` using standard ggplot2 conventions.
#'
#' When raster unit metadata is present, the y-axis tick labels include the
#' unit suffix such as `0 cm` or `1.5 cm`. If no unit metadata exists, ggplot2
#' default labels are used, showing pixel coordinates.
#' Any three-band combination can be used — RGB, CIR, SWIR false colour, or
#' any other composite.
#'
#' @seealso
#' [`hsi_plot_raster()`] for single-layer plots.
#' [`hsi_plot_profile()`] for 1-D depth profiles.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif") |> terra::subset(1:3)
#'
#' # Quick pixel-space RGB plot
#' x_rgb <- hsi_plot_spatraster_rgb(x)
#'
#' # With linear stretch
#' x_rgb <- hsi_plot_spatraster_rgb(x, stretch = "lin")
#'
#' # Physical-space plot after calibration
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
#' x_physical <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
#' x_rgb <- hsi_plot_spatraster_rgb(x_physical)
#'
#' # Add labels and theme with ggplot2
#' x_rgb +
#'   ggplot2::labs(x = "Width (cm)", y = "Depth (cm)") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @export
hsi_plot_raster_rgb <- function(
  x,
  stretch = NULL
) {
  # Validate inputs
  check_spatraster(x)

  if (terra::nlyr(x) != 3) {
    cli::cli_abort(
      "{.arg x} has {.val {terra::nlyr(x)}} layers. Exactly three are needed."
    )
  }

  check_crs_null(x)

  units <- hsi_get_units(x)
  label_fun <- if (!is.null(units)) {
    hsi_unit_label_fun(units)
  } else {
    ggplot2::waiver()
  }

  x_rgb <- terra::colorize(
    x,
    to = "col",
    stretch = if (is.null(stretch)) NULL else stretch
  )

  plot <- ggplot2::ggplot(
    x_rgb,
    ggplot2::aes(x, y, fill = value),
    pivot = TRUE
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::scale_y_reverse(labels = label_fun) +
    ggplot2::labs(x = "", y = "")

  plot
}
