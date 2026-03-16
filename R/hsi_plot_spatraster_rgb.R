#' Plot a three-layer pseudoRGB SpatRaster
#'
#' @family Plotting
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Must have exactly three layers.
#' @param physical Logical. When `TRUE`, negates y-axis tick labels to display
#'   positive physical distances. Set `TRUE` when `x` has been processed by
#'   [`hsi_set_physical_extent()`]. Default `FALSE`.
#' @param stretch Character. Contrast stretch applied before rendering. One of
#'   `NULL` (no stretch), `"lin"` (linear, using 2nd and 98th percentiles), or
#'   `"hist"` (histogram equalization). Passed to
#'   [`tidyterra::geom_spatraster_rgb()`]. Default `NULL`.
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal RGB raster map with a fixed aspect ratio and no axis
#' expansion. The returned ggplot carries no theme or axis labels — add
#' these with `+` using standard ggplot2 conventions.
#'
#' When `physical = TRUE`, [`ggplot2::scale_y_continuous()`] applies
#' `labels = \(i) -i` to strip the negation introduced by
#' [`hsi_set_physical_extent()`], so that axes read as positive physical
#' distances. When `physical = FALSE`, ggplot2 default labels are used,
#' showing pixel coordinates.
#'
#' Layers are mapped to red, green, and blue channels in band order (1, 2, 3).
#' Any three-band combination can be used — RGB, CIR, SWIR false colour, or
#' any other composite.
#'
#' @seealso
#' [`hsi_set_physical_extent()`] to assign a physically calibrated extent
#' before plotting.
#' [`hsi_plot_spatraster()`] for single-layer plots.
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
#' # Physical-space plot after extent assignment
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#' x_physical <- hsi_set_physical_extent(x, y = x_coords, units = "mm")
#' x_rgb <- hsi_plot_spatraster_rgb(x_physical, physical = TRUE)
#'
#' # Add labels and theme with ggplot2
#' x_rgb +
#'   ggplot2::labs(x = "Width (mm)", y = "Depth (mm)") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @export
hsi_plot_spatraster_rgb <- function(
  x,
  physical = FALSE,
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

  # Create labelling function
  label_fun <- if (physical) \(i) -i else ggplot2::waiver()

  # Create ggplot and tidyterra object
  result <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(
      data = x,
      stretch = stretch
    ) +
    ggplot2::scale_y_continuous(labels = label_fun) +
    ggplot2::coord_fixed(expand = FALSE)

  # Return result
  result
}
