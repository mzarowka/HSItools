#' Plot a single-layer SpatRaster
#'
#' @family Plotting
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Must be single-layer.
#' @param physical Logical. When `TRUE`, negates y-axis tick labels to display
#'   positive physical distances. Set `TRUE` when `x` has been processed by
#'   [`hsi_set_physical_extent()`]. Default `FALSE`.
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal raster map with a fixed aspect ratio and no axis
#' expansion. The returned ggplot carries no theme, colour scale, or axis
#' labels — add these with `+` using standard ggplot2 conventions.
#'
#' When `physical = TRUE`, [`ggplot2::scale_y_continuous()`] applies
#' `labels = \(i) -i` to strip the negation introduced by
#' [`hsi_set_physical_extent()`], so that axes read as positive physical
#' distances. When `physical = FALSE`, ggplot2 default labels are used,
#' showing pixel coordinates.
#'
#' @seealso
#' [`hsi_set_physical_extent()`] to assign a physically calibrated extent
#' before plotting.
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
#' # Physical-space plot after extent assignment
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#' x_physical <- hsi_set_physical_extent(x, y = x_coords, units = "mm")
#' x_spatraster <- hsi_plot_spatraster(x_physical, physical = TRUE)
#'
#' # Add labels and theme with ggplot2
#' x_spatraster +
#'   ggplot2::labs(x = "Width (mm)", y = "Depth (mm)", fill = "RABD") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @export
hsi_plot_spatraster <- function(
  x,
  physical = FALSE
) {
  # Validate inputs
  check_spatraster(x)

  if (terra::nlyr(x) > 1) {
    cli::cli_abort(
      "{.arg x} has {.val {terra::nlyr(x)}} layers. Only one is accepted."
    )
  }

  check_crs_null(x)

  # Create labelling function
  label_fun <- if (physical) \(i) -i else ggplot2::waiver()

  # Create ggplot and tidyterra object
  result <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = x) +
    ggplot2::scale_y_continuous(labels = label_fun) +
    ggplot2::coord_fixed(expand = FALSE)

  # Return result
  result
}
