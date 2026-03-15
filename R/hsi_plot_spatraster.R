#' Plot a single-layer SpatRaster with physical spatial coordinates
#'
#' @family Plotting
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Must be single-layer.
#' @param y A [`SpatRaster`][terra::SpatRaster-class] with physical coordinate
#'   layers `row_um` and `col_um`, as produced by [`hsi_calc_coords()`] or
#'   [`hsi_shift_coords()`]. Must have the same number of rows and columns as
#'   `x`. Default `NULL` plots in pixel coordinates.
#' @param units Character. Display units for axis labels. One of `"um"`,
#'   `"mm"`, or `"cm"`. Default `"mm"`.
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' When `y` is supplied, the function replaces the spatial extent of a deep
#' copy of `x` with a physically calibrated extent derived from `y`. This
#' means axis tick positions are genuine physical coordinates, not pixel
#' indices relabelled after the fact. [`ggplot2::coord_fixed()`] is applied
#' so that one unit on the x-axis equals one unit on the y-axis, preserving
#' the true aspect ratio of the scan.
#'
#' Terra uses a y-up convention: `ymax` corresponds to row 1 and `ymin` to
#' the last row. Because depth increases downward in sediment core scans, the
#' row coordinate is negated when building the extent — `ymin` receives
#' `-max(row_um)` and `ymax` receives `-min(row_um)`. This satisfies terra's
#' requirement that `ymin < ymax` while placing shallow positions at the top
#' of the plot. [`ggplot2::scale_y_continuous()`] with `labels = \(i) -i`
#' then strips the negation from the displayed tick labels so that axes read
#' as positive physical distances.
#'
#' [`terra::deepcopy()`] is used to ensure the caller's raster is never
#' mutated, since terra's C++ backend can share object state across R names.
#'
#' The returned ggplot carries no theme, colour scale, or axis labels — add
#' these with `+` using standard ggplot2 conventions.
#'
#' @seealso
#' [`hsi_calc_coords()`] and [`hsi_shift_coords()`] to produce `y`.
#' [`hsi_plot_profile()`] for 1-D depth profiles.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("RABD_testdata.tif") |> terra::subset(1)
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#'
#' # Pixel-space plot
#' x_spatraster <- hsi_plot_spatraster(x)
#'
#' # Physical-space plot in mm
#' x_spatraster <- hsi_plot_spatraster(x, y = x_coords)
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
  y = NULL,
  units = "mm"
) {
  # Validate inputs
  check_spatraster(x)

  if (terra::nlyr(x) > 1) {
    cli::cli_abort(
      "{.arg x} has {.val {terra::nlyr(x)}} layers. Only one is accepted."
    )
  }

  check_crs_null(x)

  if (!is.null(y)) {
    check_spatraster(y)

    check_crs_null(y)

    check_list_has(
      terra::as.list(y) |> stats::setNames(terra::names(y)),
      elements = c("row_um", "col_um")
    )

    if (terra::nrow(x) != terra::nrow(y) || terra::ncol(x) != terra::ncol(y)) {
      cli::cli_abort(c(
        "{.arg x} has different dimensions than {.arg y}.",
        "i" = "{.arg x} extent has {.val {terra::nrow(x)}} rows and {.val {terra::ncol(x)}} cols, while {.arg y} extent has {.val {terra::nrow(y)}} rows and {.val {terra::ncol(y)}} cols."
      ))
    }
  }

  check_one_of(units, choices = c("um", "mm", "cm"))

  # Get multiplier
  multiplier <- list(um = 1, mm = 0.001, cm = 0.0001) |>
    purrr::pluck(units)

  # Create a deep copy of x
  x_deep <- terra::deepcopy(x)

  # Conditional extent translation
  if (!is.null(y)) {
    # Create a new extent from y SpatRaster
    # Real world units grow the opposite way to terra cells
    e <- terra::ext(
      terra::minmax(y)["min", "col_um"] * multiplier,
      terra::minmax(y)["max", "col_um"] * multiplier,
      -terra::minmax(y)["max", "row_um"] * multiplier,
      -terra::minmax(y)["min", "row_um"] * multiplier
    )

    terra::ext(x_deep) <- e
  }

  # Create ggplot and tidyterra object
  result <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = x_deep) +
    ggplot2::scale_y_continuous(labels = \(i) -i) +
    ggplot2::coord_fixed(expand = FALSE)

  # Return result
  result
}
