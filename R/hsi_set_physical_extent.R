#' Assign a physically calibrated spatial extent to a SpatRaster
#'
#' @family HSI Calibration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param y A [`SpatRaster`][terra::SpatRaster-class] with physical coordinate
#'   layers `row_um` and `col_um`, as produced by [`hsi_calc_coords()`] or
#'   [`hsi_shift_coords()`]. Must have the same number of rows and columns as
#'   `x`. Default `NULL` returns `x` unchanged.
#' @param units Character. Physical units for the output extent. One of
#'   `"um"`, `"mm"`, or `"cm"`. Default `"mm"`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data
#'   and a physically calibrated spatial extent.
#'
#' @details
#' Replaces the spatial extent of a deep copy of `x` with a physically
#' calibrated extent derived from `y`. The result can be cropped in physical
#' units directly with [`terra::crop()`], or passed to
#' [`hsi_plot_spatraster()`] or [`hsi_plot_spatraster_rgb()`] without
#' supplying `y` — the extent is already embedded in the raster.
#'
#' Terra uses a y-up convention: `ymax` corresponds to row 1 and `ymin` to
#' the last row. Because depth increases downward in sediment core scans, the
#' row coordinate is negated when building the extent — `ymin` receives
#' `-max(row_um)` and `ymax` receives `-min(row_um)`. This satisfies terra's
#' requirement that `ymin < ymax` while placing shallow positions at the top
#' of any downstream plot. When `y` contains negative `row_um` values (i.e.
#' the origin was shifted with [`hsi_shift_coords()`]), the negation still
#' holds correctly — positions above the reference point appear as positive
#' `ymax` values.
#'
#' [`terra::deepcopy()`] is used to ensure the caller's raster is never
#' mutated, since terra's C++ backend can share object state across R names.
#'
#' @seealso
#' [`hsi_calc_coords()`] and [`hsi_shift_coords()`] to produce `y`.
#' [`hsi_plot_spatraster()`] and [`hsi_plot_spatraster_rgb()`] for
#' downstream plotting.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#'
#' x_physical <- hsi_set_physical_extent(x, y = x_coords)
#' x_physical <- hsi_set_physical_extent(
#'   x,
#'   y = x_coords,
#'   units = "mm",
#'   filename = "x_physical.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_set_physical_extent <- function(
  x,
  y = NULL,
  units = "mm",
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

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

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = names(x)
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get multiplier
  multiplier <- list(um = 1, mm = 0.001, cm = 0.0001) |>
    purrr::pluck(units)

  # Create a deep copy of x
  result <- terra::deepcopy(x)

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

    terra::ext(result) <- e
  }

  # Write to file if requested
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return result
  result
}
