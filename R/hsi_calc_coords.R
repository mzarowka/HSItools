#' Calculate physical coordinate rasters from pixel indices
#'
#' @family HSI Calibration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param um_per_pixel Numeric. Physical size of one pixel in µm, as returned
#'   by `hsi_calibration_direct()`, `hsi_calibration_from_dims()`, or
#'   `hsi_calibration_from_scale()`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with two layers: `row_um`
#'   (physical row position in µm) and `col_um` (physical column position in µm).
#'
#' @details
#' Pixel positions are expressed as pixel centres: the first pixel holds
#' `0.5 * um_per_pixel`, not zero. This is consistent with how
#' [`terra::extract()`] addresses pixels and ensures alignment with physical
#' measurements taken at the pixel footprint centre.
#'
#' Output is always in µm. Convert to mm by dividing by `1000`, to cm by
#' dividing by `10000`.
#'
#' @seealso
#' [`hsi_calibration_direct()`], [`hsi_calibration_from_dims()`],
#' [`hsi_calibration_from_scale()`] to obtain `um_per_pixel`.
#' [`hsi_shift_coords()`] to anchor the coordinate raster to a known
#' reference position.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' um <- hsi_calibration_from_dims(pixels = 50000, distance = 10000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#' x_coords <- hsi_calc_coords(
#'   x,
#'   um_per_pixel = um,
#'   filename = "coords.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_coords <- function(
  x,
  um_per_pixel,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate no CRS
  check_crs_null(x)

  # Validate real world units (y)
  check_numeric(um_per_pixel, len = 1, positive = TRUE)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = c("row_um", "col_um")
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Calculate row_um SpatRaster
  row_um <- (terra::init(x, fun = "row") - 0.5) * um_per_pixel

  # Calculate col_um SpatRaster
  col_um <- (terra::init(x, fun = "col") - 0.5) * um_per_pixel

  result <- c(row_um, col_um)

  names(result) <- c("row_um", "col_um")

  # Write to file if requested
  if (filename != "") {
    result <- terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  result
}
