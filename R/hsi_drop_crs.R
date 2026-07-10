#' Drop CRS from a SpatRaster or SpatVector
#'
#' @family HSI Calibration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] or
#'   [`SpatVector`][terra::SpatVector-class].
#'
#' @returns `x` with CRS set to `""`.
#'
#' @details
#' GIS software (e.g. QGIS) often assigns a default CRS such as WGS84 to
#' data that is in pixel coordinate space. HSItools calibration and
#' co-registration functions require a `NULL` CRS to avoid misinterpretation
#' of pixel coordinates as geographic coordinates. Use this function to strip
#' an unwanted CRS before passing data to those functions.
#'
#' @examples
#' x <- terra::rast(nrows = 9, ncols = 9, nlyr = 3)
#' terra::crs(x) <- "EPSG:4326"
#'
#' x_stripped <- hsi_drop_crs(x)
#'
#' @export
hsi_drop_crs <- function(x) {
  # Validate inputs
  if (!inherits(x, c("SpatRaster", "SpatVector"))) {
    cli::cli_abort(
      "{.arg x} is a neither SpatRaster nor a SpatVector.",
      class = "hsitools_error"
    )
  }

  # Strip CRS
  terra::crs(x) <- ""

  # Return
  x
}
