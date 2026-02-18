#' Create spatial calibration from image dimensions and physical distance
#'
#' @family HSI Calibration
#'
#' @param pixels Numeric. Total number of pixels along the measured axis
#'   (single positive value).
#' @param distance Numeric. Total physical distance corresponding to
#'   those pixels (single positive value).
#' @param units Character. Units of the provided resolution.
#'   One of "um" (micrometers, default), "mm", or "cm".
#'
#' @description
#' Create a spatial calibration from total pixel count and total physical
#' distance. Useful when scanner metadata provides the scan length and
#' you know the image dimensions.
#'
#' @details
#' The input SpatVector can be:
#' - A line drawn along the reference
#' - Two points placed at the reference endpoints
#'
#' CRS must be NULL because coordinates are in pixel space.
#'
#' @return A named numeric: resolution in µm/px, named "um_per_px".
#'
#' @examples
#' \dontrun{
#' # Scan was 150 mm, image has 2500 rows
#' # Get calibration
#' calibration <- hsi_calibration_from_dims(
#'   pixels = 2500,
#'   distance = 150,
#'   units = "mm"
#' )
#'
#' # From a SpatRaster directly
#' # Get SpatRaster
#' raster <- terra::rast("scan.tif")
#'
#' # Get calibration
#' cal <- hsi_calibration_from_dims(
#'   pixels = terra::nrow(raster),
#'   distance = 150,
#'   units = "mm"
#' )
#' }
#'
#' @export
hsi_calibration_from_dims <- function(
  pixels,
  distance = 1000,
  units = "um"
) {
  # Validate
  check_numeric(pixels, len = 1, positive = TRUE)
  check_numeric(distance, len = 1, positive = TRUE)
  check_one_of(units, choices = c("um", "mm", "cm"))

  # Get calibration
  # First convert distance to um
  calibration <- to_um(distance, from = units) |>
    # Find resolution
    (\(x) x / pixels)()

  # Set names
  names(calibration) <- "um_per_px"

  # Return
  calibration
}
