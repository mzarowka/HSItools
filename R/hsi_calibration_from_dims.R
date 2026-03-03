#' Create spatial calibration from image dimensions and physical distance
#'
#' @family HSI Calibration
#'
#' @param pixels Numeric. Total number of pixels along the measured axis.
#'   Single positive value.
#' @param distance Numeric. Total physical distance corresponding to those
#'   pixels. Single positive value. Default `1000`.
#' @param units Character. Units of the provided distance. One of `"um"`
#'   (micrometers), `"mm"`, or `"cm"`. Default `"um"`.
#'
#' @returns A named numeric with resolution in µm/px, named `"um_per_px"`.
#'
#' @description
#' Create a spatial calibration from total pixel count and total physical
#' distance. Useful when scanner metadata provides the scan length and
#' the image dimensions are known.
#'
#' @examples
#' calibration <- hsi_calibration_from_dims(
#'   pixels = 2500,
#'   distance = 150,
#'   units = "mm")
#'
#' \dontrun{
#' raster <- terra::rast("scan.tif")
#'
#' calibration <- hsi_calibration_from_dims(
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
  # Validate inputs
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
