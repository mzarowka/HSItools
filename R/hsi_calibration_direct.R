#' Create spatial calibration from known resolution
#'
#' @family HSI Calibration
#'
#' @param resolution Numeric. Pixel size (single positive value).
#' @param units Character. Units of the provided resolution.
#'   One of "um" (micrometers, default), "mm", or "cm".
#'
#' @description
#' Create a spatial calibration when you know the pixel size directly.
#' This is the simplest calibration path. The value is stored internally
#' in micrometers per pixel.
#'
#' @return A named numeric: resolution in µm/px, named "um_per_px".
#'
#' @examples
#' # With 60 µm per pixel
#' calibration <- hsi_calibration_direct(60)
#'
#' # With 0.06 mm per pixel (so the same as above, but unit is different)
#' calibration <- hsi_calibration_direct(0.06, units = "mm")
#'
#' @export
hsi_calibration_direct <- function(
  resolution,
  units = "um"
) {
  # Validate
  check_numeric(resolution, len = 1, positive = TRUE)

  # Validate units
  check_one_of(units, choices = c("um", "mm", "cm"))

  # Get calibration
  calibration <- to_um(resolution, from = units)

  # Set names
  names(calibration) <- "um_per_px"

  # Return
  calibration
}
