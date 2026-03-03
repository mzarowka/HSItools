#' Create spatial calibration from known resolution
#'
#' @family HSI Calibration
#'
#' @param resolution Numeric. Pixel size in the specified units. Single positive value.
#' @param units Character. Units of the provided resolution. One of `"um"`
#'   (micrometers), `"mm"`, or `"cm"`. Default `"um"`.
#'
#' @returns A named numeric with resolution in µm/px, named `"um_per_px"`.
#'
#' @description
#' Create a spatial calibration when the pixel size is known directly.
#' This is the simplest calibration path. The value is stored internally
#' in micrometers per pixel.
#'
#' @examples
#' calibration <- hsi_calibration_direct(60)
#'
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
