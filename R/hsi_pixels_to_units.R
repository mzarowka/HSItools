#' Convert pixel coordinates to physical units
#'
#' @family HSI Calibration
#'
#' @param pixels Numeric. Pixel coordinates to convert.
#' @param calibration Numeric. Spatial calibration in µm/px, as created
#'   by any \code{hsi_calibration_*} function.
#' @param origin Numeric. Pixel coordinate that corresponds to position
#'   zero (default 0).
#' @param direction Numeric. Either 1 or -1. Controls the sign of the
#'   output (default 1). Set to -1 to flip direction, which is typically
#'   needed for vertical profiles in terra where y-coordinates decrease
#'   downward but physical positions should increase.
#' @param units Character. Output units. One of "um" (micrometers, default),
#'   "mm", or "cm".
#'
#' @description
#' Convert pixel coordinates to physical positions using a spatial calibration.
#' Optionally set an origin pixel (specimen start) so that output positions
#' are specimen-relative.
#'
#' @details
#' The conversion formula is:
#'
#' \code{position = (pixels - origin) * calibration * direction}
#'
#' The result is in µm and then converted to the requested output units.
#'
#' In terra, y-coordinates and row indices run in opposite directions:
#' y-coordinates decrease downward while row indices increase downward.
#' The appropriate \code{direction} depends on what you pass as \code{pixels}:
#' \itemize{
#'   \item{y-coordinates (e.g., from \code{hsi_extract_profile()}): use
#'     \code{direction = -1} for positions increasing along the specimen}
#'   \item{Row indices: use \code{direction = 1} (default)}
#' }
#'
#' @return Numeric vector. Physical positions in the requested units.
#'   Same length as input \code{pixels}.
#'
#' @seealso
#' \code{\link{hsi_calibration_direct}},
#' \code{\link{hsi_calibration_from_scale}},
#' \code{\link{hsi_calibration_from_dims}} for creating calibration objects
#'
#' @examples
#' # Create calibration: 60 µm per pixel
#' calibration <- hsi_calibration_direct(60)
#'
#' # Row indices are increasing downward, direction = 1 (default)
#' hsi_pixels_to_units(c(0, 10, 20), calibration)
#'
#' # Row indices with origin and specimen starts at row 50
#' hsi_pixels_to_units(c(50, 60, 70), calibration, origin = 50)
#'
#' # Y-coordinates from terra and decreasing downward, use direction = -1
#' hsi_pixels_to_units(c(850, 840, 830), calibration, origin = 850, direction = -1)
#'
#' # Output in mm
#' hsi_pixels_to_units(c(0, 10, 20), calibration, units = "mm")
#'
#' @export
hsi_pixels_to_units <- function(
  pixels,
  calibration,
  origin = 0,
  direction = 1,
  units = "um"
) {
  # Validate numerics
  check_numeric(pixels)
  check_numeric(calibration, len = 1, positive = TRUE)
  check_numeric(origin, len = 1)

  # Validate units
  check_one_of(units, choices = c("um", "mm", "cm"))

  # Validate direction
  if (!direction %in% c(1, -1)) {
    cli::cli_abort(
      "{.arg direction} must be {.val {1}} or {.val {-1}}.",
      class = "hsitools_error"
    )
  }

  # Conver pixel distance from origin × resolution × direction
  position_um <- (pixels - origin) * as.numeric(calibration) * direction

  # Convert to target units
  from_um(position_um, to = units)
}
