#' Convert pixel coordinates to physical positions
#'
#' @family HSI Calibration
#'
#' @param pixels Numeric. Pixel coordinates to transform.
#' @param calibration List. Output from \code{\link{hsi_spatial_calibration}}.
#' @param target Character. Target position system. One of:
#'   \describe{
#'     \item{"physical"}{Scale/tape reading coordinates (default)}
#'     \item{"sample"}{Sample-relative coordinates (e.g., 0 at sample top)}
#'     \item{"splice"}{Composite/spliced sequence coordinates}
#'   }
#'
#' @description
#' Transform pixel coordinates to physical positions using a spatial calibration
#' object. Supports multiple position systems depending on how ends were defined.
#'
#' @details
#' The transformation uses the formula:
#'
#' \code{position = start_position + (pixel - start_pixel) * signed_ratio}
#'
#' The signed ratio accounts for the relationship between pixel coordinate
#' direction (Y increases downward in raster space) and physical position
#' direction.
#'
#' Extrapolation beyond the start/end range is allowed.
#'
#' @return Numeric vector of positions in the target coordinate system,
#'   same length as input `pixels`.
#'
#' @seealso
#' \code{\link{hsi_spatial_calibration}} for creating calibration object
#'
#' @examples
#' \dontrun{
#' # Build calibration
#' scale_info <- hsi_scale(scale_line, distance = 10, units = "mm")
#' ends_info <- hsi_ends(ends_pts)
#' calibration <- hsi_spatial_calibration(scale_info, ends_info)
#'
#' # Convert pixel coordinates
#' pixel_values <- c(100, 200, 300, 400, 500)
#' physical_pos <- hsi_pixels_to_units(pixel_values, calibration)
#'
#' # Use sample-relative coordinates
#' sample_pos <- hsi_pixels_to_units(pixel_values, calibration, target = "sample")
#' }
#'
#' @export
hsi_pixels_to_units <- function(
  pixels,
  calibration,
  target = "physical"
) {
  # Validate pixels
  check_numeric(pixels)

  # Validate calibration structure
  check_list_has(
    calibration,
    elements = c("signed_ratio", "start", "end")
  )

  # Validate start has required elements
  check_list_has(
    calibration$start,
    elements = c("pixel", "physical_position")
  )

  # Validate target
  check_one_of(target, choices = c("physical", "sample", "splice"))

  # Get reference position based on target
  reference_position <- switch(
    target,
    physical = calibration$start$physical_position,
    sample = calibration$start$sample_position,
    splice = calibration$start$splice_position
  )

  # Check that target position exists
  if (is.null(reference_position)) {
    cli::cli_abort(
      c(
        "Target {.val {target}} position not available in calibration.",
        "i" = "Ensure {.val {target}_position} was provided in ends."
      )
    )
  }

  # Transform
  position <- reference_position +
    (pixels - calibration$start$pixel) * calibration$signed_ratio

  # Return numeric vector
  position
}
