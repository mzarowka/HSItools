#' Create spatial calibration from a digitized scale reference
#'
#' @family HSI Calibration
#'
#' @param x A terra SpatVector representing a digitized scale reference.
#'   Must be either a line geometry or exactly 2 points. CRS must be NULL
#'   (pixel coordinates).
#' @param distance Numeric. Physical distance the reference represents
#'   (single positive value). Default is 1000 (µm) for 1 cm.
#' @param units Character. Units of the provided resolution.
#'   One of "um" (micrometers, default), "mm", or "cm".
#'
#' @description
#' Create a spatial calibration by measuring a known-length reference
#' (e.g., scale bar, measuring tape) digitized in GIS software.
#' The function calculates pixel distance from the geometry and derives
#' resolution from the provided physical distance.
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
#' # From a digitized line along a 10 mm scale bar
#' # Get the scale
#' scale_line <- terra::vect("scale.gpkg", layer = "scale")
#'
#' # Get the calibration
#' calibration <- hsi_calibration_from_scale(
#'   scale_line,
#'   distance = 10,
#'   units = "mm")
#' 
#' # From two points at scale bar endpoints
#' # Get the scale
#' scale_points <- terra::vect("points.gpkg", layer = "scale_points")
#' 
#' # Get the calibration
#' calibration <- hsi_calibration_from_scale(
#'   scale_pts,
#'   distance = 10000,
#'   units = "um")
#' }
#' 
#' @export
hsi_calibration_from_scale <- function(
  x,
  distance = 10000,
  units = "um"
) {
  # Validate
  check_spatvector(x)
  check_crs_null(x)
  check_geom_type(x, allowed = c("lines", "points"))

  # Validate number of points
  if (terra::geomtype(x) == "points" && nrow(x) != 2) {
    cli::cli_abort(
      "{.arg x} must contain exactly 2 points (start and finish), not {nrow(x)}."
    )
  }

  # Validate distance
  check_numeric(distance, len = 1, positive = TRUE)

  # Validate units
  check_one_of(units, choices = c("um", "mm", "cm"))

  # Calculate distance in pixels from SpatVector
  pixel_distance <- if (terra::geomtype(x) == "lines") {
    # Calculate 1D perimeter
    terra::perim(x)
    # Else calculate from endpoints
  } else {
    terra::distance(x)[1, 2]
  }

  # Get calibration
  # First convert distance to um
  calibration <- to_um(distance, from = units) |>
    # Find resolution
    (\(x) x / pixel_distance)()

  # Set names
  names(calibration) <- "um_per_px"

  # Return
  calibration
}
