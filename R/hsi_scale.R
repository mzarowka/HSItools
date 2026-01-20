#' Extract pixel-to-unit ratio from scale reference
#'
#' @family HSI Calibration
#'
#' @param x A terra SpatVector representing a digitized scale reference.
#'   Must be either a line geometry or exactly 2 points.
#' @param distance Numeric. Physical distance the scale represents (positive). Default 10000 (um).
#' @param units Character. Physical units of measurement. One of: "um"
#'   (micrometers), "mm" (millimeters), "cm" (centimeters). Default "um".
#'
#' @description
#' Calculate the ratio between pixels and physical units from a known reference
#' measurement. This is the first step in spatial calibration, establishing
#' how many physical units correspond to one pixel.
#'
#' @details
#' The function accepts either:
#' - A line digitized along a measuring tape/scale bar
#' - Two points placed at scale endpoints
#'
#' @return A list containing:
#'   \item{ratio}{Numeric. Physical units per pixel}
#'   \item{units}{Character. Unit of measurement}
#'   \item{pixel_distance}{Numeric. Measured distance in pixels}
#'   \item{physical_distance}{Numeric. User-provided physical distance}
#'   \item{geometry}{SpatVector. Original input geometry for provenance}
#'
#' @seealso
#' \code{\link{hsi_ends}} for defining sample boundaries,
#' \code{\link{hsi_spatial_calibration}} for combining scale and ends
#'
#' @examples
#' \dontrun{
#' # From a digitized line along 10 mm scale bar
#' scale_line <- terra::vect("scale.gpkg", layer = "scale")
#' scale_info <- hsi_scale(scale_line, distance = 10, units = "mm")
#'
#' # From two points at scale endpoints
#' scale_pts <- terra::vect("scale.gpkg", layer = "scale_points")
#' scale_info <- hsi_scale(scale_pts, distance = 10000, units = "um")
#'
#' # Access ratio for calculations
#' scale_info$ratio
#' }
#'
#' @export
hsi_scale <- function(
  x,
  distance = 10000,
  units = "um"
) {
  # Validate input
  check_spatvector(x)

  # Check the CRS
  check_crs_null(x)

  # Validate geometry type
  check_geom_type(x, allowed = c("lines", "points"))

  # Validate point count
  if (terra::geomtype(x) == "points" && nrow(x) != 2) {
    cli::cli_abort(
      "{.arg x} must contain exactly 2 points, not {nrow(x)}."
    )
  }

  # Validate distance
  check_numeric(distance, len = 1, positive = TRUE)

  # Validate units
  check_one_of(units, choices = c("um", "mm", "cm"))

  # Calculate pixel distance
  if (terra::geomtype(x) == "lines") {
    pixel_distance <- terra::perim(x)
  } else {
    pixel_distance <- terra::distance(x)[1, 2]
  }

  # Calculate ratio
  ratio <- distance / pixel_distance

  # Return
  list(
    ratio = ratio,
    units = units,
    pixel_distance = pixel_distance,
    physical_distance = distance,
    geometry = x
  )
}
