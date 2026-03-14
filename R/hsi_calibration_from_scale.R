#' Create spatial calibration from a digitized scale reference
#'
#' @family HSI Calibration
#'
#' @param x A [`SpatVector`][terra::SpatVector-class] representing a digitized
#'   scale reference. Must be either a line geometry or exactly 2 points.
#'   CRS must be `NULL` (pixel coordinates).
#' @param distance Numeric. Physical distance the reference represents.
#'   Single positive value. Default `10000`.
#' @param units Character. Units of the provided distance. One of `"um"`
#'   (micrometers), `"mm"`, or `"cm"`. Default `"um"`.
#'
#' @returns A named numeric with resolution in µm/px, named `"um_per_px"`.
#'
#' @description
#' Create a spatial calibration by measuring a known-length reference
#' digitized in GIS software. The function calculates pixel distance from
#' the geometry and derives resolution from the provided physical distance.
#'
#' @details
#' The input [`SpatVector`][terra::SpatVector-class] can be a line drawn along
#' the reference, or two points placed at the reference endpoints. CRS must be
#' `NULL` because coordinates are in pixel space.
#'
#' @examples
#' \dontrun{
#' scale_line <- terra::vect("scale.gpkg", layer = "scale")
#'
#' calibration <- hsi_calibration_from_scale(
#'   scale_line,
#'   distance = 10,
#'   units = "mm"
#' )
#'
#' scale_points <- terra::vect("points.gpkg", layer = "scale_points")
#'
#' calibration <- hsi_calibration_from_scale(
#'   scale_points,
#'   distance = 10000,
#'   units = "um"
#' )
#' }
#'
#' @export
hsi_calibration_from_scale <- function(
  x,
  distance = 10000,
  units = "um"
) {
  # Validate inputs
  check_spatvector(x)

  # Validate empty CRS
  check_crs_null(x)

  # Validate geometry
  check_geom_type(x, allowed = c("lines", "points"))

  # Validate number of line features
  if (terra::geomtype(x) == "lines" && nrow(x) > 1) {
    cli::cli_warn(c(
      "{.arg x} contains {nrow(x)} line features. Only the first will be used.",
      "i" = "Filter {.arg x} to a single feature before calling this function."
    ))

    # Subset line feature
    x <- x[1]
  }

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
