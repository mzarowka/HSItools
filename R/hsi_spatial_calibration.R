#' Combine scale and ends into spatial calibration object
#'
#' @family HSI Calibration
#'
#' @param scale List. Output from \code{\link{hsi_scale}} containing
#'   pixel-to-unit ratio information.
#' @param ends List. Output from \code{\link{hsi_ends}} containing
#'   sample boundary positions.
#' @param direction Character. Axis for position calculations. Either
#'   "vertical" (uses y coordinates, default) or "horizontal" (uses x coordinates).
#'
#' @description
#' Combine scale ratio and sample boundary information into a single calibration
#' object. This object contains everything needed to convert between pixel
#' coordinates and physical positions.
#'
#' @details
#' The calibration object bundles:
#' - The pixel-to-unit ratio from scale measurement
#' - Reference points (start/end) with known physical positions
#' - Direction to determine which pixel coordinate to use
#' - Pre-computed spans for convenience
#'
#' The resulting object is passed to \code{\link{hsi_pixels_to_units}} for
#' coordinate transformations.
#'
#' @return A list containing:
#'   \item{ratio}{Numeric. Physical units per pixel (absolute value)}
#'   \item{signed_ratio}{Numeric. Physical units per pixel with direction sign}
#'   \item{direction_sign}{Numeric. +1 or -1 indicating pixel-to-physical relationship}
#'   \item{units}{Character. Unit of measurement}
#'   \item{direction}{Character. "vertical" or "horizontal"}
#'   \item{start}{List with pixel, physical_position, sample_position, splice_position}
#'   \item{end}{List with pixel, physical_position, sample_position, splice_position}
#'   \item{pixel_span}{Numeric. Absolute pixel distance between start and end}
#'   \item{physical_span}{Numeric. Absolute physical distance between start and end}
#'   \item{scale_geometry}{SpatVector. Original scale geometry for provenance}
#'   \item{ends_geometry}{SpatVector. Original ends geometry for provenance}
#'
#' @seealso
#' \code{\link{hsi_scale}} for extracting pixel-to-unit ratio,
#' \code{\link{hsi_ends}} for defining sample boundaries,
#' \code{\link{hsi_pixels_to_units}} for applying the calibration
#'
#' @examples
#' \dontrun{
#' # Load geometries from geopackage
#' scale_line <- terra::vect("calibration.gpkg", layer = "scale")
#' ends_pts <- terra::vect("calibration.gpkg", layer = "ends")
#'
#' # Build calibration
#' scale_info <- hsi_scale(scale_line, distance = 10, units = "mm")
#' ends_info <- hsi_ends(ends_pts)
#' calibration <- hsi_spatial_calibration(scale_info, ends_info)
#'
#' # Use for transformation
#' physical_positions <- hsi_pixels_to_units(pixel_values, calibration)
#' }
#'
#' @export
hsi_spatial_calibration <- function(
  scale,
  ends,
  direction = "vertical"
) {
  # Validate scale structure
  check_list_has(
    scale,
    elements = c(
      "ratio",
      "units",
      "pixel_distance",
      "physical_distance",
      "geometry"
    )
  )

  # Validate ends structure
  check_list_has(
    ends,
    elements = c("start", "end", "geometry")
  )

  # Validate ends$start structure
  check_list_has(
    ends$start,
    elements = c("pixel_x", "pixel_y", "physical_position")
  )

  # Validate ends$end structure
  check_list_has(
    ends$end,
    elements = c("pixel_x", "pixel_y", "physical_position")
  )

  # Validate direction
  check_one_of(direction, choices = c("vertical", "horizontal"))

  # Select pixel coordinate based on direction
  pixel_coord <- if (direction == "vertical") "pixel_y" else "pixel_x"

  start_pixel <- ends$start[[pixel_coord]]
  end_pixel <- ends$end[[pixel_coord]]

  # Calculate direction sign for transformation
  # Difference in pixels
  pixel_delta <- end_pixel - start_pixel

  # Difference in physical units
  physical_delta <- ends$end$physical_position - ends$start$physical_position

  # Sign (direction)
  direction_sign <- sign(physical_delta) * sign(pixel_delta)

  # Pre-compute signed ratio
  signed_ratio <- scale$ratio * direction_sign

  # Return calibration object
  list(
    ratio = scale$ratio,
    signed_ratio = signed_ratio,
    direction_sign = direction_sign,
    units = scale$units,
    direction = direction,
    start = list(
      pixel = start_pixel,
      physical_position = ends$start$physical_position,
      sample_position = ends$start$sample_position,
      splice_position = ends$start$splice_position
    ),
    end = list(
      pixel = end_pixel,
      physical_position = ends$end$physical_position,
      sample_position = ends$end$sample_position,
      splice_position = ends$end$splice_position
    ),
    pixel_span = abs(end_pixel - start_pixel),
    physical_span = abs(
      ends$end$physical_position - ends$start$physical_position
    ),
    scale_geometry = scale$geometry,
    ends_geometry = ends$geometry
  )
}
