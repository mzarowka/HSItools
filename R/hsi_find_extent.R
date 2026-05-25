#' Find a fixed-width extent from reference points
#'
#' @family Utilities
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param points A [`SpatVector`][terra::SpatVector-class] with exactly 2 point
#'   geometries marking the vertical extent of the region of interest. Must be
#'   in the same coordinate space as `x`.
#' @param width Positive integer. Width of the output extent in pixels.
#' @param filename Character. Output filename. Default `""` skips writing.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param insert Logical. Insert layer into an existing file. Default `FALSE`.
#' @param layer Character. Layer name for vector output. Default `""`.
#' @param ... Additional arguments passed to [`terra::writeVector()`].
#'
#' @returns A [`SpatVector`][terra::SpatVector-class] polygon snapped to the
#'   grid of `x`.
#'
#' @details
#' The output polygon spans vertically between the two reference points and
#' horizontally by `width` pixels centered on the mean x-coordinate of the
#' points. The extent is snapped to cell boundaries of `x` with
#' [`terra::align()`] using `snap = "near"`.
#'
#' The function aborts if the requested extent falls outside the raster bounds.
#' Reduce `width` or adjust the reference points to fit within `x`.
#'
#' @seealso
#' [`hsi_calibrate_raster()`] to assign physical units to a raster extent.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' ends <- terra::vect("spatials.gpkg", layer = "ends")
#'
#' x_extent <- hsi_find_extent(x, points = ends, width = 900)
#' x_cropped <- terra::crop(x, x_extent)
#'
#' x_extent <- hsi_find_extent(
#'   x,
#'   points = ends,
#'   width = 900,
#'   filename = "spatials.gpkg",
#'   insert = TRUE,
#'   layer = "extent"
#' )
#' }
#'
#' @export
hsi_find_extent <- function(
  x,
  points,
  width,
  filename = "",
  overwrite = FALSE,
  insert = FALSE,
  layer = "",
  ...
) {
  # Validate inputs
  check_spatraster(x)
  check_spatvector(points)
  check_geom_type(points, allowed = "points")

  if (nrow(points) != 2) {
    cli::cli_abort(
      "{.arg points} must contain exactly 2 points, not {nrow(points)}."
    )
  }

  check_numeric(width, len = 1, positive = TRUE)

  # Extract coordinates from reference points
  coords <- terra::crds(points)

  # Compute center x and y span
  center_x <- mean(coords[, 1])
  y_min <- min(coords[, 2])
  y_max <- max(coords[, 2])

  # Compute x span in coordinate units
  half_width <- (width / 2) * terra::res(x)[1]

  # Build raw extent
  e <- terra::ext(
    center_x - half_width,
    center_x + half_width,
    y_min,
    y_max
  )

  # Snap to raster grid
  e <- terra::align(e, x, snap = "near")

  # Validate extent fits within raster
  x_ext <- terra::ext(x)

  if (
    e[1] < x_ext[1] || e[2] > x_ext[2] || e[3] < x_ext[3] || e[4] > x_ext[4]
  ) {
    cli::cli_abort(c(
      "Requested extent exceeds raster bounds.",
      "i" = "Width {.val {width}} px centered at column {.val {round(center_x, 1)}} does not fit within {.arg x}.",
      "i" = "Reduce {.arg width} or adjust {.arg points}."
    ))
  }

  # Convert to polygon
  result <- terra::as.polygons(e)

  # Write to file
  if (filename != "") {
    terra::writeVector(
      result,
      filename = filename,
      overwrite = overwrite,
      insert = insert,
      layer = layer,
      ...
    )
  }

  # Return result
  result
}
