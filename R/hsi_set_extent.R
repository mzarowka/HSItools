#' Set raster extent to physical units
#'
#' @family HSI Calibration
#'
#' @param raster A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param reference A [`SpatVector`][terra::SpatVector-class] with a single point
#'   marking the physical origin anchor on the vertical axis. Its y value is a
#'   spatial coordinate in the raster's own frame, increasing upward, as produced
#'   by digitising over the raster or by [`terra::xyFromCell()`]..
#'   It is converted internally to a fractional row position, so sub-pixel
#'   anchors are preserved and a point falling outside `raster` is extrapolated
#'   with a warning.
#' @param um_per_pixel Numeric. Physical size of one pixel in µm.
#' @param origin Numeric. Physical position assigned to `reference`, in µm
#'   regardless of `units`. Converted to `units` alongside `um_per_pixel`.
#'   Default `0`.
#' @param units Character. Output units for the calibrated raster extent.
#'   One of `"um"`, `"mm"`, or `"cm"`. Default `"cm"`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with a physically calibrated
#'   extent.
#'
#' @details
#' This keeps the current raster values unchanged, but updates the spatial
#' extent so that downstream plotting and profile extraction work in physical
#' units.
#'
#' The calibration is linear: the raster grid is converted from pixels to
#' physical units using the supplied micrometers-per-pixel ratio. The extent is
#' expressed in cell edges, so the calibrated raster has a resolution of exactly
#' `um_per_pixel` on both axes. The horizontal axis starts at zero, and the
#' vertical axis is anchored so that `reference` sits at `origin`.
#'
#' Importantly, at this stage, calibration metag does not carry forvard into
#' analysis products. Calibrate immediate products, where real world units are necessary.
#'
#' @seealso
#' [`hsi_calibration_from_scale()`], [`hsi_calibration_direct()`],
#' [`hsi_calibration_from_dims()`] for obtaining `um_per_pixel`.
#' [`hsi_pixels_to_units()`] for direct conversion of pixel positions.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Anchor on the vertical axis, e.g. the core top. The y value is a spatial
#' # coordinate increasing upward, so the centre of the top row sits at
#' # `terra::nrow(x) - 0.5`.
#' reference <- terra::vect(cbind(0.5, terra::nrow(x) - 0.5), type = "points")
#'
#' # 60 µm per pixel, supplied directly.
#' um <- hsi_calibration_direct(60)
#'
#' x_cal <- hsi_set_extent(x, reference, um_per_pixel = um, units = "cm")
#' }
#' @export
hsi_set_extent <- function(
  raster,
  reference,
  um_per_pixel,
  origin = 0,
  units = "cm"
) {
  # Validate inputs
  check_spatraster(raster)
  check_crs_null(raster)

  check_spatvector(reference)
  check_crs_null(reference)
  check_geom_type(reference, allowed = "points")

  if (terra::nrow(reference) != 1) {
    cli::cli_abort(
      "{.arg reference} must contain exactly one point.",
      class = "hsitools_error"
    )
  }

  check_numeric(um_per_pixel, len = 1, positive = TRUE)
  check_numeric(origin, len = 1)
  check_one_of(units, choices = c("um", "mm", "cm"))

  pixel_size <- from_um(um_per_pixel, to = units)

  # Both um_per_pixel and origin arrive in micrometres; work in output units.
  origin <- from_um(origin, to = units)

  nrow <- terra::nrow(raster)
  ncol <- terra::ncol(raster)

  # The reference point carries a spatial coordinate, where y increases upward,
  # but the extent below is built from row positions counted downward from the
  # top. Convert, keeping the fractional part and allow positions outside the raster.
  ref_y <- (terra::ymax(raster) - terra::geom(reference)[1, "y"]) /
    terra::yres(raster) +
    0.5

  # An anchor outside the raster is legitimate after a crop, but it is also what
  # a coordinate taken from the wrong frame looks like. Report the distance so
  # the two can be told apart.
  if (ref_y < 0.5 || ref_y > nrow + 0.5) {
    distance <- if (ref_y < 0.5) 0.5 - ref_y else ref_y - (nrow + 0.5)

    cli::cli_warn(
      c(
        "{.arg reference} falls outside {.arg raster}; extrapolating.",
        "i" = "Anchor is {signif(distance, 3)} row{?s} beyond the edge
               ({signif(distance * pixel_size, 3)} {units})."
      ),
      class = "hsitools_warning"
    )
  }

  # Extents are cell edges.
  # Grid reaches half a pixel beyond the first and last cell centres.
  x_min <- 0
  x_max <- ncol * pixel_size

  # Vertical positions anchored to the reference point.
  y_min <- -(origin + (nrow + 0.5 - ref_y) * pixel_size)
  y_max <- -(origin + (0.5 - ref_y) * pixel_size)

  # Create a copy of the input raster with the new extent.
  result <- terra::deepcopy(raster)
  terra::ext(result) <- terra::ext(x_min, x_max, y_min, y_max)

  # Add units metadata to the raster.
  tags <- terra::metags(result)
  if (is.data.frame(tags) && nrow(tags) > 0) {
    tags <- tags[tags$name != "hsi_units", , drop = FALSE]
  }
  tags <- rbind(
    tags,
    data.frame(
      name = "hsi_units",
      value = units,
      domain = ""
    )
  )

  # Set metags
  terra::metags(result) <- tags

  # Return result
  result
}
