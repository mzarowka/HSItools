#' Shift physical coordinate raster to a known reference position
#'
#' @family HSI Calibration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with two layers
#'   `row_um` and `col_um`, as produced by [`hsi_calc_coords()`].
#' @param reference A [`SpatVector`][terra::SpatVector-class] with a single
#'   point geometry in the same pixel coordinate space as `x`.
#' @param origin Numeric. Known physical position of the reference point
#'   in µm. Default `0` places the reference point at zero, with positions
#'   above returning negative values.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with two layers: `row_um`
#'   (shifted physical row position in µm) and `col_um` (unchanged physical
#'   column position in µm).
#'
#' @details
#' The shift is computed by linearly estimating the `row_um` value at the
#' `reference` point from the spatial resolution of the coordinate raster.
#' This works even when the reference point falls outside the raster extent
#' (e.g. a digitised point fractionally beyond the edge), because the
#' relationship between pixel position and physical position is linear.
#'
#' The physical resolution (µm per pixel row) is derived from the first two
#' row centres of `x`. The distance in pixels between cell 1 and `reference`
#' is then multiplied by this resolution to estimate the `row_um` value at
#' `reference`. The resulting offset (`estimated - origin`) is subtracted
#' from the entire `row_um` layer.
#'
#' Pixels above the reference point will hold negative values when
#' `origin = 0`.
#'
#' `reference` must share the same pixel coordinate space as `x` — no CRS is
#' expected on either input. The point is typically digitised from the
#' coordinate raster directly, for example in QGIS or via [`terra::click()`].
#'
#' @seealso
#' [`hsi_calc_coords()`] to produce the input coordinate raster.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#'
#' ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
#'
#' x_coords_shifted <- hsi_shift_coords(x_coords, reference = ref)
#' x_coords_shifted <- hsi_shift_coords(
#'   x_coords,
#'   reference = ref,
#'   origin = 300000,
#'   filename = "coords_shifted.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_shift_coords <- function(
  x,
  reference,
  origin = 0,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate no CRS
  check_crs_null(x)

  # Validate SpatRaster layers
  check_list_has(
    terra::as.list(x) |> stats::setNames(terra::names(x)),
    elements = c("row_um", "col_um")
  )

  # Validate input
  check_spatvector(reference)

  # Validate no CRS
  check_crs_null(reference)

  # Check for point
  check_geom_type(reference, allowed = "points")

  # Validate input
  check_numeric(origin, len = 1)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = c("row_um", "col_um")
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get cell centres for first two rows
  cell1 <- terra::xyFromCell(x, 1)
  cell2 <- terra::xyFromCell(x, terra::ncol(x) + 1)

  # Compute physical resolution in row_um units per pixel row
  um_per_pixel <- terra::extract(x$row_um, cell2)[[2]] -
    terra::extract(x$row_um, cell1)[[2]]

  # Compute distance from cell 1 to reference point in pixel rows
  point_y <- terra::geom(reference)[, "y"]
  dist_pixels <- (cell1[2] - point_y) / terra::res(x)[2]

  # Estimate row_um value at reference point by linear extrapolation
  estimated <- terra::extract(x$row_um, cell1)[[2]] +
    dist_pixels * um_per_pixel

  # Find shift value from estimated position and known origin
  shift <- estimated - origin

  # Calculate shifted row_um SpatRaster
  x$row_um <- x$row_um - shift

  names(x) <- c("row_um", "col_um")

  # Write to file if requested
  if (filename != "") {
    terra::writeRaster(
      x,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  x
}
