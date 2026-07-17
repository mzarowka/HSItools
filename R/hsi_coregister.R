#' Co-register source raster to target raster grid
#'
#' @family HSI Co-registration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] to warp. Must have a
#'   file source on disk.
#' @param y A [`SpatRaster`][terra::SpatRaster-class] defining the output grid.
#'   Output extent, resolution, and dimensions are taken from this raster.
#' @param gcp A [data.frame] or [tibble][tibble::tibble] of matched GCPs from
#'   [`hsi_match_gcp()`]. Must contain columns `source_x`, `source_y`,
#'   `target_x`, `target_y`.
#' @param method Character. Resampling method. One of `"near"`, `"bilinear"`,
#'   `"cubic"`, `"cubicspline"`, `"lanczos"`. Default `"bilinear"`.
#' @param filename Character. Output filename. Default `""` keeps result in
#'   memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] aligned to the `y` grid.
#'
#' @description
#' Warp a source raster onto the target grid using matched ground control
#' points via GDAL. Areas without source coverage are `NA`.
#'
#' @details
#' Uses [`sf::gdal_utils()`] internally. The source GCPs are embedded in a
#' lightweight VRT (no data duplication), then GDAL warps onto the target grid
#' with a first-order polynomial (affine) transformation. Band names from `x`
#' are preserved in the output file.
#'
#' Requires the \pkg{sf} package.
#'
#' @seealso
#' [`hsi_match_gcp()`] for matching GCPs,
#' [`hsi_check_gcp()`] for checking residuals first.
#'
#' @examples
#' \dontrun{
#' matched <- hsi_match_gcp(swir_gcps, vnir_gcps)
#'
#' x_coregistered <- hsi_coregister(
#'   x = swir,
#'   y = vnir,
#'   gcp = matched,
#'   filename = "SWIR_coregistered.tif"
#' )
#' }
#'
#' @export
hsi_coregister <- function(
  x,
  y,
  gcp,
  method = "bilinear",
  filename = "",
  overwrite = FALSE
) {
  # Validate inputs
  check_spatraster(x)

  check_spatraster(y)

  check_data_frame(gcp)

  # Validate columns
  check_has_cols(gcp, cols = c("source_x", "source_y", "target_x", "target_y"))

  # Check minimum of 3 GCPs
  if (nrow(gcp) < 3) {
    cli::cli_abort(
      "Need at least 3 matched GCPs, found {nrow(gcp)}.",
      class = "hsitools_error"
    )
  }

  # Check for collinear GCPs
  if (qr(cbind(1, gcp$source_x, gcp$source_y))$rank < 3) {
    cli::cli_abort(
      "GCPs are collinear and affine transform cannot be fit.",
      class = "hsitools_error"
    )
  }

  # Check method
  check_one_of(
    method,
    choices = c("near", "bilinear", "cubic", "cubicspline", "lanczos")
  )

  # Check dependency on sf package
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg sf} is required for co-registration.",
        "i" = "Install with: {.code install.packages('sf')}"
      ),
      class = "hsitools_error"
    )
  }

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Drop CRS from both rasters - work in pixel space
  x <- hsi_drop_crs(x)
  y <- hsi_drop_crs(y)

  # Source must be on disk for GDAL to work
  source_path <- terra::sources(x)[1]

  if (is.na(source_path) || source_path == "") {
    cli::cli_abort(
      c(
        "{.arg x} has no file source.",
        "i" = "Write it to disk first with {.code terra::writeRaster()}."
      ),
      class = "hsitools_error"
    )
  }

  # Temp VRT for GCP embedding
  vrt_path <- withr::local_tempfile(fileext = ".vrt")

  # Temp path for GDAL warp output
  warp_temp <- withr::local_tempfile(fileext = ".tif")

  # Output path
  if (filename == "") {
    filename <- withr::local_tempfile(fileext = ".tif")
  }

  # Translate source raster coordinates to GDAL pixel/line
  src_xmin <- terra::xmin(x)
  src_ymax <- terra::ymax(x)
  src_xres <- terra::xres(x)
  src_yres <- terra::yres(x)

  # Build -gcp flags: pixel, line, target_x, target_y
  gcp_flags <- purrr::pmap(
    list(
      gcp$source_x,
      gcp$source_y,
      gcp$target_x,
      gcp$target_y
    ),
    \(sx, sy, tx, ty) {
      pixel <- (sx - src_xmin) / src_xres
      line <- (src_ymax - sy) / src_yres
      c(
        "-gcp",
        format(pixel, scientific = FALSE),
        format(line, scientific = FALSE),
        format(tx, scientific = FALSE),
        format(ty, scientific = FALSE)
      )
    }
  ) |>
    purrr::list_c()

  # Embed GCPs in VRT file
  sf::gdal_utils(
    util = "translate",
    source = source_path,
    destination = vrt_path,
    options = c("-of", "VRT", gcp_flags)
  )

  # Warp raster to target grid
  sf::gdal_utils(
    util = "warp",
    source = vrt_path,
    destination = warp_temp,
    options = c(
      "-r",
      method,
      "-te",
      format(terra::xmin(y), scientific = FALSE),
      format(terra::ymin(y), scientific = FALSE),
      format(terra::xmax(y), scientific = FALSE),
      format(terra::ymax(y), scientific = FALSE),
      "-ts",
      as.character(terra::ncol(y)),
      as.character(terra::nrow(y)),
      "-order",
      "1"
    )
  )

  # Read back warped result
  result <- terra::rast(warp_temp)

  # Write to final destination with band names
  terra::writeRaster(
    result,
    filename = filename,
    overwrite = overwrite,
    wopt = list(names = names(x))
  )

  # Set names if in-memory
  names(result) <- names(x)

  # Return
  result
}
