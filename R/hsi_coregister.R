#' Co-register source raster to target raster grid
#'
#' @family HSI Co-registration
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] to warp.
#' @param y A [`SpatRaster`][terra::SpatRaster-class] defining the output grid.
#'   Output extent, resolution, and dimensions are taken from this raster.
#' @param gcp A [data.frame] or [tibble][tibble::tibble] of matched control
#'   points from [`hsi_match_gcp()`]. Each row identifies the same point in
#'   both images through `source_x`, `source_y`, `target_x`, and `target_y`.
#' @param method Character. Resampling method. One of `"near"`, `"bilinear"`,
#'   `"cubic"`, `"cubicspline"`, `"lanczos"`. Default `"bilinear"`.
#' @param filename Character. Output filename. Default `""` keeps result in
#'   memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] aligned to the `y` grid.
#'
#' @description
#' Warp a source raster onto the target grid using matched control points.
#' Areas without source coverage are `NA`.
#'
#' @details
#' A control point (GCP) is the same recognizable feature marked in both
#' images.
#'
#' The function fits a first-order affine transformation from the points, then
#' uses [`terra::project()`] to warp directly onto the grid defined by `y`.
#' Band names from `x` are preserved.
#'
#' @seealso
#' [`hsi_match_gcp()`] for matching control points,
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

  # Validate coordinate columns
  coord_cols <- c("source_x", "source_y", "target_x", "target_y")
  check_has_cols(gcp, cols = coord_cols)

  if (!all(vapply(gcp[coord_cols], is.numeric, logical(1)))) {
    cli::cli_abort("GCP coordinate columns must be numeric.")
  }

  if (any(!is.finite(as.matrix(gcp[coord_cols])))) {
    cli::cli_abort("GCP coordinate columns must contain only finite values.")
  }

  # Check minimum of 3 GCPs
  if (nrow(gcp) < 3) {
    cli::cli_abort(
      "Need at least 3 matched GCPs, found {nrow(gcp)}.",
      class = "hsitools_error"
    )
  }

  design <- cbind(1, gcp$source_x, gcp$source_y)

  # Check for collinear GCPs
  if (qr(design)$rank < 3) {
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

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  if (filename != "" && file.exists(filename) && !overwrite) {
    cli::cli_abort(
      c(
        "File {.file {filename}} already exists.",
        "i" = "Use {.arg overwrite = TRUE} to overwrite."
      ),
      class = "hsitools_error"
    )
  }

  # Drop CRS from both rasters - work in pixel space
  x <- hsi_drop_crs(x)
  y <- hsi_drop_crs(y)

  # Fit target coordinates as affine functions of source coordinates.
  coeffs_x <- stats::lm.fit(design, gcp$target_x)$coefficients
  coeffs_y <- stats::lm.fit(design, gcp$target_y)$coefficients

  affine_pipeline <- sprintf(
    paste(
      "+proj=pipeline +step +proj=affine",
      "+xoff=%.17g +yoff=%.17g",
      "+s11=%.17g +s12=%.17g +s21=%.17g +s22=%.17g"
    ),
    coeffs_x[[1]],
    coeffs_y[[1]],
    coeffs_x[[2]],
    coeffs_x[[3]],
    coeffs_y[[2]],
    coeffs_y[[3]]
  )

  terra::crs(x) <- "local"

  terra::project(
    x,
    y,
    method = method,
    pipeline = affine_pipeline,
    filename = filename,
    overwrite = overwrite
  )
}
