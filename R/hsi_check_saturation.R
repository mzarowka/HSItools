#' Check a raster for saturated pixels
#'
#' @family HSI Diagnostics
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param limit Numeric. Saturation threshold in the units of `x`. A pixel is
#'   saturated when its value is greater than or equal to `limit`. Required.
#' @param collapse Logical. Reduce the per-band mask to a single layer marking
#'   pixels saturated in any band. Default `FALSE`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] of `0`/`1` values marking
#'   saturated pixels. With `collapse = FALSE` it has one layer per band of `x`,
#'   carrying the band names of `x`; with `collapse = TRUE` it has a single layer
#'   named `"saturated"`.
#'
#' @description
#' Mark every pixel whose value reaches the sensor's saturation threshold.
#'
#' @details
#' `limit` has no default and is never inferred from the data. It is instrument—specific
#' for the digital number at which the sensor clips and it must be supplied.
#'
#' Further:
#'
#' * **Intended for raw digital numbers.** Saturation is a property of the sensor,
#'   so the check is meaningful only before calibration. The function cannot verify
#'   the processing level of `x` and will not stop you running it on radiance or
#'   reflectance, where the result is meaningless.
#' * **A saturated white reference compromises a whole session.** It corrupts the
#'   denominator of every reflectance calculation that uses it, and the damage is
#'   invisible once calibration has been applied. Check references and captures
#'   right after acquisition, while re-scanning is still an option.
#' * **`NA` propagates rather than defaulting to "not saturated".** Any `NA` in a
#'   pixel's spectrum makes that pixel `NA` in the collapsed mask, even where
#'   another band of the same pixel is saturated. The collapsed mask therefore
#'   never reads as clean on incomplete evidence, which is the point — an unknown
#'   band cannot be ruled out. Raw digital numbers seldom contain `NA` at all; the
#'   usual source is a raster that has already been cropped or masked, where a
#'   background pixel is `NA` in every band and collapses to `NA` either way.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("CAPTURE_testdata.tif")
#'
#' # Per-band mask for a 12-bit sensor
#' x_saturated <- hsi_check_saturation(x, limit = 4095)
#'
#' # Per-band counts of saturated pixels
#' terra::global(x_saturated, "sum", na.rm = TRUE)
#'
#' # Single-layer mask of pixels saturated in any band, written to disk
#' x_any <- hsi_check_saturation(
#'   x,
#'   limit = 4095,
#'   collapse = TRUE,
#'   filename = "saturated.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_check_saturation <- function(
  x,
  limit,
  collapse = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)
  rlang::check_required(limit)
  check_numeric(limit, len = 1)
  rlang::check_bool(collapse)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)

  # A pixel sitting exactly on the saturation threshold has clipped, so the
  # comparison is inclusive. The comparison is lazy, and NA propagates.
  result <- x >= limit

  # Collapsing keeps na.rm = FALSE so that an unknown band cannot silently clear
  # a pixel.
  # terra's any() propagates NA so any NA in a pixel's spectrum yields NA here.
  if (collapse) {
    result <- any(result, na.rm = FALSE)
    names(result) <- "saturated"
  }

  # Build write options
  wopt_default <- list(names = names(result))
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Write to file
  if (filename != "") {
    result <- terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return result
  result
}
