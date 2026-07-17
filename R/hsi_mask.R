#' Mask hyperspectral raster
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param mask A [`SpatRaster`][terra::SpatRaster-class] with a single mask
#'   layer. Must have the same extent and resolution as `x`.
#' @param inverse Logical. How `mask` is read. Default `FALSE`.
#'   \describe{
#'     \item{`FALSE`}{Keep-mask: nonzero cells are kept, `0` cells dropped.}
#'     \item{`TRUE`}{Bad-mask: nonzero cells are dropped, `0` cells kept.}
#'   }
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with masked values.
#'
#' @details
#' Dropped cells become `NA` in every layer of `x`. A single mask layer is
#' recycled across all layers, so each cell is kept or dropped for its whole
#' spectrum.
#'
#' Cells that are `NA` in `mask` are dropped whatever `inverse` is set to. `NA`
#' marks a cell the mask carries no information about, and an unknown cell is
#' discarded rather than trusted. This departs from [`terra::mask()`], which
#' keeps `NA` cells when `inverse = TRUE`.
#'
#' Mask *creation* is out of scope: what counts as background, defect, or
#' specimen is domain knowledge that varies by material and study. Masks are
#' built upstream, whether by thresholding a band or index, by rasterising
#' digitised polygons with [`terra::rasterize()`], or by any other rule.
#'
#' Combine several sources in mask space rather than calling this function
#' repeatedly. Masks are single-layer and cheap to combine, while `x` is the
#' expensive side. Keep every source on one polarity while combining: bad-masks
#' combine as a union (`any()`), keep-masks as an intersection (`all()`), and
#' mixing the two silently turns one into the other. Note that
#' [`terra::rasterize()`] writes `NA`, not `0`, outside its polygons, so pass
#' `background = 0` to keep the result usable in that algebra.
#'
#' Masking precedes endmember search and pixel statistics. Background and tray
#' pixels are spectrally distinct from the specimen, and will be selected as
#' endmembers or skew band statistics if left in.
#'
#' @seealso
#' [`hsi_destripe()`], which likewise requires background pixels excluded.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Keep-mask: nonzero marks the specimen to keep
#' specimen <- terra::rast("SPECIMEN_testdata.tif")
#'
#' x_mask <- hsi_mask(x, specimen)
#'
#' # Bad-mask: nonzero marks defects to drop
#' defects <- terra::rast("DEFECTS_testdata.tif")
#'
#' x_mask <- hsi_mask(x, defects, inverse = TRUE)
#'
#' x_mask <- hsi_mask(
#'   x,
#'   defects,
#'   inverse = TRUE,
#'   filename = "REFLECTANCE_mask.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_mask <- function(
  x,
  mask,
  inverse = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

  check_spatraster(mask)

  rlang::check_bool(inverse)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Check number of layers
  if (terra::nlyr(mask) != 1) {
    cli::cli_abort(
      "{.arg mask} must contain exactly 1 layer, not {terra::nlyr(mask)}.",
      class = "hsitools_error"
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Mask raster. maskvalues list what is to discard when inverse = FALSE and what to keep
  # when inverse = TRUE. This way NA drops everytime.
  result <- terra::mask(
    x,
    mask,
    maskvalues = if (inverse) 0 else c(NA, 0),
    inverse = inverse,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt_user
  )

  # Return result
  result
}
