#' Mask hyperspectral raster
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param mask A [`SpatRaster`][terra::SpatRaster-class] with mask.
#' @param inverse
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with masked data.
#'
#' @export
#' @examples
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
