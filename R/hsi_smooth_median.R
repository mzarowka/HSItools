#' Focal raster smooth with a median
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param window Positive odd integer. Focal window size. Default `3`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with median filtered values.
#'
#' @details
#' `NA` cells are preserved rather than filled. The focal window is applied with
#' `na.policy = "omit"`, so a cell that is already `NA` stays `NA` instead of
#' taking the median of its neighbours. Cells that hold data are unaffected and
#' still smooth across whichever neighbours are available, so masked input
#' changes only the masked pixels.
#'
#' This matters when the input has been masked, for example against a saturation
#' screen: a blank marks a pixel with no usable measurement, and filling it from
#' its neighbours would fabricate a spectrum where the mask recorded an absence.
#' Whether a blank covers one band or all of them is decided upstream, by
#' whatever produced the mask; this function only declines to overwrite it.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_smooth_median <- hsi_smooth_median(x)
#'
#' x_smooth_median <- hsi_smooth_median(
#'   x,
#'   filename = "output_median.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_smooth_median <- function(
  x,
  window = 3,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

  check_numeric(window, odd = TRUE)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply terra focal statistic
  result <- terra::focal(
    x,
    w = window,
    fun = "median",
    na.rm = TRUE,
    na.policy = "omit",
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return
  result
}
