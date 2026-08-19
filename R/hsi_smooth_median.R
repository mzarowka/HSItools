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
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return
  result
}
