#' Spectral raster smooth with a Savitzky-Golay filter
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param p Integer. Filter polynomial order. Typically 2–4. Default `3`.
#' @param n Positive odd integer. Filter window size. Must be odd and greater
#'   than `p`. Typically 5–15. Default computed from `p`.
#' @param m Integer. Derivative order. `0` for smoothing, `1` for first
#'   derivative. Default `0`.
#' @param ts Numeric. Sampling interval for derivative calculations. Default `1`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with Savitzky-Golay
#'   filtered values.
#'
#' @description
#' Smooth hyperspectral data using a Savitzky-Golay filter via
#' [`gsignal::sgolayfilt()`]. The filter fits successive subsets of adjacent
#' data points with a low-degree polynomial by the method of linear least
#' squares.
#'
#' @details
#' The Savitzky-Golay filter preserves spectral features such as peak height
#' and width that are typically flattened by other smoothing methods. The
#' filter fits a polynomial of order `p` through a moving window of `n` points.
#'
#' Setting `m = 1` or `m = 2` computes the first or second derivative of the
#' smoothed spectrum respectively; higher-order derivatives are also supported
#' by increasing `m`. Note that edge bands equal to roughly half the window
#' size are unreliable for derivatives — always compute on the full spectrum
#' before subsetting to a wavelength range of interest.
#'
#' Pixels with `NA` values will cause the function to fail. For full-raster
#' processing, [`hsi_tiled()`] can distribute the workload across parallel
#' workers. Requires the
#' [`gsignal`](https://CRAN.R-project.org/package=gsignal) package.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_savgol <- hsi_smooth_savgol(x)
#'
#' x_savgol <- hsi_smooth_savgol(
#'   x,
#'   filename = "output_savgol.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_smooth_savgol <- function(
  x,
  p = 3,
  n = p + 13 - p %% 2,
  m = 0,
  ts = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate required packages
  rlang::check_installed("gsignal")

  # Filter polynomial order (typically 2-4)
  if (p >= n) {
    cli::cli_abort(
      "Filter order {.arg p} must be less than filter length {.arg n}.",
      class = "hsitools_error"
    )
  }

  # Validate input
  check_numeric(n, odd = TRUE)

  # Check if filter parameters are positive
  if (p < 0 || n < 0 || m < 0) {
    cli::cli_abort(
      "Filter parameters must be non-negative.",
      class = "hsitools_error"
    )
  }

  # Check for sufficient bands
  if (terra::nlyr(x) < n) {
    cli::cli_abort(
      "SpatRaster has {terra::nlyr(x)} bands but filter length is {n}.",
      "i" = "Reduce filter length or use a raster with more bands.",
      class = "hsitools_error"
    )
  }

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

  # Apply Savitzky-Golay filter
  result <- terra::app(
    x,
    fun = \(x) {
      if (anyNA(x)) {
        return(rep(NA_real_, length(x)))
      }
      gsignal::sgolayfilt(as.vector(x), p = p, n = n, m = m, ts = ts)
    },
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return
  result
}
