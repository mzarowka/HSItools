#' Spectral raster smooth with a Savitzky-Golay filter
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param p Integer. Filter polynomial order (typically 2-4)
#' @param n Integer. Filter length/window size (must be odd, typically 5-15)
#' @param m Integer. Derivative order (0 = smoothing, 1 = first derivative, etc.)
#' @param ts Numeric. Sampling interval for derivative calculations
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Smooth hyperspectral data using a Savitzky-Golay filter via \code{\link[gsignal]{sgolayfilt}}.
#' This filter fits successive sub-sets of adjacent data points with a low-degree polynomial
#' by the method of linear least squares.
#'
#' @details
#' The Savitzky-Golay filter is a spectral smoothing technique that preserves features
#' of the spectral curve such as peak height and width, which are usually flattened
#' by other smoothing methods. The filter works by fitting a polynomial of order \code{p}
#' through a moving window of \code{n} points.
#'
#' Any pixels with NA values will result in function failure.
#'
#' Requires the \pkg{gsignal} package.
#'
#' @return A terra SpatRaster with Savitzky-Golay filtered values
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate continuum removed reflectance
#' x_savgol <- hsi_smooth_savgol(x)
#'
#' # Save to file
#' x_savgol <- hsi_smooth_savgol(
#'  x,
#'  filename = "output_savgol.tif",
#'  overwrite = TRUE)
#' }
#'
#' @export
hsi_smooth_savgol <- function(
  x,
  p = 3,
  n = p + 13 - p %% 2,
  m = 0,
  ts = 1,
  cores = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Check if gsignal is available
  if (!requireNamespace("gsignal", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gsignal} is required for Savitzky-Golay filtering.",
      "i" = "Install with: {.code install.packages('gsignal')}"
    )
  }

  # Filter polynomial order (typically 2-4)
  if (p >= n) {
    cli::cli_abort(
      "Filter order {.arg p} must be less than filter length {.arg n}."
    )
  }

  # Validate input
  check_numeric(n, odd = TRUE)

  # Check if filter parameters are positive
  if (p < 0 || n < 0 || m < 0) {
    cli::cli_abort("Filter parameters must be non-negative.")
  }

  # Check for sufficient bands
  if (terra::nlyr(x) < n) {
    cli::cli_abort(
      "SpatRaster has {terra::nlyr(x)} bands but filter length is {n}.",
      "i" = "Reduce filter length or use a raster with more bands."
    )
  }

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
    fun = \(x) gsignal::sgolayfilt(as.vector(x), p = p, n = n, m = m, ts = ts),
    filename = filename,
    overwrite = overwrite,
    cores = cores,
    wopt = wopt
  )

  # Return raster
  return(result)
}
