#' Smooth raster with focal median
#'
#' @param capture a terra SpatRaster of captured data.
#' @param window focal window size, default is 3.
#'
#' @return smoothed SpatRaster
#' @export
#'
filter_median <- function(raster = raster, window = 3, ...){
  # Store additional parameters
  params <- list(...)

  cli::cli_h1("{basename(params$path)}")

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  cli::cli_alert_info("{format(Sys.time())}: calculating median filtered raster.")

  # Apply terra focal statistic with 3 x 3 window
  reflectance <- terra::focal(raster,
                              w = window,
                              fun = \(x) median(x),
                              filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_MEDIAN.tif"),
                              overwrite = TRUE,
                              wopt = wopts)

  # Return
  return(reflectance)

  cli::cli_alert_success("{format(Sys.time())}: finished.")

}

#' Apply a Savitzky-Golay smoothing filter
#'
#' @description
#' Smooth data with a Savitzky-Golay smoothing filter using \code{\link[signal]{sgolayfilt}}.
#'
#' @param raster a terra SpatRaster of normalized data
#' @param p filter order.
#' @param n filter length (must be odd).
#' @param m return the m-th derivative of the filter coefficients.
#' @param ts time scaling factor.
#'
#' @return A filtered terra SpatRaster.
#' @export
#'
filter_savgol <- function(raster, p = 3, n = p + 3 - p%%2, m = 0, ts = 1, ...){
  # Store additional parameters
  params <- list(...)

  cli::cli_h1("{basename(params$path)}")

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  # Extract names
  band_names <- names(raster)

  cli::cli_alert_info("{format(Sys.time())}: calculating Savitzky-Golay filtered raster.")

  # Apply Savitzky-Golay filter
  raster <- terra::app(raster,
                       fun = \(raster) signal::sgolayfilt(raster, p = p, n = n, m = m, ts = ts),
                       filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_SAVGOL.tif"),
                       overwrite = TRUE,
                       wopt = wopts)

  # Set names
  names(raster) <- as.character(band_names)

  # Update names on disk
  update(raster, names = TRUE)

  # Return raster to the environment
  return(raster)
}
