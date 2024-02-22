#' Smooth raster with focal median
#'
#' @param raster a terra SpatRaster to smooth.
#' @param window focal window size, default is 3.
#' @param product logical, whether in the shiny pipeline, what type of filename should be used.
#' @param ... further parameters such as file path.
#'
#' @importFrom stats median
#'
#' @return smoothed SpatRaster
#' @export
#'
filter_median <- function(raster = raster, window = 3, product = TRUE, ...){
  # Store additional parameters
  params <- list(...)

  # Choose write mode
  if (product == TRUE) {
    filename <- paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_MEDIAN.tif")
  } else {
    filename <- paste0(basename(params$path), ".tif")
  }

  cli::cli_h1("{basename(params$path)}")

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  cli::cli_alert_info("{format(Sys.time())}: calculating median filtered raster.")

  # Apply terra focal statistic with 3 x 3 window
  reflectance <- terra::focal(
    raster,
    w = window,
    fun = \(x) stats::median(x),
    filename = filename,
    overwrite = TRUE,
    wopt = wopts)

  cli::cli_alert_success("{format(Sys.time())}: finished.")

  # Return
  return(reflectance)
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
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @return A filtered terra SpatRaster.
#' @export
#'
filter_savgol <- function(raster, p = 3, n = p + 3 - p%%2, m = 0, ts = 1, .extent = NULL, .ext = NULL){
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/", raster_name, "savitzky-golay.", .ext)

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  # Extract names
  band_names <- names(raster)

  cli::cli_alert_info("{format(Sys.time())}: calculating Savitzky-Golay filtered raster.")

  # Apply Savitzky-Golay filter
  raster <- terra::app(
    raster,
    fun = \(raster) signal::sgolayfilt(raster, p = p, n = n, m = m, ts = ts),
    filename = filename,
  overwrite = TRUE,
  wopt = wopts)

  # Set names
  names(raster) <- as.character(band_names)

  # Update names on disk
  terra::update(raster, names = TRUE)

  cli::cli_alert_success("{format(Sys.time())}: finished.")

  # Reset window
  terra::window(raster) <- NULL

  # Return raster to the environment
  return(raster)
}
