#' Remove continuum from spectrum
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return one layer terra SpatRaster with continuum removed.
#' @export
remove_continuum <- function(
    raster,
    extent = NULL,
    ext = NULL,
    filename = NULL,
    ...) {
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

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", raster_name, "_continuum-removed.tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Check extent type
  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  cli::cli_alert_info("{format(Sys.time())}: calculating continuum removed raster.")

  # Extract names
  band_names <- names(raster)

  # Remove continuum in a single pixel pixel
  remove_continuum_fun <- function(raster) {
    # Get new values
    new_values <- raster |>
      # Coerce to data frame
      as.data.frame() |>
      # Pivot == transpose
      tidyr::pivot_longer(dplyr::everything(),
        names_to = "band",
        values_to = "reflectance",
        names_transform = as.numeric
      ) |>
      # Coerce to matrix
      as.matrix() |>
      # Remove continuum
      (\(x) prospectr::continuumRemoval(x[, 2], x[, 1]))() |>
      # Coerce to tibble
      tibble::enframe() |>
      # Coerce band to numeric
      dplyr::mutate(
        band = as.numeric(.data$name),
        reflectance = .data$value,
        .keep = "none"
      ) |>
      # Get values
      dplyr::pull(.data$reflectance)
  }

  # Apply function over entire SpatRaster
  raster <- terra::app(
    raster,
    fun = \(x) remove_continuum_fun(x),
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

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

#' Smooth raster with focal median
#'
#' @param raster a terra SpatRaster to smooth.
#' @param window focal window size, default is 3.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @importFrom stats median
#'
#' @return smoothed SpatRaster
#' @export
filter_median <- function(
    raster = raster,
    window = 3,
    extent = NULL,
    ext = NULL,
    filename = NULL) {
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

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", raster_name, "_median.tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Check extent type
  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  cli::cli_alert_info("{format(Sys.time())}: calculating median filtered raster.")

  # Apply terra focal statistic with 3 x 3 window
  raster <- terra::focal(
    raster,
    w = window,
    fun = \(x) stats::median(x),
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

  cli::cli_alert_success("{format(Sys.time())}: finished.")

  # Reset window
  terra::window(raster) <- NULL

  # Return
  return(raster)
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
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return A filtered terra SpatRaster.
#' @export
#'
filter_savgol <- function(raster, p = 3, n = p + 13 - p %% 2, m = 0, ts = 1, extent = NULL, ext = NULL, filename = NULL) {
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

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", raster_name, "_savitzky-golay.tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Check extent type
  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
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
    wopt = wopts
  )

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
