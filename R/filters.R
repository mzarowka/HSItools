#' Remove continuum from spectrum
#'
#' @family Filters
#' @param raster terra SpatRaster of normalized capture data.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#' @param extension character, a graphic format extension.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return one layer terra SpatRaster with continuum removed.
#' @export
remove_continuum <- function(
  raster,
  extent = NULL,
  filename = NULL,
  extension = NULL,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Validate required packages
  if (!requireNamespace("prospectr", quietly = TRUE)) {
    rlang::abort("Package 'prospectr' is required for continuum removal.")
  }

  # Filename handling
  if (is.null(filename)) {
    # Extract source information
    raster_src <- dirname(terra::sources(raster))

    # Extract file name
    raster_name <- tools::file_path_sans_ext(basename(terra::sources(raster)))

    # Construct default filename
    filename <- fs::path(
      raster_src,
      paste0(raster_name, "_CONTINUUM-REMOVED.tif")
    )
  } else {
    # Ensure proper file extension is applied
    filename <- fs::path(filename, extension = extension %||% "tif")
  }

  # Extent handling
  if (!is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  }

  # Extract names
  band_names <- names(raster)

  # Named list with write options
  wopts <- list(
    steps = terra::ncell(raster) * terra::nlyr(raster),
    names = band_names
  )

  # Get wavelengths
  wavelengths <- suppressWarnings(as.numeric(band_names))

  # If wavelengths couldn't be converted, create a sequence
  if (all(is.na(wavelengths))) {
    wavelengths <- seq_along(band_names)
  }

  # Continuum removal function
  remove_continuum_fun <- function(x) {
    # Skip NA values
    if (any(is.na(x))) return(rep(NA, length(x)))

    # For a single pixel, we need to transpose the data structure
    X_matrix <- matrix(x, nrow = 1) # 1 sample (pixel) with multiple wavelengths as columns

    # Apply continuum removal - expects wavelengths and reflectance values
    # Note: prospectr::continuumRemoval returns only the CR values
    cr_result <- prospectr::continuumRemoval(X = X_matrix, wav = wavelengths)

    return(as.vector(cr_result))
  }

  # Apply function over entire SpatRaster
  raster <- terra::app(
    raster,
    fun = remove_continuum_fun,
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(raster)
}

#' Smooth raster with focal median
#'
#' @family Filters
#' @param raster a terra SpatRaster to smooth.
#' @param window focal window size, default is 3.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#' @param extension character, a graphic format extension.
#'
#' @importFrom stats median
#'
#' @return smoothed SpatRaster
#' @export
filter_median <- function(
  raster,
  window = 3,
  extent = NULL,
  filename = NULL,
  extension = NULL
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Validate window terra::size (must be odd)
  if (window %% 2 == 0) {
    rlang::abort("Window size must be an odd number.")
  }

  # Filename handling
  if (is.null(filename)) {
    # Extract source information
    raster_src <- dirname(terra::sources(raster))

    # Extract file name
    raster_name <- tools::file_path_sans_ext(basename(terra::sources(raster)))

    # Construct default filename
    filename <- fs::path(
      raster_src,
      paste0(raster_name, "_MEDIAN.tif")
    )
  } else {
    # Ensure proper file extension is applied
    filename <- fs::path(filename, extension = extension %||% "tif")
  }

  # Extent handling
  if (!is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  }

  # Extract names
  band_names <- names(raster)

  # Named list with write options
  wopts <- list(
    steps = terra::ncell(raster) * terra::nlyr(raster),
    names = band_names
  )

  # Apply terra focal statistic with 3 x 3 window
  raster <- terra::focal(
    raster,
    w = window,
    fun = median,
    na.rm = TRUE,
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

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
#' @family Filters
#' @param raster a terra SpatRaster of normalized data
#' @param p filter order.
#' @param n filter length (must be odd).
#' @param m return the m-th derivative of the filter coefficients.
#' @param ts time scaling factor.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#' @param extension character, a graphic format extension.
#'
#' @return A filtered terra SpatRaster.
#' @export
#'
filter_savgol <- function(
  raster,
  p = 3,
  n = p + 13 - p %% 2,
  m = 0,
  ts = 1,
  extent = NULL,
  filename = NULL,
  extension = NULL
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Filename handling
  if (is.null(filename)) {
    # Extract source information
    raster_src <- dirname(terra::sources(raster))

    # Extract file name
    raster_name <- tools::file_path_sans_ext(basename(terra::sources(raster)))

    # Construct default filename
    filename <- fs::path(
      raster_src,
      paste0(raster_name, "_SAVITZKY-GOLAY.tif")
    )
  } else {
    # Ensure proper file extension is applied
    filename <- fs::path(filename, extension = extension %||% "tif")
  }

  # Extent handling
  if (!is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  }

  # Extract names
  band_names <- names(raster)

  # Write options
  wopts <- list(
    steps = terra::ncell(raster) * terra::nlyr(raster),
    names = band_names
  )

  # Apply Savitzky-Golay filter
  raster <- terra::app(
    raster,
    fun = \(raster) signal::sgolayfilt(raster, p = p, n = n, m = m, ts = ts),
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(raster)
}
