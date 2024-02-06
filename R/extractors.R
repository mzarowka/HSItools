#' Extract average proxy series from ROI
#'
#' @param raster terra SpatRaster with one layer with calculated values.
#' @param .hsi_index character indicating hyperspectral index layer to plot.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return tibble frame with XY coordinates and averaged proxy values.
#' @export
extract_spectral_series <- function(raster, .hsi_index = NULL, .extent = NULL) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  spectral_series <- raster |>
    terra::aggregate(
      fact = c(1, terra::ncol(raster)),
      fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble()

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_series)
}

#' Extract spectral profile from the ROI
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return a tibble with averaged spectral profile.
#' @export
extract_spectral_profile <- function(raster, .extent = NULL) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  # Aggregate SpatRaster into one data point
  spectral_profile <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), terra::ncol(raster)),
    fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble()

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_profile)
}

#' Extract spectral indices
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param .hsi_index character indicating hyperspectral index layer to plot.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return a tibble with averaged value or multiple values of spectral indices.
#' @export
extract_spectral_indices <- function(raster, .hsi_index = NULL, .extent = NULL) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  # Aggregate SpatRaster into one data point
  spectral_indices <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), terra::ncol(raster)),
    fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble()

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_indices)
}
