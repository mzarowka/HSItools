#' Extract average proxy series from ROI
#'
#' @param raster terra SpatRaster with one layer with calculated values.
#' @param index character indicating hyperspectral index layer to plot.
#' @param categorical is SpatRaster categorical. Defaults to FALSE. If categorical, then most abundant class is retained.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return tibble frame with XY coordinates and averaged proxy values.
#' @export
extract_spectral_series <- function(
  raster,
  index,
  categorical = FALSE,
  calibration = NULL,
  extent = NULL
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Set extents in windows
  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Subset if layers provided
  if (!is.null(index)) {
    raster <- raster |>
      terra::subset(index)
  }

  # Check if it is categorical
  if (categorical == TRUE) {
    # Check calibration
    if (is.null(calibration) == TRUE) {
      spectral_series <- raster |>
        # Coerce do data frame with coordinates
        terra::as.data.frame(xy = TRUE) |>
        # To tibble
        dplyr::tibble()
    } else {
      spectral_series <- raster |>
        # Coerce do data frame with coordinates
        terra::as.data.frame(xy = TRUE) |>
        # To tibble
        dplyr::tibble() |>
        # Gropb by y
        dplyr::group_by(.data$y) |>
        # Count occurences
        dplyr::count(.data$layer) |>
        # Keep max
        dplyr::slice_max(order_by = .data$n, n = 1) |>
        # Ungroup
        dplyr::ungroup() |>
        # Calculate metric depths
        dplyr::mutate(
          depth.mm = calibration$distance - (.data$y * calibration$pixel_ratio),
          tube.mm = .data$depth.mm - calibration$point_zero
        ) |>
        # Drop x and y
        dplyr::select(-.data$y) |>
        # Keep only non-negative depths
        dplyr::filter(.data$tube.mm >= 0)
    }
  } else {
    if (is.null(calibration) == TRUE) {
      spectral_series <- raster |>
        terra::aggregate(
          fact = c(1, terra::ncol(raster)),
          fun = "mean",
          na.rm = TRUE
        ) |>
        # Coerce do data frame with coordinates
        terra::as.data.frame(xy = TRUE) |>
        # To tibble
        dplyr::tibble()
    } else {
      spectral_series <- raster |>
        terra::aggregate(
          fact = c(1, terra::ncol(raster)),
          fun = "mean",
          na.rm = TRUE
        ) |>
        # Coerce do data frame with coordinates
        terra::as.data.frame(xy = TRUE) |>
        # To tibble
        dplyr::tibble() |>
        # Calculate metric depths
        dplyr::mutate(
          depth.mm = calibration$distance - (.data$y * calibration$pixel_ratio),
          tube.mm = .data$depth.mm - calibration$point_zero
        ) |>
        # Drop x and y
        dplyr::select(-c(.data$x, .data$y)) |>
        # Keep only non-negative depths
        dplyr::filter(.data$tube.mm >= 0)
    }
  }

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_series)
}

#' Extract spectral profile from the ROI
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return a tibble with averaged spectral profile.
#' @export
extract_spectral_profile <- function(
  raster,
  extent = NULL
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Aggregate SpatRaster into one data point
  spectral_profile <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), terra::ncol(raster)),
    fun = "mean"
  ) |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble() |>
    # Drop x and y
    dplyr::select(-c(.data$x, .data$y))

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_profile)
}

#' Extract spectral indices
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param hsi_index character indicating hyperspectral index layer to plot.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return a tibble with averaged value or multiple values of spectral indices.
#' @export
extract_spectral_indices <- function(
  raster,
  hsi_index = NULL,
  extent = NULL
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Aggregate SpatRaster into one data point
  spectral_indices <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), terra::ncol(raster)),
    fun = "mean"
  ) |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble()

  # Reset window
  terra::window(raster) <- NULL

  # Return object
  return(spectral_indices)
}
