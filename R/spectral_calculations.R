#' Remove continuum from spectrum
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return a terra SpatRaster of normalized capture data with continuum removed
#' @export
remove_continuum <- function(raster, ...) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Store additional parameters
  params <- rlang::list2(...)

  # Extract names
  band_names <- names(raster)

  # Remove continuum in a single pixel pixel
  remove_continuum_fun <- function(raster, ...) {
    # Store additional parameters
    params <- rlang::list2(...)

    new_values <- raster |>
      # Coerce to data frame
      as.data.frame() |>
      # Pivot == transpose
      tidyr::pivot_longer(dplyr::everything(),
        names_to = "band",
        values_to = "reflectance",
        names_transform = as.numeric) |>
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
        .keep = "none") |>
      # Get values
      dplyr::pull(.data$reflectance)
  }

  # Apply function over entire SpatRaster
  raster <- terra::app(
    raster,
    fun = \(x) remove_continuum_fun(x),
    filename = paste0(params$path, "/products/REFLECTANCE_CONTINUUM_REMOVED_", basename(params$path), ".tif"),
    overwrite = TRUE)

  # Set names
  names(raster) <- as.character(band_names)

  # Update names on disk
  terra::update(raster, names = TRUE)

  # Return raster to the environment
  return(raster)
}

#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param .edges numeric vector of two for the wide calculation window.
#' @param .trough character vector of wavelength to look for trough.
#' @param .rabd_name character, name of calculated RABD.
#'
#' @return a terra SpatRaster with one layer with calculated RABD values
#' @export
calculate_rabd <- function(raster, .edges, .trough, .rabd_name) {
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

  filename <- paste0(raster_src, "/", .rabd_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {(.rabd_name)} to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(
    terra::ext(raster),
    resolution = terra::res(raster))

  # Set layer name based on the rabd_name argument
  names(template) <- .rabd_name

  # Find trough position
  trough_position <- spectra_position(raster = raster, spectra = .trough) |>
    # Pull vector with positions
    dplyr::pull(var = 2) |>
    # Subset normalized raster to match trough
    (\(x) terra::subset(raster, x))() |>
    # Find trough position
    (\(x) as.numeric(min(x)[1]))() |>
    # Find trough position in the original raster
    (\(x) terra::which.lyr(raster == x))() |>
    # Coerce to integer
    (\(x) as.integer(x[1]))()

  # Find minimum reflectance value in the trough (denominator)
  trough_reflectance <- raster[, , trough_position] |>
    # Coerce to numeric
    as.numeric()

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = .edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Find reflectance value of the left edge (lower wavelength)
  ledge_reflectance <- raster[, , edge_positions[1]]

  # Find reflectance value of the right edge (higher wavelength)
  redge_reflectance <- raster[, , edge_positions[2]]

  # Find number of the bands between through minimum and left edge (lower wavelength, Y)
  ledge_width <- abs(trough_position - edge_positions[1])

  # Find number of the bands between through minimum and right edge (higher wavelength, X)
  redge_width <- abs(trough_position - edge_positions[2])

  # Calculate equation numerator
  numerator <- (redge_width * ledge_reflectance + ledge_width * redge_reflectance) /
    (redge_width + ledge_width)

  # Calculate RABD
  rabd <- numerator / trough_reflectance

  # If there are infinities coerce to 0
  rabd[is.infinite(rabd)] <- 0

  # Set RABD values onto SpatRaster template
  terra::values(template) <- rabd

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}

#' Calculate band ratio
#'
#' @param raster a terra SpatRaster of normalized capture data
#' @param .edges a numeric vector of two for the (nominator and denominator)
#' @param .ratio_name a character, name of calculated ratio
#'
#' @return a terra SpatRaster with one layer with calculated ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths.
calculate_band_ratio <- function(raster, .edges, .ratio_name) {
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

  filename <- paste0(raster_src, "/", .ratio_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {ratio_name} to {filename}.")

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = .edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  raster <- terra::subset(raster, edge_positions[1]) / terra::subset(raster, edge_positions[2])

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(raster, filename = filename, overwrite = TRUE)
}

#' calculate rMean
#'
#' @param raster a terra SpatRaster of normalized capture data.
#'
#' @return a terra SpatRaster with one layer with calculated Rmean values.
#' @export
#'
#' @description calculate mean reflectance from all layers for given pixel.
#'
calculate_rmean <- function(raster) {
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

  filename <- paste0(raster_src, "/RMEAN_", raster_name, ".tif")

  cli::cli_alert("Writing RMEAN to {filename}.")

    # Apply mean function over entire SpatRaster
  raster <- terra::app(raster, fun = "mean")

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(raster, filename = filename, overwrite = TRUE)

  # Return
  return(raster)
}

#' Calculate Relative Absorption Band Area (RABA)
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param .edges a numeric vector of two for the wide calculation window.
#' @param .trough a character vector of wavelength to look for trough.
#' @param .raba_name a character, name of calculated RABA.
#'
#' @return a terra SpatRaster with one layer with calculated RABA values.
#' @export
calculate_raba <- function(raster, .edges, .trough, .raba_name) {
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

  filename <- paste0(raster_src, "/", .raba_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {(.raba_name)} to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  # Set layer name based on the raba_name argument
  names(template) <- .raba_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}

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
    terra::window(raster) <- .extent
  }

  spectral_series <- raster |>
    terra::aggregate(
    fact = c(1, terra::ncol(raster)),
    fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble()

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
    terra::window(raster) <- .extent
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
    terra::window(raster) <- .extent
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

  # Return object
  return(spectral_indices)
}

#' Calculate lambdaREMP
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param .edges a numeric vector of two for the derivative calculation window.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @return a terra SpatRaster with one layer with calculated lambdaREMP values.
#'
#' @description
#' Based on the Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023. A new index for the rapid generation of chlorophyll time series from hyperspectral imaging of sediment cores. Limnology and Oceanography: Methods 21, 703–717. https://doi.org/10.1002/lom3.10576
#'
#' @export
calculate_lambdaremp <- function(raster, .edges, .ext, .write) {
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

  filename <- paste0(raster_src, "/", "lambdaREMP_", raster_name, ".tif")

  cli::cli_alert("Writing lambdaREMP to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  # Set layer name
  names(template) <- "lambdaREMP"

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}
# Function to stack all calculated rasters and RGB channels
stack_raster <- function(data){

}
