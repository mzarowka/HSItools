#' Remove continuum from spectrum
#'
#' @param raster a terra SpatRaster of normalized capture data
#'
#' @return a terra SpatRaster of normalized capture data with continuum removed
#' @export
remove_continuum <- function(raster, ...) {
  # Store additional parameters
  params <- list(...)

  # Extract names
  band_names <- names(raster)

  # Remove continuum in a single pixel pixel
  remove_continuum_fun <- function(raster, ...) {
    # Store additional parameters
    params <- list(...)

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
        band = as.numeric(name),
        reflectance = value,
        .keep = "none") |>
      # Get values
      dplyr::pull(reflectance)
  }

  # Apply function over entire SpatRaster
  raster <- terra::app(raster,
                       fun = \(x) remove_continuum_fun(x),
                       filename = paste0(params$path, "/products/REFLECTANCE_CONTINUUM_REMOVED_", basename(params$path), ".tif"),
                       overwrite = TRUE)

  # Set names
  names(raster) <- as.character(band_names)

  # Update names on disk
  update(raster, names = TRUE)

  # Return raster to the environment
  return(raster)
}

#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @param raster a terra SpatRaster of normalized capture data
#' @param edges a numeric vector of two for the wide calculation window
#' @param trough a character vector of wavelenght to look for trough
#' @param rabd_name a character, name of calculated RABD
#'
#' @return a terra SpatRaster with one layer with calculated RABD values
#' @export
#'
#' @description # RABD655−680max = ( X × R590 + Y × R730 X+Y ) /R655−680min
#' Denominator: Find the lowest reflectance between 655 and 680
#' Numenator: find reflectance at 590, find reflectance at 730
#' Find how many bands there are between trough minimum and 730
#' Find how many bands there are between trough minimum and 590
#'
calculate_rabd <- function(raster, edges, trough, rabd_name) {
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

  filename <- paste0(raster_src, "/", rabd_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {rabd_name} to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster),
                          resolution = terra::res(raster))

  # Set layer name based on the rabd_name argument
  names(template) <- rabd_name

  # Find trough position
  trough_position <- spectra_position(raster = raster, spectra = trough) |>
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
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
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

  # Calculate equation nominator
  nominator <- (redge_width * ledge_reflectance + ledge_width * redge_reflectance) /
    (redge_width + ledge_width)

  # Calculate RABD
  rabd <- nominator / trough_reflectance

  # If there are infinities coerce to 0
  rabd[is.infinite(rabd)] <- 0

  # Set rabd values onto SpatRaster template
  values(template) <- rabd

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}

#' Calculate band ratio
#'
#' @param raster a terra SpatRaster of normalized capture data
#' @param edges a numeric vector of two for the (nominator and denominator)
#' @param ratio_name a character, name of calculated ratio
#'
#' @return a terra SpatRaster with one layer with calculated ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths
#'
calculate_band_ratio <- function(raster, edges, ratio_name) {
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

  filename <- paste0(raster_src, "/", ratio_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {ratio_name} to {filename}.")

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
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

  filename <- paste0(raster_src, "/", RMEAN, "_", raster_name, ".tif")

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
#' @param edges a numeric vector of two for the wide calculation window.
#' @param trough a character vector of wavelenght to look for trough.
#' @param raba_name a character, name of calculated RABA.
#'
#' @return a terra SpatRaster with one layer with calculated RABA values.
#' @export
#'
#' @description # RABD655−680max = ( X × R590 + Y × R730 X+Y ) /R655−680min
#' Denominator: Find the lowest reflectance between 655 and 680
#' Numenator: find reflectance at 590, find reflectance at 730
#' Find how many bands there are between trough minimum and 730
#' Find how many bands there are between trough minimum and 590
#'
calculate_raba <- function(raster, edges, trough, raba_name) {
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

  filename <- paste0(raster_src, "/", raba_name, "_", raster_name, ".tif")

  cli::cli_alert("Writing {raba_name} to {filename}.")

    # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  # Set layer name based on the raba_name argument
  names(template) <- raba_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}

#' Extract average proxy profile
#'
#' @param raster a terra SpatRaster with one layer with calculated values
#'
#' @return a data frame with XY coordinates and averaged proxy values
#' @export
#'
extract_profile <- function(raster) {
  # Aggregate SpatRaster into average rows
  profile <- terra::aggregate(raster,
                              fact = c(1, ncol(raster)),
                              fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE)

  # Return object
  return(profile)
}

#' Calcualte λREMP
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param edges a numeric vector of two for the derivative calculation window.
#'
#' @return a terra SpatRaster with one layer with calculated λREMP values.
#' @export
calculate_lambdaremp <- function(raster, edges) {
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

  cli::cli_alert("Writing λREMP to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  # Set layer name based on the raba_name argument
  names(template) <- "REMP"

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = filename, overwrite = TRUE)

  # Return raster
  return(template)
}



# Function to stack all calculated rasters and RGB channels
stack_raster <- function(data){}
