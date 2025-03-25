#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param rabd_name character, lower case name name of calculated RABD.
#' @param rabd_type character, lower case, type of RABD. One of "strict" - specific wavelength, "max" - flexible choice of the maximum reflectance dip, "mid" - middle point between the min and max trough wavelength (similar to strict).
#' @param edges numeric vector of two for the wide calculation window.
#' @param trough character vector of wavelength to look for trough.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated RABD values
#' @export
calculate_rabd <- function(
    raster,
    rabd_name,
    rabd_type = "max",
    edges,
    trough,
    extent = NULL,
    ext = NULL,
    filename = NULL) {

  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Named list with write options
  wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(
    terra::ext(raster),
    resolution = terra::res(raster)
  )

  # If RABD is defined as range and "max" is selected flexibly find the position of the absolute minimum within the range.
  if (rabd_type == "max") {

    # Check type of filename
    if (is.null(filename) == TRUE) {
      filename <- paste0(raster_src, "/", toupper(rabd_name), "_max_", raster_name, ".tif")
    } else {
      filename <- fs::path(filename, ext = ext)
    }

      # Set layer name based on the rabd_name argument
    names(template) <- paste0(rabd_name, "_max")

    # Find trough position
    trough_position <- spectra_position(
      raster = raster,
      spectra = trough) |>
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

    } else if (rabd_type == "mid") {

      # Check type of filename
      if (is.null(filename) == TRUE) {
      filename <- paste0(raster_src, "/", toupper(rabd_name), "_mid_", raster_name, ".tif")
      } else {
      filename <- fs::path(filename, ext = ext)
    }

    # Check extent type
    if (is.null(extent) == TRUE) {
      # Set window of interest
      terra::window(raster) <- terra::ext(raster)
    } else {
      # Set window of interest
      terra::window(raster) <- terra::ext(extent)
    }

    # Set layer name based on the rabd_name argument
    names(template) <- paste0(rabd_name, "_mid")

    # Find trough position
    trough <- stats::median(trough)

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

    # If RABD is defined as a specific wavelength.
  } else if (rabd_type == "strict") {

    # Check type of filename
    if (is.null(filename) == TRUE) {
      filename <- paste0(raster_src, "/", toupper(rabd_name), "_strict_", raster_name, ".tif")
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

    # Set layer name based on the rabd_name argument
    names(template) <- paste0(rabd_name, "_strict")

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
  }

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
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE,
    wopt = wopts)

  # Reset the window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}

#' Calculate Relative Absorption Band Area (RABA)
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param raba_name character, lower case name name of calculated RABA.
#' @param edges numeric vector of two for the wide calculation window.
#' @param trough character vector of wavelength to look for trough.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated RABA values.
#' @export
calculate_raba <- function(
    raster,
    raba_name,
    edges,
    trough,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", toupper(raba_name), "_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(
    terra::ext(raster),
    resolution = terra::res(raster))

  # Set layer name based on the raba_name argument
  names(template) <- raba_name

  # Calculate RABD for every band in the range

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}

#' Calculate band ratio
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param ratio_name character, lower case name of calculated ratio.
#' @param edges numeric vector of two for the (numerator and denominator).
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated ratio values.
#' @export
#'
#' @description calculate band ratio of selected wavelengths.
calculate_band_ratio <- function(
    raster,
    ratio_name,
    edges,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", toupper(ratio_name), "_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  template <- terra::subset(raster, edge_positions[1]) / terra::subset(raster, edge_positions[2])

  # Set layer name
  names(template) <- ratio_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return
  return(template)
}

#' Calculate band difference
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param difference_name a character, lower case name of calculated difference.
#' @param edges numeric vector of two for the difference.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated difference values
#' @export
#'
#' @description calculate band difference of selected wavelengths.
calculate_band_difference <- function(
    raster,
    difference_name,
    edges,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", toupper(difference_name), "_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  template <- terra::subset(raster, edge_positions[1]) - terra::subset(raster, edge_positions[2])

  # Set layer name
  names(template) <- difference_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}

#' Calculate Rmean
#'
#' @family Spectral calculations
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated Rmean values.
#' @export
#'
#' @description calculate mean reflectance from all layers for given pixel.
#'
calculate_rmean <- function(
    raster,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/RMEAN_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Apply mean function over entire SpatRaster
  template <- terra::app(raster, fun = "mean")

  # Set layer name
  names(template) <- "rmean"

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return
  return(template)
}

#' Calculate lambdaREMP
#'
#' @family Spectral calculations
#'
#' @param raster A terra SpatRaster of normalized capture data.
#' @param trough_range Numeric vector defining the wavelength range to search for trough (default: c(660, 680)).
#' @param extent An extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param extension Character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products folder, provide full path and extension to override.
#'
#' @return One layer terra SpatRaster with calculated lambdaREMP values.
#'
#' @description Calculate lambdaREMP (wavelength of the red-edge minimum point). 
#' This is the wavelength between 660 and 680 nm where the first derivative of 
#' reflectance equals zero, indicating the maximum absorption of light by chlorophyll.
#' Based on Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023. 
#' A new index for the rapid generation of chlorophyll time series from hyperspectral imaging of sediment cores.
#' Limnology and Oceanography: Methods 21, 703–717. https://doi.org/10.1002/lom3.10576
#'
#' @export
calculate_lambdaremp <- function(
  raster,
  trough_range = c(660, 680),
  extent = NULL,
  extension = NULL,
  filename = NULL) {

# Check if correct class is supplied
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
    paste0("REMP_", raster_name, ".tif")
  )
} else {
  # Ensure proper file extension is applied
  filename <- fs::path(filename, extension = extension %||% "tif")
}

# Extent handling
if (!is.null(extent)) {
  # Set window of interest
  terra::window(raster) <- terra::ext(extent)
}

# Get wavelength values from band names
wavelengths <- as.numeric(names(raster))

# In case names can't be converted to numeric, create a sequence
if (all(is.na(wavelengths))) {
  cli::cli_alert_warning("Band names couldn't be converted to wavelengths. Using band indices instead.")
  wavelengths <- seq_len(terra::nlyr(raster))
}

# Find which bands fall within our trough range
trough_indices <- which(wavelengths >= trough_range[1] & wavelengths <= trough_range[2])

if (length(trough_indices) < 3) {
  rlang::abort(
    message = paste0(
      "Not enough bands found in the trough range (", 
      trough_range[1], "-", trough_range[2], " nm) to calculate derivatives. ",
      "Found only ", length(trough_indices), " bands. Need at least 3."
    )
  )
}

# Function to calculate λREMP using first derivative approach
find_remp_derivative <- function(pixel_values) {
  # Check for NA values
  if (any(is.na(pixel_values[trough_indices]))) {
    return(NA_real_)
  }
  
  # Extract values within trough range
  trough_values <- pixel_values[trough_indices]
  trough_waves <- wavelengths[trough_indices]
  
  # Calculate first derivatives between adjacent bands using purrr
  idx_pairs <- 1:(length(trough_indices) - 1)
  
  derivatives <- purrr::map_dbl(idx_pairs, \(i) {
    delta_refl <- trough_values[i + 1] - trough_values[i]
    delta_wave <- trough_waves[i + 1] - trough_waves[i]
    delta_refl / delta_wave
  })
  
  # Look for zero crossing (where derivative changes from negative to positive)
  idx_pairs_for_crossing <- 1:(length(derivatives) - 1)
  
  zero_cross <- purrr::map_lgl(idx_pairs_for_crossing, \(i) {
    # Check if derivative crosses zero from negative to positive
    derivatives[i] <= 0 && derivatives[i + 1] > 0
  }) |> 
    which()
  
  # If a zero crossing is found
  if (length(zero_cross) > 0) {
    # If multiple zero crossings, take the one with steepest positive slope
    if (length(zero_cross) > 1) {
      # Find crossing with largest positive derivative change
      slope_changes <- derivatives[zero_cross + 1] - derivatives[zero_cross]
      max_change_idx <- zero_cross[which.max(slope_changes)]
    } else {
      max_change_idx <- zero_cross[1]
    }
    
    # Linear interpolation to find exact wavelength where derivative = 0
    x1 <- trough_waves[max_change_idx]
    x2 <- trough_waves[max_change_idx + 1]
    y1 <- derivatives[max_change_idx]
    y2 <- derivatives[max_change_idx + 1]
    
    # Calculate wavelength where derivative = 0
    lambda_remp <- x1 + (0 - y1) * (x2 - x1) / (y2 - y1)
    
    # Make sure result is within the specified range
    lambda_remp <- max(min(lambda_remp, trough_range[2]), trough_range[1])
    
    return(lambda_remp)
  } else {
    # If no zero crossing is found, find the wavelength at minimum reflectance
    # This is a fallback method when the derivative approach doesn't find a solution
    min_idx <- which.min(trough_values)
    return(trough_waves[min_idx])
  }
}

# Apply the function to each pixel
remp_values <- terra::app(
  raster, 
  fun = find_remp_derivative,
  filename = filename,
  overwrite = TRUE
)

# Set the layer name
names(remp_values) <- "REMP"

# Reset window
terra::window(raster) <- NULL

# Return the result
return(remp_values)
}

#' Calculate derivative
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param derivative_name a character, lower case name of calculated difference.
#' @param band numeric vector of one, to find derivative.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated derivative values.
#'
#' @description calculate derivative.
calculate_derivative <- function(
    raster,
    derivative_name,
    band,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", toupper(derivative_name), "_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Find derivative value
  # derivative

  # Set layer name
  names(template) <- derivative_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}

#' Calculate normalized difference index (NDI)
#'
#' @family Spectral calculations
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param ndi_name a character, lower case name of calculated NDI
#' @param edges numeric vector of two for the difference.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated NDI values
#' @export
#'
#' @description calculate band difference of selected wavelengths.
calculate_ndi <- function(
    raster,
    ndi_name,
    edges,
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

  # Check extent type
  if (is.null(extent) == TRUE) {
    # Set window of interest
    raster <- raster
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", toupper(ndi_name), "_", raster_name, ".tif")
  } else {
    filename <- fs::path(filename, ext = ext)
  }

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  template <- (terra::subset(raster, edge_positions[1]) - terra::subset(raster, edge_positions[2])) / (terra::subset(raster, edge_positions[1]) + terra::subset(raster, edge_positions[2]))

  # Set layer name
  names(template) <- ndi_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    template,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}
