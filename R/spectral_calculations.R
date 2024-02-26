#' Calculate Relative Absorption Band Depth (RABD)
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

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(
    terra::ext(raster),
    resolution = terra::res(raster)
  )

  # If RABD is defined as range and "max" is selected flexibly find the position of the absolute minimum within the range.
  if (rabd_type == "max") {

    # Check type of filename
    if (is.null(filename) == TRUE) {
      filename <- paste0(raster_src, "/", rabd_name, "_max_", raster_name, ".tif")
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
    names(template) <- paste0(rabd_name, "_max")

    cli::cli_alert("Writing {rabd_name}_max to {filename}.")

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
      filename <- paste0(raster_src, "/", rabd_name, "_mid_", raster_name, ".tif")
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
    names(template) <- paste0(rabd_name, "_mid")

    cli::cli_alert("Writing {rabd_name}_mid to {filename}.")

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
      filename <- paste0(raster_src, "/", rabd_name, "_strict_", raster_name, ".tif")
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

    cli::cli_alert("Writing {rabd_name}_strict to {filename}.")

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
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(template)
}

#' Calculate Relative Absorption Band Area (RABA)
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

  cli::cli_h1("{raster_name}")

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", raba_name, "_", raster_name, ".tif")
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

  cli::cli_alert("Writing {raba_name} to {filename}.")

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  # Set layer name based on the raba_name argument
  names(template) <- raba_name

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

  cli::cli_h1("{raster_name}")

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", ratio_name, "_", raster_name, ".tif")
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

  cli::cli_alert("Writing {ratio_name} to {filename}.")

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  raster <- terra::subset(raster, edge_positions[1]) / terra::subset(raster, edge_positions[2])

  # Set layer name
  names(raster) <- ratio_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    raster,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return
  return(raster)
}

#' Calculate band difference
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

  cli::cli_h1("{raster_name}")

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", difference_name, "_", raster_name, ".tif")
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

  cli::cli_alert("Writing {difference_name} to {filename}.")

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  raster <- terra::subset(raster, edge_positions[1]) - terra::subset(raster, edge_positions[2])

  # Set layer name
  names(raster) <- difference_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    raster,
    filename = filename,
    overwrite = TRUE)

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(raster)
}

#' Calculate Rmean
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

  cli::cli_h1("{raster_name}")

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/RMEAN_", raster_name, ".tif")
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

  cli::cli_alert("Writing RMEAN to {filename}.")

  # Apply mean function over entire SpatRaster
  raster <- terra::app(raster, fun = "mean")

  # Set layer name
  names(raster) <- "rmean"

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    raster,
    filename = filename,
    overwrite = TRUE
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return
  return(raster)
}

#' Calculate lambdaREMP
#'
#' @param raster a terra SpatRaster of normalized capture data.
#' @param trough character vector of wavelength to look for trough.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products, provide full path and ext to override.
#'
#' @return one layer terra SpatRaster with calculated lambdaREMP values.
#'
#' @description Calculate lambdaREMP. A wavelength number between 660 and 680 nm, where first derivative equals zero. After Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023. A new index for the rapid generation of chlorophyll time series from hyperspectral imaging of sediment cores. Limnology and Oceanography: Methods 21, 703–717. https://doi.org/10.1002/lom3.10576.
#'
#' @export
calculate_lambdaremp <- function(
    raster,
    trough = c(660, 680),
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
    filename <- paste0(raster_src, "/", "lambdaREMP_", raster_name, ".tif")
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

  cli::cli_alert("Writing lambdaREMP to {filename}.")

  # Find trough position
  remp <- spectra_position(raster = raster, spectra = trough) |>
    # Pull vector with positions
    dplyr::pull(var = 2) |>
    # Subset normalized raster to match trough
    (\(x) terra::subset(raster, x))()
  # Calculate derivative

  # Create empty SpatRaster template from original cropped raster
  template <- terra::rast(terra::ext(raster), resolution = terra::res(raster))

  terra::values(template) <- remp

  # Set layer name
  names(template) <- "lambdaREMP"

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

#' Calculate derivative
#'
#' @param raster terra SpatRaster of normalized capture data.
#' @param derivative_name a character, lower case name of calculated difference.
#' @param edges numeric vector of two for the difference.
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

  cli::cli_h1("{raster_name}")

  # Check type of filename
  if (is.null(filename) == TRUE) {
    filename <- paste0(raster_src, "/", derivative_name, "_", raster_name, ".tif")
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

  cli::cli_alert("Writing {derivative_name} to {filename}.")

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Find derivative value
  raster

  # Set layer name
  names(raster) <- derivative_name

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(
    raster,
    filename = filename,
    overwrite = TRUE
  )

  # Reset window
  terra::window(raster) <- NULL

  # Return raster
  return(raster)
}
