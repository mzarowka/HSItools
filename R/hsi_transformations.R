#' Focal raster smooth with a median
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param window focal window size, default is 3
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @details
#' Focal (spatial) median filter smoothes data by findig the median value within a given window and assiging its value to a pixel of interest.
#'
#' @return A terra SpatRaster with median filtered values
#' @export
hsi_median <- function(
  x,
  window = 3,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate window terra::size (must be odd)
  if (window %% 2 == 0) {
    cli::abort("Window size must be an odd number.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply terra focal statistic with 3 x 3 window
  result <- terra::focal(
    x,
    w = window,
    fun = "median",
    na.rm = TRUE,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return raster
  return(result)
}

#' Spectral raster smooth with a Savitzky-Golay filter
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param p filter order
#' @param n filter length (must be odd)
#' @param m return the m-th derivative of the filter coefficients
#' @param ts time scaling factor
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' #' @description
#' Smooth data with a Savitzky-Golay smoothing filter using \code{\link[signal]{sgolayfilt}}.
#'
#' @details
#' Focal (spatial) median filter smoothes data by findig the median value within a given window and assiging its value to a pixel of interest.
#'
#' @return A terra SpatRaster with Savitzky-Golay filtered values
#' @export
hsi_savgol <- function(
  x,
  p = 3,
  n = p + 13 - p %% 2,
  m = 0,
  ts = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply Savitzky-Golay filter
  result <- terra::app(
    x,
    fun = \(x) signal::sgolayfilt(x, p = p, n = n, m = m, ts = ts),
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return raster
  return(result)
}

#' Remove continuum from hyperspectral data
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @details
#' Continuum removal normalizes reflectance spectra to highlight absorption
#' features by removing the overall spectral shape. The continuum is the
#' convex hull that connects local maxima in the spectrum.
#'
#' @return A terra SpatRaster with continuum-removed values
#' @export
hsi_continuum <- function(
  x,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate if it is possible to remove the continuum
  if (terra::nlyr(x) < 3) {
    cli::cli_abort(
      "Input raster must have at least 3 bands for continuum removal."
    )
  }

  # Validate required packages
  if (!requireNamespace("prospectr", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg prospectr} is required for continuum removal.",
      "i" = "Install with: {.code utils::install.packages('prospectr')}"
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get wavelengths
  wavelengths <- suppressWarnings(as.numeric(band_names))

  # If wavelengths couldn't be converted, create a sequence
  if (all(is.na(wavelengths))) {
    wavelengths <- seq_along(band_names)
  }

  # Continuum removal function
  remove_continuum_fun <- function(x) {
    # Skip NA values
    if (anyNA(x)) {
      return(rep(NA, length(x)))
    }

    # For a single pixel, transpose the data structure
    X_matrix <- matrix(x, nrow = 1) # 1 sample (pixel) with multiple wavelengths as columns

    # Apply continuum removal - expects wavelengths and reflectance values
    # Note: prospectr::continuumRemoval returns only the CR values
    cr_result <- prospectr::continuumRemoval(X = X_matrix, wav = wavelengths)

    return(as.vector(cr_result))
  }

  # Apply function over entire SpatRaster
  result <- terra::app(
    x,
    fun = remove_continuum_fun,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return SpatRaster
  return(result)
}

#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param rabd_name Character. Name of calculated RABD
#' @param rabd_type Character. Type of RABD. One of "strict" - specific wavelength, "max" - flexible choice of the maximum reflectance dip, "mid" - middle point between the min and max trough wavelength (similar to strict)
#' @param edges Numeric. Vector of two for the wide calculation window
#' @param trough Character. Vector of wavelength(s) to look for trough
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with RABD values
#' @export
hsi_rabd <- function(
  x,
  rabd_name,
  rabd_type,
  edges,
  trough,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = rabd_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # If RABD is defined as range and "max" is selected flexibly find the position of the absolute minimum within the range.
  if (rabd_type == "max") {
    # Find trough position
    trough_position <- spectra_position(
      raster = x,
      spectra = trough
    ) |>
      # Pull vector with positions
      dplyr::pull(var = 2) |>
      # Subset normalized raster to match trough
      (\(i) terra::subset(x, i))() |>
      # Find trough position
      (\(i) as.numeric(min(i)[1]))() |>
      # Find trough position in the original raster
      (\(i) terra::which.lyr(x == i))() |>
      # Coerce to integer
      (\(i) as.integer(i[1]))()
  } else if (rabd_type == "mid") {
    # Find trough position
    trough <- stats::median(trough)

    # Find trough position
    trough_position <- spectra_position(raster = x, spectra = trough) |>
      # Pull vector with positions
      dplyr::pull(var = 2) |>
      # Subset normalized raster to match trough
      (\(i) terra::subset(x, i))() |>
      # Find trough position
      (\(i) as.numeric(min(i)[1]))() |>
      # Find trough position in the original raster
      (\(i) terra::which.lyr(x == i))() |>
      # Coerce to integer
      (\(i) as.integer(i[1]))()

    # If RABD is defined as a specific wavelength.
  } else if (rabd_type == "strict") {
    # Find trough position
    trough_position <- spectra_position(raster = x, spectra = trough) |>
      # Pull vector with positions
      dplyr::pull(var = 2) |>
      # Subset normalized raster to match trough
      (\(i) terra::subset(x, i))() |>
      # Find trough position
      (\(i) as.numeric(min(i)[1]))() |>
      # Find trough position in the original raster
      (\(i) terra::which.lyr(x == i))() |>
      # Coerce to integer
      (\(i) as.integer(x[i]))()
  }

  # Find minimum reflectance value in the trough (denominator)
  trough_reflectance <- x[,, trough_position] |>
    # Coerce to numeric
    as.numeric()

  # Find edge positions
  edge_positions <- spectra_position(raster = x, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Find reflectance value of the left edge (lower wavelength)
  ledge_reflectance <- x[,, edge_positions[1]]

  # Find reflectance value of the right edge (higher wavelength)
  redge_reflectance <- x[,, edge_positions[2]]

  # Find number of the bands between through minimum and left edge (lower wavelength, Y)
  ledge_width <- abs(trough_position - edge_positions[1])

  # Find number of the bands between through minimum and right edge (higher wavelength, X)
  redge_width <- abs(trough_position - edge_positions[2])

  # Calculate equation numerator
  numerator <- (redge_width *
    ledge_reflectance +
    ledge_width * redge_reflectance) /
    (redge_width + ledge_width)

  # Calculate RABD
  rabd <- numerator / trough_reflectance

  # If there are infinities coerce to 0
  rabd[is.infinite(rabd)] <- 0

  # Set RABD values onto SpatRaster template
  terra::values(result) <- rabd

  # Set name
  names(result) <- rabd_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return raster
  return(result)
}

#' Calculate band ratio
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param ratio_name Character. Name of calculated ratio
#' @param edges Numeric. Vector of two for the numerator and denominator
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths
hsi_ratio <- function(
  x,
  ratio_name,
  edges,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = ratio_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Find edge positions
  edge_positions <- spectra_position(raster = x, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  result <- terra::subset(x, edge_positions[1]) /
    terra::subset(x, edge_positions[2])

  # Set layer name
  names(result) <- ratio_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  return(result)
}

#' Calculate band difference
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param difference_name Character. Name of calculated ratio
#' @param edges Numeric. Vector of two for the numerator and denominator
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths.
hsi_difference <- function(
  x,
  difference_name,
  edges,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = difference_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Find edge positions
  edge_positions <- spectra_position(raster = x, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  result <- terra::subset(x, edge_positions[1]) -
    terra::subset(x, edge_positions[2])

  # Set layer name
  names(result) <- difference_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  return(result)
}

#' Calculate mean reflectance (Rmean)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param rmean_name Character. Name of calculated rmean.
#' @param na.rm Logical. Remove NA values when calculating mean (default: TRUE)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with mean reflectance values
#' @export
#'
#' @description Calculate mean reflectance across all spectral bands for each pixel
#' in a hyperspectral image.
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' hsi_data <- terra::rast("hyperspectral_image.tif")
#'
#' # Calculate mean reflectance
#' rmean <- hsi_rmean(hsi_data, rmean_name = "mean_reflectance")
#'
#' # Save to file
#' rmean <- hsi_rmean(hsi_data,
#'                    rmean_name = "mean_reflectance",
#'                    filename = "output_rmean.tif",
#'                    overwrite = TRUE)
#' }
hsi_rmean <- function(
  x,
  rmean_name,
  na.rm = TRUE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate name handling
  if (
    missing(rmean_name) || !is.character(rmean_name) || length(rmean_name) != 1
  ) {
    cli::cli_abort("{.arg rmean_name} must be a single character string.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = rmean_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply mean function over entire SpatRaster
  result <- terra::app(x, fun = "mean", na.rm = na.rm)

  # Set layer name
  names(result) <- rmean_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      ...
    )
  }

  # Return
  return(result)
}

#' Calculate Relative Absorption Band Area (RABA)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param raba_name Character. Name of calculated RABA
#' @param edges Numeric. Vector of two for the wide calculation window
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with RABA values
#' @export
hsi_raba <- function(
  x,
  raba_name,
  edges,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = raba_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  ##########################
  ##### Function logic goes here
  # Set RABD values onto SpatRaster template
  # terra::values(result) <- work_here
  ##########################

  # Set name
  names(result) <- raba_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return raster
  return(result)
}

#' Calculate lambdaREMP
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param remp_name Character. Name of calculated index
#' @param edges Numeric. Vector of two for the wide calculation window. Default c(660, 680)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with lambdaREMP values
#'
#' @description Calculate lambdaREMP (wavelength of the red-edge minimum point).
#' This is the wavelength somewhere between 660 and 680 nm where the first derivative of
#' reflectance equals zero, indicating the maximum absorption of light by chlorophyll.
#' Based on Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023.
#' A new index for the rapid generation of chlorophyll time series from hyperspectral imaging of sediment cores.
#' Limnology and Oceanography: Methods 21, 703-717 https://doi.org/10.1002/lom3.10576
#'
#' @export
hsi_remp <- function(
  x,
  remp_name,
  edges = c(660, 680),
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = remp_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get wavelength values from band names
  wavelengths <- as.numeric(names(x))

  # In case names can't be converted to numeric, create a sequence
  if (all(is.na(wavelengths))) {
    cli::cli_alert_warning(
      "Band names couldn't be converted to wavelengths. Using band indices instead."
    )
    wavelengths <- seq_len(terra::nlyr(x))
  }

  # Find which bands fall within trough range
  trough_indices <- which(
    wavelengths >= edges[1] & wavelengths <= edges[2]
  )

  if (length(trough_indices) < 3) {
    cli::cli_abort(
      message = paste0(
        "Not enough bands found in the trough range (",
        edges[1],
        "-",
        edges[2],
        " nm) to calculate derivatives. ",
        "Found only ",
        length(trough_indices),
        " bands. Need at least 3."
      )
    )
  }

  # Calculate lambdaREMP using first derivative approach
  find_remp_derivative <- function(pixel_values) {
    # Check for NA values
    if (any(is.na(pixel_values[trough_indices]))) {
      return(NA_real_)
    }

    # Extract values within trough range
    trough_values <- pixel_values[trough_indices]
    trough_waves <- wavelengths[trough_indices]

    # Calculate first derivatives between adjacent bands
    idx_pairs <- 1:(length(trough_indices) - 1)

    derivatives <- purrr::map_dbl(idx_pairs, \(i) {
      delta_refl <- trough_values[i + 1] - trough_values[i]
      delta_wave <- trough_waves[i + 1] - trough_waves[i]
      delta_refl / delta_wave
    })

    # Zero crossing (where derivative changes from negative to positive)
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
      lambda_remp <- max(min(lambda_remp, edges[2]), edges[1])

      return(lambda_remp)
    } else {
      # If no zero crossing is found, find the wavelength at minimum reflectance
      # This is a fallback method when the derivative approach doesn't find a solution
      min_idx <- which.min(trough_values)
      return(trough_waves[min_idx])
    }
  }

  # Apply the function to each pixel
  result <- terra::app(
    x,
    fun = find_remp_derivative,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set the layer name
  names(result) <- remp_name

  # Return the result
  return(result)
}

#' Calculate spectral derivative
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param derivative_name Character. Name of calculated ratio
#' @param band Numeric. Wavelength at which to calculate the derivative
#' @param method Character. method to use for derivative calculation. One of "central" (default), "forward", or "backward".
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description
#' Calculates the spectral derivative at a specific wavelength using one of three methods:
#' - "central": Central difference method, [f(x+h1) - f(x-h2)]/(h1+h2)
#' - "forward": Forward difference method, [f(x+h) - f(x)]/h
#' - "backward": Backward difference method, [f(x) - f(x-h)]/h
#'
#' Where h, h1, and h2 are wavelength differences between bands.
#' The derivative provides information about the rate of change in reflectance,
#' which can be useful for identifying absorption features and inflection points.
hsi_derivative <- function(
  x,
  derivative_name,
  band,
  method = "central",
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = derivative_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Validate method
  method <- match.arg(method, c("central", "forward", "backward"))

  # Get wavelengths from band names
  wavelengths <- x |>
    terra::names() |>
    as.numeric()

  # If wavelengths couldn't be converted, create a sequence
  if (all(is.na(wavelengths))) {
    cli::cli_alert_warning(
      "Band names couldn't be converted to wavelengths. Using band indices instead."
    )
    wavelengths <- seq_len(terra::nlyr(x))
  }

  # Find position of the requested band
  band_position <- x |>
    spectra_position(spectra = band) |>
    dplyr::pull(var = 2)

  # Validate band position is within range
  if (band_position < 1 || band_position > terra::nlyr(x)) {
    cli::cli_abort(
      message = paste0(
        "Requested band (",
        band,
        ") is outside the available range of ",
        "your raster (",
        min(wavelengths),
        "-",
        max(wavelengths),
        ")."
      )
    )
  }

  # Calculate derivative based on method
  if (method == "central") {
    if (band_position <= 1 || band_position >= terra::nlyr(x)) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'central' method for band at position ",
          band_position,
          ". Need bands before and after for central difference. ",
          "Try using 'forward' or 'backward' methods instead."
        )
      )
    }

    # Calculate using central difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_minus <- terra::subset(i, band_position - 1)
        band_plus <- terra::subset(i, band_position + 1)
        wave_diff <- wavelengths[band_position + 1] -
          wavelengths[band_position - 1]
        (band_plus - band_minus) / wave_diff
      })()
  } else if (method == "forward") {
    if (band_position >= terra::nlyr(x)) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'forward' method for the last band at position ",
          band_position,
          ". Try using 'backward' method instead."
        )
      )
    }

    # Calculate using forward difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_current <- terra::subset(i, band_position)
        band_plus <- terra::subset(i, band_position + 1)
        wave_diff <- wavelengths[band_position + 1] - wavelengths[band_position]
        (band_plus - band_current) / wave_diff
      })()
  } else if (method == "backward") {
    if (band_position <= 1) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'backward' method for the first band at position ",
          band_position,
          ". Try using 'forward' method instead."
        )
      )
    }

    # Calculate using backward difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_current <- terra::subset(i, band_position)
        band_minus <- terra::subset(i, band_position - 1)
        wave_diff <- wavelengths[band_position] - wavelengths[band_position - 1]
        (band_current - band_minus) / wave_diff
      })()
  }

  # Set derivative values onto SpatRaster template
  # terra::values(result) <- derivative_values

  # Set layer name
  names(result) <- derivative_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  return(result)
}

#' Calculate normalized difference index (NDI)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param ndi_name Character. Name of calculated ratio
#' @param edges Numeric. Vector of two for the numerator and denominator
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ndi values
#' @export
#'
#' @description calculate normalized difference index
hsi_ndi <- function(
  x,
  ndi_name,
  edges,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = ndi_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Find edge positions
  edge_positions <- spectra_position(raster = x, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  result <- (terra::subset(x, edge_positions[1]) -
    terra::subset(x, edge_positions[2])) /
    (terra::subset(x, edge_positions[1]) +
      terra::subset(x, edge_positions[2]))

  # Set layer name
  names(result) <- ndi_name

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  return(result)
}

#' Stretch and optionally save full RGB preview of SpatRaster
#'
#' Performs histogram stretching on selected bands from a hyperspectral SpatRaster.
#' Supports both predefined band combinations and custom wavelength selection.
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data. Band names must be
#'   numeric wavelengths in nm.
#' @param type Character or numeric. Either a predefined band combination
#'   ("RGB", "CIR", "NIR", "SWIR") or a numeric vector of exactly 3
#'   wavelengths in nm (e.g., c(400, 500, 600))
#' @param tol Numeric. Tolerance for band selection in nm (default: 25)
#' @param histeq Logical. If TRUE histogram equalization is used instead of
#'   linear stretch (default: FALSE)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A SpatRaster with 3 bands after stretching
#'
#' @examples
#' \dontrun{
#' # Using predefined band combination
#' rgb_stretched <- hsi_stretch(hyperspectral_raster, type = "RGB")
#'
#' # Using custom wavelengths
#' custom_stretched <- hsi_stretch(hyperspectral_raster, type = c(400, 500, 600))
#'
#' # Save to file with histogram equalization
#' hsi_stretch(hyperspectral_raster, type = "CIR", histeq = TRUE,
#'             filename = "cir_stretched.tif", overwrite = TRUE)
#' }
#'
#' @export
hsi_stretch <- function(
  x,
  type,
  tol = 25,
  histeq = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input raster
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate and process the type argument
  if (is.character(type) && length(type) == 1) {
    # Predefined band combinations
    spectra <- switch(
      type,
      RGB = c(650, 550, 450),
      NIR = c(900, 800, 700),
      CIR = c(860, 650, 555),
      SWIR = c(2200, 1650, 1200),
      cli::cli_abort(
        "Unknown band type: {.val {type}}",
        i = "Use one of: RGB, NIR, CIR, SWIR, or provide numeric wavelengths."
      )
    )
  } else if (is.numeric(type)) {
    # Custom wavelengths
    if (length(type) != 3) {
      cli::cli_abort(
        "Custom wavelengths must provide exactly 3 values, got {length(type)}."
      )
    }
    spectra <- type
  } else {
    cli::cli_abort(
      "{.arg type} must be either a character string (e.g., 'RGB') or a numeric vector of 3 wavelengths."
    )
  }

  # Band names should always be the wavelengths (as character)
  band_names <- as.character(spectra)

  # Validate tolerance
  if (!is.numeric(tol) || length(tol) != 1 || tol < 0) {
    cli::cli_abort("{.arg tol} must be a single non-negative numeric value.")
  }

  # Check if all required bands exist
  available_bands <- as.numeric(terra::names(x))

  # Check each band individually
  band_exists <- purrr::map_lgl(spectra, \(target_wl) {
    any(dplyr::near(target_wl, available_bands, tol = tol))
  })

  if (!all(band_exists)) {
    missing_bands <- spectra[!band_exists]
    cli::cli_abort(
      c(
        "Cannot find all required bands within tolerance of {tol} nm.",
        x = "Missing bands near: {missing_bands} nm",
        i = "Available bands: {sort(available_bands)} nm"
      )
    )
  }

  # Find band positions and subset
  band_positions <- HSItools::spectra_position(
    x,
    spectra = spectra
  )

  selected_bands <- HSItools::spectra_sub(
    raster = x,
    spectra_tbl = band_positions
  )

  # Perform stretching
  if (filename != "") {
    # If saving to file, pass writeRaster options
    result <- terra::stretch(
      selected_bands,
      histeq = histeq,
      filename = filename,
      overwrite = overwrite,
      names = band_names,
      ...
    )
  } else {
    # If keeping in memory
    result <- terra::stretch(
      selected_bands,
      histeq = histeq
    )
    names(result) <- band_names
  }

  # Return stretched SpatRaster
  return(result)
}

#' Normalize hyperspectral raster
#'
#' @family HSI Transformations
#' @param sample A terra SpatRaster with hyperspectral sample data. Band names must be
#'   numeric wavelengths in nm.
#' @param whiteref A terra SpatRaster with hyperspectral white reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param darkref A terra SpatRaster with hyperspectral dark reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param tint A vector of two with integration times for white reference and sample data (in this order).
#'
#' @details
#' Normalizes a SpatRaster (prefferably a layer) in respect to white and dark references.
#'
#' @return A temporary terra SpatRaster with normalized reflectance values.
#' @export
hsi_normalize <- function(
  sample,
  whiteref,
  darkref,
  tint
) {
  # Get the average value of the white reference for each column
  whiteref_onecol_raster <- terra::aggregate(
    whiteref,
    fact = c(terra::nrow(whiteref), 1),
    fun = "mean"
  )

  # Store it in a vector
  whiteref_onecol_vector <- as.vector(whiteref_onecol_raster)

  # Get the average value of the dark reference for each column
  darkref_onecol_raster <- terra::aggregate(
    darkref,
    fact = c(terra::nrow(darkref), 1),
    fun = "mean"
  )

  # Store it in a vector
  darkref_onecol_vector <- as.vector(darkref_onecol_raster)

  # Convert the raster to a matrix
  sample_matrix <- terra::as.matrix(sample, wide = TRUE)

  # Subtract the dark reference from the capture matrix for each column
  numerator <- sweep(sample_matrix, 2, darkref_onecol_vector, FUN = "-")

  # Subtract the dark reference from the white reference for each column
  denominator <- whiteref_onecol_vector - darkref_onecol_vector

  f_tint <- tint[1] / tint[2]

  # Divide the numerator by the denominator for each column and multiply by the tint factor
  result <- sweep(numerator, 2, denominator, "/") * f_tint

  # Set the result to NA if the denominator is lower than 0
  result[is.na(result) | result < 0] <- 0

  # Create a temporary raster to store the result
  result <- terra::init(
    sample,
    t(result),
    filename = tempfile(fileext = ".tif"),
    wopt = list(gdal = c("COMPRESS=NONE"))
  )

  # Return SpatRaster
  return(result)
}

#' Hyperspectral reflectance raster
#'
#' @family HSI Transformations
#' @param sample A terra SpatRaster with hyperspectral sample data. Band names must be
#'   numeric wavelengths in nm.
#' @param whiteref A terra SpatRaster with hyperspectral white reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param darkref A terra SpatRaster with hyperspectral dark reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param tint A vector of two with integration times for white reference and sample data (in this order). Default c(1, 1).
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @details
#' Normalizes a SpatRaster layer by layer in respect to white and dark references.
#'
#' @return A terra SpatRaster with normalized reflectance values.
#' @export
hsi_reflectance <- function(
  sample,
  whiteref,
  darkref,
  tint = c(1, 1),
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Needs cleanup
  # Needs proper validation, now it produces and error with not matching raster extents
  # Needs to properly handle temporary files, otherwise it clogs up the drive almost imediately
  # Test if last parallelization in purrr gives anything

  # Validate input
  # if (
  #   !all(purrr::map_lgl(c(sample, whiteref, darkref), \(x) {
  #     inherits(x, what = "SpatRaster")
  #   }))
  # ) {
  #   cli::cli_abort("All of inputs must be terra SpatRasters.")
  # }

  # # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # # Extract band names
  # band_names <- terra::names(x)

  # # Named list with write options
  wopt_default <- list(
    # names = band_names
  )

  # # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Perform normalization
  # If saving to file, pass writeRaster options
  result <- list(
    sample = terra::as.list(sample),
    whiteref = terra::as.list(whiteref),
    darkref = terra::as.list(darkref),
    tint = list(tint)
  ) |>
    purrr::pmap(purrr::in_parallel(\(sample, whiteref, darkref, tint) {
      HSItools::hsi_normalize(
        sample = sample,
        whiteref = whiteref,
        darkref = darkref,
        tint = tint
      )
    })) |>
    terra::rast()

  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return SpatRaster
  return(result)
}
