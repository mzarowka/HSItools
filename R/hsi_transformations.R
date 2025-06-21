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
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param type Character. One of "RGB", "CIR", "NIR", "SWIR" or any choice of three bands
#' @param tol Numeric. Tolerance for band selection in nm (default: 25)
#' @param histeq logical. If TRUE histogram equalization is used instead of linear stretch
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
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
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = type
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # New logic

  if (type == "RGB") {
    spectra <- c(650, 550, 450)
  } else if (type == "NIR") {
    spectra <- c(900, 800, 700)
  } else if (type == "CIR") {
    spectra <- c(860, 650, 555)
  } else if (type == "SWIR") {
    spectra <- c(2200, 1650, 1200)
  } else {
    spectra <- type
  }

  if (all(any(purrr::list_c(purrr::map(spectra,\(i) dplyr::near(i, as.numeric(terra::names(x)), tol = tol))))) == FALSE) {
    cli::cli_abort("No layers matching {.arg type} within {.arg tol}.",
    i = "Are you sure your SpatRaster have appropriate layers?")
  }

  # Old logic

  if (type == "RGB") {
    # Check if there are values close to RGB, within the tolerance
    if (
      all(
        any(
          purrr::list_c(
            purrr::map(
              c(650, 550, 450),
              \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = tol)
            )
          )
        )
      ) ==
        TRUE
    ) {
      spectra <- c(650, 550, 450)
    } else {
      cli::cli_warn(
        "No layers matching RGB. Using the first, middle and last available layers."
      )
      spectra <- c(
        min(1:terra::nlyr(x)),
        terra::median(1:terra::nlyr(x)),
        max(terra::nlyr(x))
      ) |>
        (\(i) as.numeric(terra::names(1:terra::subset(x, i))))()
    }
  } else if (type == "CIR") {
    # Check if there are values close to CIR, within the tolerance
    if (
      all(
        any(
          purrr::list_c(
            purrr::map(
              c(860, 650, 555),
              \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = tol)
            )
          )
        )
      ) ==
        TRUE
    ) {
      spectra <- c(860, 650, 555)
    } else {
      cli::cli_abort("No layers matching CIR.")
    }
  } else if (type == "NIR") {
    # Check if there are values close to NIR, within the tolerance
    if (
      all(
        any(
          purrr::list_c(
            purrr::map(
              c(900, 800, 700),
              \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = tol)
            )
          )
        )
      ) ==
        TRUE
    ) {
      spectra <- c(900, 800, 700)
    } else {
      cli::cli_abort("No layers matching NIR.")
    }
  } else if (type == "SWIR") {
    # Check if there are values close to SWIR, within the tolerance
    if (
      all(
        any(
          purrr::list_c(
            purrr::map(
              c(2200, 1650, 1200),
              \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = tol)
            )
          )
        )
      ) ==
        TRUE
    ) {
      spectra <- c(2200, 1650, 1200)
    } else {
      cli::cli_abort("No layers matching SWIR.")
    }
  }

  # Resume code

  # Subset and stretch
  result <- HSItools::spectra_position(
    x,
    spectra = spectra
  ) |>
    HSItools::spectra_sub(
      raster = x,
      spectra_tbl = _
    ) |>
    terra::stretch(
      filename = filename,
      histeq = histeq,
      overwrite = overwrite,
      wopt = wopt
    )

  # Return SpatRaster
  return(result)
}
