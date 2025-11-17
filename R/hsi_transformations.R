#' Focal raster smooth with a median
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param window Focal window size, must be odd (default: 3)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Apply a focal (spatial) median filter to smooth hyperspectral data.
#' The median filter finds the median value within a given window and assigns
#' it to the pixel of interest, reducing noise while preserving edges.
#'
#' @details
#' The focal median filter smoothes data by finding the median value within
#' a given window and assigning its value to a pixel of interest.
#'
#' @return A terra SpatRaster with median filtered values
#' @export
hsi_smooth_median <- function(
  x,
  window = 3,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate window size (must be odd)
  if (window %% 2 == 0) {
    cli::cli_abort("{.arg window} size must be an odd number.")
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

#' Remove continuum from hyperspectral data
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param cores positive integer. If cores > 1, a 'parallel' package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Remove the spectral continuum from hyperspectral reflectance data to
#' normalize spectra and highlight absorption features. The continuum represents
#' the overall convex hull shape of the spectrum connecting local maxima.
#'
#' @details
#' Continuum removal normalizes reflectance spectra to highlight absorption
#' features by removing the overall spectral shape. The continuum is the
#' convex hull that connects local maxima in the spectrum.
#'
#' Requires the \pkg{prospectr} package.
#'
#' @return A terra SpatRaster with continuum-removed values
#' @export
hsi_remove_continuum <- function(
  x,
  filename = "",
  overwrite = FALSE,
  cores = 1,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate if it is possible to remove the continuum
  if (terra::nlyr(x) < 3) {
    cli::cli_abort(
      "Input raster must have at least 3 bands for continuum removal.",
      i = "Current raster has {terra::nlyr(x)} band{?s}."
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
    cli::cli_alert_warning(
      "Band names cannot be converted to wavelengths. Using band indices."
    )
    wavelengths <- seq_along(band_names)
  }

  # Continuum removal function
  remove_continuum_fun <- function(x) {
    # Skip NA values
    if (anyNA(x)) {
      return(rep(NA_real_, length(x)))
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
    cores = cores,
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
#' @param continuum_edges Numeric. Vector of two for the continuum anchor points
#' @param absorption_band Numeric Vector of wavelength(s) to look for trough (absorption feature location)
#' @param index_type Character. Type of RABD. One of:
#' #'   \describe{
#'     \item{"strict"}{Use specific wavelength as trough}
#'     \item{"max"}{Flexibly find maximum reflectance dip within trough range}
#'     \item{"mid"}{Use midpoint between min and max trough wavelength}
#'   }
#' @param index_name Character. Name of calculated RABD index. Default NULL.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with RABD values
#' @export
hsi_calc_rabd <- function(
  x,
  continuum_edges,
  absorption_band,
  index_type,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate input
  if (!index_type %in% c("strict", "mid", "max")) {
    cli::cli_abort(
      "Input {.arg index_type} must be one of 'strict', 'mid' or 'max'."
    )
  }

  # # Validate name handling
  # if (
  #   missing(index_name) || !is.character(index_name) || length(index_name) != 1
  # ) {
  #   cli::cli_abort("{.arg index_name} must be a single character string.")
  # }

  # Validate continuum_edges
  if (!is.numeric(continuum_edges) || length(continuum_edges) != 2) {
    cli::cli_abort(
      "{.arg continuum_edges} must be a numeric vector of length 2 (wavelength boundaries)."
    )
  }

  # Validate absorption feature
  if (!is.numeric(absorption_band)) {
    cli::cli_abort(
      "{.arg absorption_band} must be numeric."
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # If RABD is defined as range and "max" is selected flexibly find the position of the absolute minimum within the range.
  if (index_type == "max") {
    # Find trough position
    trough_position <- wavelength_position(
      x = x,
      wavelength = absorption_band
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
  } else if (index_type == "mid") {
    # Find trough position
    trough <- stats::median(absorption_band)

    # Find trough position
    trough_position <- wavelength_position(
      x = x,
      wavelength = absorption_band
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

    # If RABD is defined as a specific wavelength.
  } else if (index_type == "strict") {
    # Find trough position
    trough_position <- wavelength_position(
      x = x,
      wavelength = absorption_band
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
  }

  # Find minimum reflectance value in the trough (denominator)
  trough_reflectance <- x[,, trough_position] |>
    # Coerce to numeric
    as.numeric()

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = continuum_edges) |>
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
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

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
#' @param bands Numeric vector of length 2. The two wavelengths (in nm)
#'   to use for ratio calculation
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a band ratio index by dividing reflectance at one wavelength by
#' reflectance at another wavelength. Band ratios are commonly used to
#' normalize spectral data and highlight specific features such as clay
#' minerals or dust content.
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths
hsi_calc_ratio <- function(
  x,
  bands,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate bands
  if (missing(bands)) {
    cli::cli_abort("{.arg bands} is required and cannot be missing.")
  }

  if (!is.numeric(bands) || length(bands) != 2) {
    cli::cli_abort(
      c(
        "{.arg bands} must be a numeric vector of length 2.",
        "x" = "Got {typeof(bands)} of length {length(bands)}",
        "i" = "Example: bands = c(570, 630)"
      )
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = bands) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  result <- terra::subset(x, edge_positions[1]) /
    terra::subset(x, edge_positions[2])

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

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
#' @param bands Numeric vector of length 2. The two wavelengths (in nm)
#'   to use for difference calculation
#' @param index_name Character. Name of calculated difference index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a band difference index by subtracting reflectance at one wavelength
#' from reflectance at another wavelength. Band differences can highlight
#' spectral features and are commonly used to detect clay minerals, dust, and
#' other sedimentary components.
#'
#' @export
hsi_calc_difference <- function(
  x,
  bands,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate bands
  if (missing(bands)) {
    cli::cli_abort("{.arg bands} is required and cannot be missing.")
  }

  if (!is.numeric(bands) || length(bands) != 2) {
    cli::cli_abort(
      c(
        "{.arg bands} must be a numeric vector of length 2.",
        "x" = "Got {typeof(bands)} of length {length(bands)}",
        "i" = "Example: bands = c(675, 750)"
      )
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = bands) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  result <- terra::subset(x, edge_positions[1]) -
    terra::subset(x, edge_positions[2])

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

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
#' @param index_name Character. Name of calculated rmean. Default NULL
#' @param na.rm Logical. Remove NA values when calculating mean (default: TRUE)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param cores positive integer. If cores > 1, a 'parallel' package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with mean reflectance values
#' @export
#'
#' @description
#' Calculate mean reflectance across all spectral bands for each pixel in a
#' hyperspectral image. This provides a measure of overall brightness and can
#' be useful for normalizing other spectral indices.
#'
#' @details
#' Mean reflectance (Rmean) is calculated as the arithmetic mean of reflectance
#' values across all wavelengths for each pixel
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' hsi_data <- terra::rast("hyperspectral_image.tif")
#'
#' # Calculate mean reflectance
#' rmean <- hsi_rmean(hsi_data, index_name = "mean_reflectance")
#'
#' # Save to file
#' rmean <- hsi_rmean(hsi_data,
#'                    index_name = "mean_reflectance",
#'                    filename = "output_rmean.tif",
#'                    overwrite = TRUE)
#' }
hsi_calc_rmean <- function(
  x,
  index_name = NULL,
  na.rm = TRUE,
  filename = "",
  overwrite = FALSE,
  cores = 1,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Conditional writing can be, probably, handled a little bit better?

  # Apply mean function over entire SpatRaster
  result <- terra::app(x, fun = "mean", na.rm = na.rm, cores = cores)

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

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
#' @param continuum_edges Numeric vector of length 2. Wavelength boundaries
#'   (in nm) that define the continuum for the calculation window
#' @param index_name Character. Name of calculated RABA index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param cores Positive integer. Number of cores for parallel processing
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate Relative Absorption Band Area (RABA), which quantifies the total
#' absorption across a spectral feature by summing band-by-band RABD calculations.
#' Implementation follows Butz et al. (2015) formula in HSI scanning manual.
#'
#' @details
#' RABA extends the RABD concept from a single point to the entire absorption
#' feature. The method calculates RABD at each wavelength between the continuum
#' edges and sums them. The continuum is calculated using linear interpolation.
#'
#' This approach:
#' - Uses the same continuum concept as \code{\link{hsi_calc_rabd}}
#' - Integrates across the entire absorption feature
#' - Is bandwidth-independent (works with any spectral resolution)
#' - Provides a measure of total absorption strength
#'
#' @return A terra SpatRaster with RABA values
#'
#' @seealso
#' \code{\link{hsi_calc_rabd}} for single-point absorption depth
#'
#' @examples
#' \dontrun{
#' # Calculate RABA for chlorophyll-a (typical range 650-700 nm)
#' raba_chl <- hsi_calc_raba(
#'   x = reflectance,
#'   continuum_edges = c(650, 700),
#'   index_name = "raba_650_700"
#' )
#'
#' # Calculate RABA for different spectral window (590-730 nm)
#' raba_broad <- hsi_calc_raba(
#'   x = reflectance,
#'   continuum_edges = c(590, 730),
#'   index_name = "raba_590_730"
#' )
#'
#' # Save to file with parallel processing
#' raba <- hsi_calc_raba(
#'   x = reflectance,
#'   continuum_edges = c(650, 700),
#'   index_name = "raba_650_700",
#'   cores = 4,
#'   filename = "raba_output.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_raba <- function(
  x,
  continuum_edges,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  cores = 1,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate continuum edges
  if (!is.numeric(continuum_edges) || length(continuum_edges) != 2) {
    cli::cli_abort(
      "{.arg continuum_edges} must be a numeric vector of length 2 (wavelength boundaries)."
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get wavelengths from band names
  wavelengths <- as.numeric(terra::names(x))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths."
      )
    )
  }

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = continuum_edges) |>
    dplyr::pull(2)

  # Get all bands between edges (including edges)
  band_range <- seq(
    from = min(edge_positions),
    to = max(edge_positions)
  )

  # Extract wavelengths in the range
  range_wavelengths <- wavelengths[band_range]

  # Subset to bands in range
  x_range <- terra::subset(x, band_range)

  # Define the per-pixel calculation function
  calc_raba_pixel <- \(pixel_values) {
    # Handle NA values
    if (anyNA(pixel_values)) {
      return(NA_real_)
    }

    # Extract edge reflectances
    r_start <- pixel_values[1]
    r_end <- pixel_values[length(pixel_values)]

    # Number of bands (excluding end point, following Butz formula)
    n_bands <- length(pixel_values) - 1

    # Calculate sum of RABDs using Butz
    rabd_sum <- purrr::map_dbl(
      0:(n_bands - 1),
      \(i) {
        # Linear interpolation for continuum at position i
        continuum_i <- ((r_end - r_start) / n_bands) * i + r_start

        # RABD at position i: continuum / actual reflectance
        # Handle division by zero
        if (pixel_values[i + 1] == 0) {
          return(0)
        }

        continuum_i / pixel_values[i + 1]
      }
    ) |>
      sum()

    return(rabd_sum)
  }

  # Apply function to each pixel
  result <- terra::app(
    x_range,
    fun = calc_raba_pixel,
    cores = cores,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

  # Return raster
  return(result)
}

#' Calculate lambdaREMP
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param search_range Numeric. Vector of two for the wide calculation window. Default c(660, 680)
#' @param index_name Character. Name of calculated index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param cores positive integer. If cores > 1, a 'parallel' package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with lambdaREMP values
#'
#' @description
#' Calculate lambda REMP (wavelength of the Red-Edge Minimum Point), which
#' identifies the wavelength where the first derivative of reflectance equals
#' zero, indicating maximum chlorophyll absorption. This index provides a
#' precise measure of chlorophyll content in sediment cores.
#'
#' Based on Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023.
#' A new index for the rapid generation of chlorophyll time series from hyperspectral imaging of sediment cores.
#' Limnology and Oceanography: Methods 21, 703-717 https://doi.org/10.1002/lom3.10576
#'
#' @details
#' Lambda REMP identifies the wavelength between approximately 660-680 nm where
#' the first derivative of reflectance crosses from negative to positive
#' (i.e., the inflection point where reflectance transitions from decreasing
#' to increasing). This wavelength is highly sensitive to chlorophyll-a
#' concentration.
#'
#' The algorithm:
#' 1. Calculates first derivatives between adjacent bands within the search range
#' 2. Identifies zero-crossings (negative to positive)
#' 3. Uses linear interpolation to find the exact wavelength where derivative = 0
#' 4. If multiple crossings exist, selects the one with the steepest slope
#' 5. Falls back to minimum reflectance if no zero-crossing is found
#'
#' @references
#' Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023.
#' A new index for the rapid generation of chlorophyll time series from
#' hyperspectral imaging of sediment cores. Limnology and Oceanography:
#' Methods 21, 703-717. \doi{10.1002/lom3.10576}
#'
#' @export
hsi_calc_remp <- function(
  x,
  search_range = c(660, 680),
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  cores = 1,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate search_range
  if (!is.numeric(search_range) || length(search_range) != 2) {
    cli::cli_abort(
      "{.arg search_range} must be a numeric vector of length 2 (wavelength range)."
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
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
    wavelengths >= search_range[1] & wavelengths <= search_range[2]
  )

  if (length(trough_indices) < 3) {
    cli::cli_abort(
      message = paste0(
        "Not enough bands found in the trough range (",
        search_range[1],
        "-",
        search_range[2],
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
    cores = cores,
    wopt = wopt
  )

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

  # Return the result
  return(result)
}

#' Calculate spectral derivative
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param band Numeric. Wavelength (in nm) at which to calculate the derivative
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param method Character. method to use for derivative calculation. One of "central" (default), "forward", or "backward".
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description
#' Calculate the spectral derivative (rate of change of reflectance with respect
#' to wavelength) at a specific wavelength. Derivatives are useful for identifying
#' absorption features, inflection points, and subtle spectral variations that
#' may be obscured in the original reflectance data.
#'
#' @details
#' The spectral derivative quantifies how quickly reflectance changes with
#' wavelength. Three methods are available:
#' - "central": Central difference method, \code{[f(x+h1) - f(x-h2)]/(h1+h2)}
#' - "forward": Forward difference method, \code{[f(x+h) - f(x)]/h}
#' - "backward": Backward difference method, \code{[f(x) - f(x-h)]/h}
#'
#' Where h, h1, and h2 are wavelength differences between bands.
#' The derivative provides information about the rate of change in reflectance,
#' which can be useful for identifying absorption features and inflection points.
hsi_calc_derivative <- function(
  x,
  band,
  index_name = NULL,
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
    names = index_name
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
    wavelength_position(wavelength = band) |>
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

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

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
#' @param bands Numeric vector of length 2. The two wavelengths (in nm) to use
#'   for NDI calculation
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a Normalized Difference Index (NDI), which normalizes the difference
#' between two bands by their sum. This approach is widely used in remote sensing
#' (e.g., NDVI, NDWI) as it reduces the effects of illumination and viewing
#' geometry while highlighting spectral contrasts.
#'
#' @return A terra SpatRaster with ndi values
#' @export
#'
#' @description calculate normalized difference index
hsi_calc_ndi <- function(
  x,
  bands,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate bands
  if (!is.numeric(bands) || length(bands) != 2) {
    cli::cli_abort(
      "{.arg bands} must be a numeric vector of length 2."
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = bands) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  result <- (terra::subset(x, edge_positions[1]) -
    terra::subset(x, edge_positions[2])) /
    (terra::subset(x, edge_positions[1]) +
      terra::subset(x, edge_positions[2]))

  # Set name
  if (!is.null(index_name)) {
  names(result) <- index_name
  }

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
#' rgb_stretched <- hsi_stretch(
#' hyperspectral_raster,
#' type = "RGB")
#'
#' # Using custom wavelengths
#' custom_stretched <- hsi_stretch(
#' hyperspectral_raster,
#' type = c(400, 500, 600))
#'
#' # Save to file with histogram equalization
#' hsi_stretch(
#' x = hyperspectral_raster,
#' type = "CIR",
#' histeq = TRUE,
#' filename = "cir_stretched.tif", overwrite = TRUE)
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
  band_positions <- HSItools::wavelength_position(
    x,
    wavelength = spectra
  )

  selected_bands <- HSItools::wavelength_sub(
    x = x,
    wavelength_tbl = band_positions
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
#' @param hsi_data A terra SpatRaster with hyperspectral sample data. Band names must be
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
hsi_normalize <- function(
  hsi_data,
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
  hsi_data_matrix <- terra::as.matrix(hsi_data, wide = TRUE)

  # Subtract the dark reference from the capture matrix for each column
  numerator <- sweep(hsi_data_matrix, 2, darkref_onecol_vector, FUN = "-")

  # Subtract the dark reference from the white reference for each column
  denominator <- whiteref_onecol_vector - darkref_onecol_vector

  f_tint <- tint[1] / tint[2]

  # Divide the numerator by the denominator for each column and multiply by the tint factor
  result <- sweep(numerator, 2, denominator, "/") * f_tint

  # Set the result to NA if the denominator is lower than 0
  result[is.na(result) | result < 0] <- 0

  # Create a temporary raster to store the result
  result <- terra::init(
    hsi_data,
    t(result),
    filename = tempfile(fileext = ".tif"),
    wopt = list(gdal = c("COMPRESS=NONE"))
  )

  # Return SpatRaster
  return(result)
}

#' Hyperspectral reflectance raster
#'
#' @param x A terra SpatRaster with raw hyperspectral sample data. Band names
#'   must be numeric wavelengths in nm
#' @param whiteref A terra SpatRaster with hyperspectral white reference data.
#'   Must have same bands and wavelengths as \code{x}
#' @param darkref A terra SpatRaster with hyperspectral dark reference data.
#'   Must have same bands and wavelengths as \code{x}
#' @param tint Numeric vector of length 2. Integration times for white reference
#'   and sample data (in this order). Default c(1, 1) assumes equal integration times
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Convert raw hyperspectral imaging data (digital numbers) to calibrated
#' reflectance values using white and dark reference measurements. This is
#' the essential first step in hyperspectral data processing.
#'
#' @details
#' Reflectance calibration normalizes raw sensor values using reference
#' measurements to produce comparable reflectance data.
#'
#' **Important**: All three inputs (sample, white reference, dark reference)
#' must have:
#' - Same spatial resolution
#' - Same number of bands
#' - Same wavelength labels
#' - Compatible spatial extents (vertical stacking is allowed)
#'
#' @return A terra SpatRaster with normalized reflectance values.
#' @export
hsi_calc_reflectance <- function(
  x,
  whiteref,
  darkref,
  tint = c(1, 1),
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Processing logic by Jakub Nowosad - add to contributors at some point.
  # Needs cleanup
  # IMPORTANT Needs to properly handle temporary files, otherwise it clogs up the drive almost imediately

  # Validate inputs are SpatRasters
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  if (!inherits(whiteref, "SpatRaster")) {
    cli::cli_abort("Input {.arg whiteref} must be a terra SpatRaster.")
  }

  if (!inherits(darkref, "SpatRaster")) {
    cli::cli_abort("Input {.arg darkref} must be a terra SpatRaster.")
  }

  # Validate tint parameter
  if (!is.numeric(tint) || length(tint) != 2) {
    cli::cli_abort(
      c(
        "{.arg tint} must be a numeric vector of length 2.",
        "i" = "Format: c(white_integration_time, sample_integration_time)"
      )
    )
  }

  if (any(tint <= 0)) {
    cli::cli_abort("Integration times in {.arg tint} must be positive values.")
  }

  # Check that all inputs have the same number of bands
  n_bands_x <- terra::nlyr(x)
  n_bands_white <- terra::nlyr(whiteref)
  n_bands_dark <- terra::nlyr(darkref)

  if (n_bands_x != n_bands_white || n_bands_x != n_bands_dark) {
    cli::cli_abort(
      c(
        "All inputs must have the same number of bands.",
        "x" = "Sample: {n_bands_x} band{?s}",
        "x" = "White reference: {n_bands_white} band{?s}",
        "x" = "Dark reference: {n_bands_dark} band{?s}"
      )
    )
  }

  # Check that band names match
  bands_x <- terra::names(x)
  bands_white <- terra::names(whiteref)
  bands_dark <- terra::names(darkref)

  if (!identical(bands_x, bands_white) || !identical(bands_x, bands_dark)) {
    cli::cli_alert_warning(
      "Band names don't match across inputs. Proceeding with band-by-band processing."
    )
  }

  # # Store user input in a spliceable list -> probably not needed
  wopt_user <- rlang::list2(...)

  # # Extract band names
  # band_names <- terra::names(x)

  # # Named list with write options -> probably not needed
  wopt_default <- list(
    # names = band_names
  )

  # # Splice wopt defaults with user input if any -> probably not needed
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Perform normalization
  # In memory
  result <- list(
    hsi_data = terra::as.list(x),
    whiteref = terra::as.list(whiteref),
    darkref = terra::as.list(darkref),
    tint = list(tint)
  ) |>
    purrr::pmap(\(hsi_data, whiteref, darkref, tint) {
      hsi_normalize(
        hsi_data = hsi_data,
        whiteref = whiteref,
        darkref = darkref,
        tint = tint
      )
    }) |>
    terra::rast()

  # If saving to file, pass to writeRaster with user options
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

#' Spectral raster smooth with a Savitzky-Golay filter
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param p Integer. Filter polynomial order (typically 2-4)
#' @param n Integer. Filter length/window size (must be odd, typically 5-15)
#' @param m Integer. Derivative order (0 = smoothing, 1 = first derivative, etc.)
#' @param ts Numeric. Sampling interval for derivative calculations
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param cores Positive integer. Number of cores for parallel processing
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Smooth hyperspectral data using a Savitzky-Golay filter via \code{\link[gsignal]{sgolayfilt}}.
#' This filter fits successive sub-sets of adjacent data points with a low-degree polynomial
#' by the method of linear least squares.
#'
#' @details
#' The Savitzky-Golay filter is a spectral smoothing technique that preserves features
#' of the spectral curve such as peak height and width, which are usually flattened
#' by other smoothing methods. The filter works by fitting a polynomial of order \code{p}
#' through a moving window of \code{n} points.
#'
#' Note: Any pixels with NA values will result in function failure.
#'
#' @return A terra SpatRaster with Savitzky-Golay filtered values
#' @export
hsi_smooth_savgol <- function(
  x,
  p = 3,
  n = p + 13 - p %% 2,
  m = 0,
  ts = 1,
  filename = "",
  overwrite = FALSE,
  cores = 1,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Check if gsignal is available
  if (!requireNamespace("gsignal", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gsignal} is required for Savitzky-Golay filtering.",
      "i" = "Install with: {.code install.packages('gsignal')}"
    )
  }

  # Validate arguments
  if (n %% 2 != 1) {
    cli::cli_abort("Filter length {.arg n} must be odd, got {n}.")
  }

  if (p >= n) {
    cli::cli_abort(
      "Filter order {.arg p} must be less than filter length {.arg n}."
    )
  }

  if (p < 0 || n < 0 || m < 0) {
    cli::cli_abort("Filter parameters must be non-negative.")
  }

  # Check for sufficient bands
  if (terra::nlyr(x) < n) {
    cli::cli_abort(
      "SpatRaster has {terra::nlyr(x)} bands but filter length is {n}.",
      "i" = "Reduce filter length or use a raster with more bands."
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

  # Apply Savitzky-Golay filter
  # Note: as.vector() is required for gsignal::sgolayfilt (not needed for previous approach with signal::sgolayfilt)
  result <- terra::app(
    x,
    fun = \(x) gsignal::sgolayfilt(as.vector(x), p = p, n = n, m = m, ts = ts),
    filename = filename,
    overwrite = overwrite,
    cores = cores,
    wopt = wopt
  )

  # Return raster
  return(result)
}
