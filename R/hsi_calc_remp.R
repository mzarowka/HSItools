#' Calculate lambdaREMP
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param search_range Numeric. Vector of two for the wide calculation window. Default c(660, 680)
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster with that many cores is created and used. You can also supply a cluster object
#' @param index_name Character. Name of calculated index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
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
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate lambda REMP with default settings
#' x_remp <- hsi_calc_remp(
#'  x
#' )
#'
#' # Save to file
#' x_remp <- hsi_calc_rabd(
#'  x,
#'  index_name = "remp",
#'  filename = "output_remp.tif",
#'  overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_remp <- function(
  x,
  search_range = c(660, 680),
  cores = 1,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
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
      lambda_remp <- max(min(lambda_remp, search_range[2]), search_range[1])

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
