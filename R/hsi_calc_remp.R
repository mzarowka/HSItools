#' Calculate lambdaREMP
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with first derivative of reflectance.
#'   Calculate using \code{hsi_smooth_savgol(reflectance, m = 1)}
#' @param search_range Numeric vector of length 2. Wavelength range to search
#'   for the red-edge minimum point. Default c(660, 680)
#' @param cores Positive integer. If cores > 1, a \pkg{parallel} cluster with
#'   that many cores is created and used.
#' @param index_name Character. Name of calculated index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with lambdaREMP values (wavelength in nm)
#'
#' @description
#' Calculate lambda REMP (wavelength of the Red-Edge Minimum Point), which
#' identifies the wavelength where the first derivative of reflectance equals
#' zero, indicating maximum chlorophyll absorption.
#'
#' @details
#' Lambda REMP identifies the wavelength between approximately 660-680 nm where
#' the first derivative of reflectance crosses from negative to positive
#' (the inflection point where reflectance transitions from decreasing
#' to increasing). This wavelength is sensitive to chlorophyll-a concentration.
#'
#' The algorithm:
#' 1. Subsets derivative raster to the search range
#' 2. Identifies zero-crossings using \code{\link[gsignal]{zerocrossing}}
#' 3. Uses linear interpolation to find the exact wavelength where derivative = 0
#' 4. Falls back to wavelength nearest zero if no crossing is found
#'
#' @references
#' Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023.
#' A new index for the rapid generation of chlorophyll time series from
#' hyperspectral imaging of sediment cores. Limnology and Oceanography:
#' Methods 21, 703-717. \doi{10.1002/lom3.10576}
#'
#' @seealso
#' \code{\link{hsi_smooth_savgol}} for calculating the derivative input,
#' \code{\link{hsi_subset_range}} for wavelength range extraction
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate first derivative (do once, reuse)
#' x_deriv <- hsi_smooth_savgol(x, m = 1)
#'
#' # Calculate lambda REMP
#' x_remp <- hsi_calc_remp(x_deriv)
#'
#' # Custom search range
#' x_remp <- hsi_calc_remp(x_deriv, search_range = c(665, 690))
#'
#' # Save to file
#' x_remp <- hsi_calc_remp(
#'   x_deriv,
#'   index_name = "remp",
#'   filename = "output_remp.tif",
#'   overwrite = TRUE
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
  check_spatraster(x)

  # Validate input
  check_numeric(search_range, len = 2)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Subset to search range
  x_range <- hsi_subset_range(
    x,
    from = search_range[1],
    to = search_range[2]
  )

  # Get wavelengths in the range
  range_wavelengths <- as.numeric(terra::names(x_range))

  if (length(range_wavelengths) < 2) {
    cli::cli_abort(
      c(
        "Not enough bands in search range ({search_range[1]}-{search_range[2]} nm).",
        "i" = "Need at least 2 bands for zero-crossing detection."
      )
    )
  }

  # Find zero-crossing for each pixel
  find_zero_crossing <- function(deriv_values) {
    # Handle NA values
    if (anyNA(deriv_values)) {
      return(NA_real_)
    }

    # gsignal returns interpolated wavelengths at zero crossings
    crossings <- gsignal::zerocrossing(range_wavelengths, deriv_values)

    if (length(crossings) > 0) {
      # Filter for negative-to-positive crossings
      neg_to_pos <- crossings[
        purrr::map_lgl(crossings, \(wl) {
          # Index just before the crossing
          idx <- max(which(range_wavelengths < wl))
          # Check: negative before, positive after
          deriv_values[idx] <= 0 && deriv_values[idx + 1] > 0
        })
      ]

      if (length(neg_to_pos) > 0) {
        return(neg_to_pos[1])
      }
    }

    # Fallback: wavelength closest to zero
    min_idx <- which.min(abs(deriv_values))
    return(range_wavelengths[min_idx])
  }

  # Apply to each pixel
  result <- terra::app(
    x_range,
    fun = find_zero_crossing,
    cores = cores,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

  return(result)
}
