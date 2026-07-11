#' Calculate lambdaREMP
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with the first
#'   derivative of reflectance. Compute with [`hsi_smooth_savgol(x, m = 1)`][hsi_smooth_savgol].
#' @param search_range Numeric vector of length 2. Wavelength range in nm to
#'   search for the red-edge minimum point. Default `c(660, 680)`.
#' @param index_name Character. Name for the output layer. Default `NULL`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with lambdaREMP values (wavelength in nm).
#'
#' @description
#' Calculate lambda REMP (Red-Edge Minimum Point), the wavelength at which the
#' first derivative of reflectance crosses zero within a defined search range.
#'
#' @details
#' Lambda REMP is the inflection point where reflectance transitions from
#' decreasing to increasing — typically between 660–680 nm — and is sensitive
#' to chlorophyll-a concentration.
#'
#' The algorithm:
#' 1. Subsets the derivative raster to `search_range`.
#' 2. Identifies zero-crossings using [`gsignal::zerocrossing()`].
#' 3. Uses linear interpolation to find the exact wavelength where the
#'    derivative equals zero.
#' 4. Falls back to the wavelength nearest zero if no crossing is found.
#'
#' @references
#' Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023.
#' A new index for the rapid generation of chlorophyll time series from
#' hyperspectral imaging of sediment cores. Limnology and Oceanography:
#' Methods 21, 703-717. \doi{10.1002/lom3.10576}
#'
#' @seealso
#' [`hsi_smooth_savgol()`] for computing the derivative input,
#' [`hsi_subset_range()`] for wavelength range extraction.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_deriv <- hsi_smooth_savgol(x, m = 1)
#'
#' x_remp <- hsi_calc_remp(x_deriv)
#'
#' x_remp <- hsi_calc_remp(x_deriv, search_range = c(665, 690))
#'
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
      ),
      class = "hsitools_error"
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
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

  # Return
  result
}
