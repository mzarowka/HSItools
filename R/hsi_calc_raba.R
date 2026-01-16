#' Calculate Relative Absorption Band Area (RABA)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param continuum_edges Numeric vector of length 2. Wavelength boundaries
#'   (in nm) that define the continuum for the calculation window
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param index_name Character. Name of calculated RABA index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
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
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate RABA for chlorophyll-a (typical range 650-700 nm)
#' x_raba <- hsi_calc_raba(
#'  x,
#'  continuum_edges = c(650, 700)
#' )
#'
#' # Save to file and provide a name
#' x_raba <- hsi_calc_raba(
#'   x = reflectance,
#'   continuum_edges = c(650, 700),
#'   index_name = "raba_650700",
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
  cores = 1,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate bands
  check_numeric(continuum_edges, len = 2)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = continuum_edges) |>
    dplyr::pull(2)

  # Get all bands between edges (including edges)
  band_range <- seq(
    from = min(edge_positions),
    to = max(edge_positions)
  )

  # Extract wavelengths in the range
  range_wavelengths <- terra::names(x)[band_range]

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
