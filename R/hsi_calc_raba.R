#' Calculate Relative Absorption Band Area (RABA)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param continuum_edges Numeric vector of length 2. Wavelength boundaries
#'   (in nm) that define the continuum for the calculation window
#' @param index_name Character. Name of calculated RABA index. Default NULL
#' @param cores Positive integer. Number of parallel cores. Default `1`.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate Relative Absorption Band Area (RABA), which quantifies the total
#' absorption across a spectral feature as the area between the reflectance
#' spectrum and its linear continuum.
#'
#' @details
#' The continuum is linearly interpolated between the two continuum edges. The
#' signed area between the continuum and reflectance spectrum is then calculated
#' with the trapezoidal rule using the actual wavelength spacing.
#'
#' This approach:
#' - Uses the same continuum concept as \code{\link{hsi_calc_rabd}}
#' - Integrates across the entire absorption feature
#' - Accounts for unequal spacing between spectral bands
#' - Provides a measure of total absorption strength
#'
#' @return A terra SpatRaster with signed RABA values in reflectance-nm.
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
#'   x,
#'   continuum_edges = c(650, 700)
#' )
#'
#' # Save to file and provide a name
#' x_raba <- hsi_calc_raba(
#'   x = reflectance,
#'   continuum_edges = c(650, 700),
#'   index_name = "raba_650700",
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
  cores = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate continuum edges
  check_numeric(continuum_edges, len = 2)

  # Validate input
  check_numeric(cores, len = 1, positive = TRUE)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

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

  # Get all band indices between edges (inclusive)
  band_range <- seq(
    from = min(edge_positions),
    to = max(edge_positions)
  )

  # Subset to bands in range
  x_range <- terra::subset(x, band_range)

  # Get the wavelengths in the range
  range_wavelengths <- as.numeric(terra::names(x_range))
  wavelength_fraction <- (range_wavelengths - range_wavelengths[1]) /
    (range_wavelengths[length(range_wavelengths)] - range_wavelengths[1])
  band_widths <- diff(range_wavelengths)

  .calc_raba_pixel <- function(
    pixel_values,
    wavelength_fraction,
    band_widths
  ) {
    if (anyNA(pixel_values)) {
      return(NA_real_)
    }

    continuum <- pixel_values[1] +
      (pixel_values[length(pixel_values)] - pixel_values[1]) *
        wavelength_fraction

    absorption <- continuum - pixel_values
    last_band <- length(absorption)

    sum(
      band_widths *
        (absorption[-last_band] + absorption[-1]) /
        2
    )
  }

  # Apply function to each pixel
  result <- terra::app(
    x_range,
    fun = .calc_raba_pixel,
    wavelength_fraction = wavelength_fraction,
    band_widths = band_widths,
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
