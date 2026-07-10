#' Remove continuum from hyperspectral data
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with continuum-removed values.
#'
#' @description
#' Normalize spectra by removing the spectral continuum to highlight absorption
#' features. The continuum is the convex hull connecting local maxima across
#' the spectrum. Requires the
#' [`prospectr`](https://CRAN.R-project.org/package=prospectr) package.
#'
#' @details
#' Output values are bounded `[0, 1]` by definition — each band is divided by
#' its continuum value.
#'
#' Degenerate spectra for which no valid continuum exists (e.g. cracks,
#' all-zero or constant spectra, pixels containing `NA`) return `NA` in all
#' bands instead of aborting the computation. Inspect the `NA` pattern of the
#' result if coverage looks unexpectedly sparse.
#'
#' Wavelengths are taken from band names. If band names cannot be converted
#' to numeric wavelengths, band indices are used instead and a warning is
#' issued.
#'
#' Computationally intensive; consider applying to subsets or regions of
#' interest rather than full-resolution data. For full-raster processing,
#' [`hsi_tiled()`] can distribute the workload across parallel workers.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_crem <- hsi_remove_continuum(x)
#'
#' x_crem <- hsi_remove_continuum(
#'   x,
#'   filename = "output_crem.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_remove_continuum <- function(
  x,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

  rlang::check_installed("prospectr")

  if (terra::nlyr(x) < 3) {
    cli::cli_abort(
      "Input raster must have at least 3 bands for continuum removal.",
      i = "Current raster has {terra::nlyr(x)} band{?s}.",
      class = "hsitools_error"
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Build write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get wavelengths from band names, fall back to band indices
  wavelengths <- suppressWarnings(as.numeric(band_names))

  if (all(is.na(wavelengths))) {
    cli::cli_warn(
      "Band names cannot be converted to wavelengths. Using band indices.",
      class = "hsitools_warning"
    )
    wavelengths <- seq_along(band_names)
  }

  # Continuum removal function
  remove_continuum_fun <- function(x) {
    # Skip pixels with no valid continuum
    if (anyNA(x) || all(x == 0)) {
      return(rep(NA_real_, length(x)))
    }

    # Convex hull construction can fail on degenerate spectra (e.g. cracks).
    # A per-pixel failure must not abort the whole computation, so map it to NA.
    tryCatch(
      {
        # One pixel = one sample row, wavelengths as columns
        X_matrix <- matrix(x, nrow = 1)

        as.vector(prospectr::continuumRemoval(X = X_matrix, wav = wavelengths))
      },
      error = \(e) rep(NA_real_, length(x))
    )
  }

  # Apply function over entire SpatRaster
  result <- terra::app(
    x,
    fun = remove_continuum_fun,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return result
  result
}
