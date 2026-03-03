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
#' its continuum value. `NA` values at spectral edges are expected and handled
#' downstream. Computationally intensive; consider applying to subsets or
#' regions of interest rather than full-resolution data. For full-raster
#' processing, [`hsi_tiled()`] can distribute the workload across parallel
#' workers.
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
  # Validate input
  check_spatraster(x)

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

    # Return
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
