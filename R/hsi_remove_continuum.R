#' Remove continuum from hyperspectral data
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
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
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate continuum removed reflectance
#' x_crem <- hsi_remove_continuum(x)
#'
#' # Save to file
#' x_crem <- remove_continuum(
#'  x,
#'  filename = "output_crem.tif",
#'  overwrite = TRUE)
#' }
#' 
#' @export
hsi_remove_continuum <- function(
  x,
  cores = 1,
  filename = "",
  overwrite = FALSE,
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

    # Return
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
