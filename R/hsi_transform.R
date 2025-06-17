#' Remove continuum from spectrum
#'
#' @family Filters
#' @param raster terra SpatRaster of normalized capture data.
#' @param filename a path to save file (with extension). Defaultys to NULL and processing in memory.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return one layer terra SpatRaster with continuum removed.
#' @export
hsi_continuum <- function(
  raster,
  filename = NULL,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Validate required packages
  if (!requireNamespace("prospectr", quietly = TRUE)) {
    rlang::abort("Package 'prospectr' is required for continuum removal.")
  }

  # Extract names
  band_names <- terra::names(raster)

  # Named list with write options
  wopts <- list(
    names = band_names
  )

  # Get wavelengths
  wavelengths <- suppressWarnings(as.numeric(band_names))

  # If wavelengths couldn't be converted, create a sequence
  if (all(is.na(wavelengths))) {
    wavelengths <- seq_along(band_names)
  }

  # Continuum removal function
  remove_continuum_fun <- function(x) {
    # Skip NA values
    if (any(is.na(x))) return(rep(NA, length(x)))

    # For a single pixel, transpose the data structure
    X_matrix <- matrix(x, nrow = 1) # 1 sample (pixel) with multiple wavelengths as columns

    # Apply continuum removal - expects wavelengths and reflectance values
    # Note: prospectr::continuumRemoval returns only the CR values
    cr_result <- prospectr::continuumRemoval(X = X_matrix, wav = wavelengths)

    return(as.vector(cr_result))
  }

  # Apply function over entire SpatRaster
  raster <- terra::app(
    raster,
    fun = remove_continuum_fun,
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

  # Return raster
  return(raster)
}