#' Extract averaged spectrum from hyperspectral raster
#'
#' @family HSI Extraction
#'
#' @param x A terra SpatRaster with hyperspectral data. Band names must be
#'   numeric wavelengths in nm.
#' @param fun Character. Aggregation function passed to \code{\link[terra]{aggregate}}.
#'   Default "mean". Other \pkg{terra} functions are also supported
#'
#' @return A tibble with columns:
#'   \item{wavelength}{Numeric. Wavelength in nm}
#'   \item{value}{Numeric. Aggregated reflectance value}
#'
#' @description
#' Aggregate a hyperspectral raster to a single spectrum representing the
#' average (or other summary) across all pixels. Useful for examining
#' representative spectral signatures of regions of interest.
#'
#' @details
#' The function aggregates all pixels in the input raster using the specified
#' function, returning one value per wavelength band. If you need a spectrum
#' from a specific region, crop the raster first with \code{\link[terra]{crop}}.
#'
#' @seealso
#' \code{\link{hsi_extract_profile}} for extracting values along an axis.
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Extract mean spectrum from full raster
#' spectrum <- hsi_extract_spectrum(x)
#'
#' # Extract from specific region
#' spectrum_roi <- x |>
#'   terra::crop(my_extent) |>
#'   hsi_extract_spectrum()
#'
#' # Use median instead of mean
#' spectrum_median <- hsi_extract_spectrum(x, fun = "median")
#' }
#'
#' @export
hsi_extract_spectrum <- function(
  x,
  fun = "mean"
) {
  # Validate input
  check_spatraster(x)

  # Validate band names are numeric wavelengths
  wavelengths <- suppressWarnings(as.numeric(terra::names(x)))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(terra::names(x), 5)}}..."
      )
    )
  }

  # Aggregate entire raster to single pixel
  spectrum <- terra::aggregate(
    x,
    fact = c(terra::nrow(x), terra::ncol(x)),
    fun = fun,
    na.rm = TRUE
  ) |>
    # Coerce to data frame
    terra::as.data.frame(xy = FALSE) |>
    # Pivot to long format
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "wavelength",
      names_transform = as.numeric,
      values_to = "value"
    ) |>
    # Ensure tibble
    dplyr::tibble()

  # Return
  return(spectrum)
}
