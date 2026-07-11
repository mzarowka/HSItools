#' Extract averaged spectrum from hyperspectral raster
#'
#' @family HSI Extraction
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Band names must be numeric wavelengths in nm.
#' @param fun Character. Aggregation function passed to [`terra::aggregate()`].
#'   Default `"mean"`. Other [`terra`][terra::terra-package] summary functions
#'   are also supported.
#'
#' @returns A [tibble][tibble::tibble] with columns:
#'   \item{wavelength}{Numeric. Wavelength in nm.}
#'   \item{value}{Numeric. Aggregated reflectance value.}
#'
#' @description
#' Aggregate a hyperspectral raster to a single spectrum representing the
#' mean (or other summary) across all pixels. Useful for examining
#' representative spectral signatures of regions of interest.
#'
#' @details
#' Returns one value per wavelength band. To extract a spectrum from a
#' specific region, crop the raster first with [`terra::crop()`].
#'
#' @seealso
#' [`hsi_extract_profile()`] for extracting values along a spatial axis.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' spectrum <- hsi_extract_spectrum(x)
#'
#' spectrum_roi <- x |>
#'   terra::crop(my_extent) |>
#'   hsi_extract_spectrum()
#'
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
      ),
      class = "hsitools_error"
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
    tibble::as_tibble()

  # Return
  spectrum
}
