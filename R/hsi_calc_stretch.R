#' Stretch selected bands to an RGB preview
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Band names must be numeric wavelengths in nm.
#' @param type Character or numeric. A predefined band combination
#'   (`"RGB"`, `"CIR"`, `"NIR"`, `"SWIR"`) or a numeric vector of exactly 3
#'   wavelengths in nm.
#' @param tol Numeric. Wavelength tolerance for band matching in nm. Default `25`.
#' @param histeq Logical. Use histogram equalization instead of linear stretch. Default `FALSE`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with 3 stretched bands.
#'
#' @description
#' Subset a hyperspectral raster to three bands and apply a linear or
#' histogram-equalized stretch. Supports predefined band combinations and
#' custom wavelength selection.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_stretch <- hsi_calc_stretch(x, type = "RGB")
#'
#' x_stretch <- hsi_calc_stretch(x, type = c(400, 500, 600))
#'
#' x_stretch <- hsi_calc_stretch(
#'   x,
#'   type = "CIR",
#'   histeq = TRUE,
#'   filename = "output_stretch.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_stretch <- function(
  x,
  type,
  tol = 25,
  histeq = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate input
  check_numeric(tol, positive = TRUE)

  # Validate and process the type argument
  if (is.character(type) && length(type) == 1) {
    # Predefined band combinations
    spectra <- switch(
      type,
      RGB = c(650, 550, 450),
      NIR = c(900, 800, 700),
      CIR = c(860, 650, 555),
      SWIR = c(1650, 1100, 2200),
      cli::cli_abort(
        "Unknown band type: {.val {type}}",
        i = "Use one of: RGB, NIR, CIR, SWIR, or provide numeric wavelengths."
      )
    )
  } else if (is.numeric(type)) {
    # Custom wavelengths
    if (length(type) != 3) {
      cli::cli_abort(
        "Custom wavelengths must provide exactly 3 values, got {length(type)}."
      )
    }
    spectra <- type
  } else {
    cli::cli_abort(
      "{.arg type} must be either a character string (e.g., 'RGB') or a numeric vector of 3 wavelengths."
    )
  }

  # Band names should always be the wavelengths (as character)
  band_names <- as.character(spectra)

  # Check if all required bands exist
  available_bands <- as.numeric(names(x))

  # Check each band individually
  band_exists <- purrr::map_lgl(spectra, \(target_wl) {
    any(dplyr::near(target_wl, available_bands, tol = tol))
  })

  if (!all(band_exists)) {
    missing_bands <- spectra[!band_exists]
    cli::cli_abort(
      c(
        "Cannot find all required bands within tolerance of {tol} nm.",
        x = "Missing bands near: {missing_bands} nm",
        i = "Available bands: {sort(available_bands)} nm"
      )
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Subset SpatRaster
  selected_bands <- hsi_subset(x, spectra)

  # Perform stretching
  result <- terra::stretch(
    selected_bands,
    histeq = histeq
  )

  names(result) <- band_names

  # Write to file if requested
  if (filename != "") {
    result <- terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  result
}
