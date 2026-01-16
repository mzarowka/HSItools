#' Stretch and optionally save full RGB preview of SpatRaster
#'
#' Performs stretching on selected bands from a hyperspectral SpatRaster.
#' Supports both predefined band combinations and custom wavelength selection.
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data. Band names must be
#'   numeric wavelengths in nm.
#' @param type Character or numeric. Either a predefined band combination
#'   ("RGB", "CIR", "NIR", "SWIR") or a numeric vector of exactly 3
#'   wavelengths in nm (e.g., c(400, 500, 600))
#' @param tol Numeric. Tolerance for band selection in nm (default: 25)
#' @param histeq Logical. If TRUE histogram equalization is used instead of
#'   linear stretch (default: FALSE)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A SpatRaster with 3 bands after stretching
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Using predefined band combination of RGB c(650, 550, 450)
#' x_rgb <- hsi_calc_stretch(
#'  x,
#'  type = "RGB")
#'
#' # Using custom wavelengths
#' x_rgb_custom <- hsi_calc_stretch(
#'  x,
#'  type = c(400, 500, 600))
#'
#' # Save to file with histogram equalization
#'  x_rgb <- hsi_calc_stretch(
#'  x,
#'  type = "CIR",
#'  histeq = TRUE,
#'  filename = "output_cir.tif",
#'  overwrite = TRUE)
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

  # Validate tolerance
  if (!is.numeric(tol) || length(tol) != 1 || tol < 0) {
    cli::cli_abort("{.arg tol} must be a single non-negative numeric value.")
  }

  # Check if all required bands exist
  available_bands <- as.numeric(terra::names(x))

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

  # Find band positions and subset
  band_positions <- HSItools::wavelength_position(
    x,
    wavelength = spectra
  )

  selected_bands <- HSItools::wavelength_sub(
    x = x,
    wavelength_tbl = band_positions
  )

  # Perform stretching
  if (filename != "") {
    # If saving to file, pass writeRaster options
    result <- terra::stretch(
      selected_bands,
      histeq = histeq,
      filename = filename,
      overwrite = overwrite,
      names = band_names,
      ...
    )
  } else {
    # If keeping in memory
    result <- terra::stretch(
      selected_bands,
      histeq = histeq
    )
    names(result) <- band_names
  }

  # Return stretched SpatRaster
  return(result)
}
