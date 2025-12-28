#' Normalize hyperspectral raster
#'
#' @family HSI Transformations
#' @param hsi_data A terra SpatRaster with hyperspectral sample data. Band names must be
#'   numeric wavelengths in nm.
#' @param whiteref A terra SpatRaster with hyperspectral white reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param darkref A terra SpatRaster with hyperspectral dark reference data. Band names must be
#'   numeric wavelengths in nm.
#' @param tint A vector of two with integration times for white reference and sample data (in this order).
#' @param in_memory Logical. Should processing be done in memory.
#'
#' @details
#' Normalizes a SpatRaster in respect to white and dark references.
#'
#' @return A temporary terra SpatRaster with normalized reflectance values.
hsi_normalize <- function(
  hsi_data,
  whiteref,
  darkref,
  tint,
  in_memory = FALSE
) {
  # Get the average value of the white reference for each column
  whiteref_onecol_raster <- terra::aggregate(
    whiteref,
    fact = c(terra::nrow(whiteref), 1),
    fun = "mean"
  )

  # Store it in a vector
  whiteref_onecol_vector <- as.vector(whiteref_onecol_raster)

  # Get the average value of the dark reference for each column
  darkref_onecol_raster <- terra::aggregate(
    darkref,
    fact = c(terra::nrow(darkref), 1),
    fun = "mean"
  )

  # Store it in a vector
  darkref_onecol_vector <- as.vector(darkref_onecol_raster)

  # Convert the raster to a matrix
  hsi_data_matrix <- terra::as.matrix(hsi_data, wide = TRUE)

  # Subtract the dark reference from the capture matrix for each column
  numerator <- sweep(hsi_data_matrix, 2, darkref_onecol_vector, FUN = "-")

  # Subtract the dark reference from the white reference for each column
  denominator <- whiteref_onecol_vector - darkref_onecol_vector

  f_tint <- tint[1] / tint[2]

  # Divide the numerator by the denominator for each column and multiply by the tint factor
  result <- sweep(numerator, 2, denominator, "/") * f_tint

  # Set the result to NA if the denominator is lower than 0
  result[is.na(result) | result < 0] <- 0

  # Create a temporary raster to store the result
  if (!in_memory) {
    result <- terra::init(
      hsi_data,
      t(result),
      filename = tempfile(fileext = ".tif"),
      wopt = list(gdal = c("COMPRESS=NONE"))
    )
  } else {
    result <- terra::init(
      hsi_data,
      t(result)
    )
  }

  # Return SpatRaster
  return(result)
}

#' Hyperspectral reflectance raster
#'
#' @param x A terra SpatRaster with raw hyperspectral sample data. Band names
#'   must be numeric wavelengths in nm
#' @param whiteref A terra SpatRaster with hyperspectral white reference data.
#'   Must have same bands and wavelengths as \code{x}
#' @param darkref A terra SpatRaster with hyperspectral dark reference data.
#'   Must have same bands and wavelengths as \code{x}
#' @param tint Numeric vector of length 2. Integration times for white reference
#'   and sample data (in this order). Default c(1, 1) assumes equal integration times
#' @param in_memory Logical. Should processing be done in memory (default: FALSE). Consider size of data and system specs. If FALSE (default) it will write as many temporary files as there are layers in the data. If TRUE, it will process in RAM.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Convert raw hyperspectral imaging data (digital numbers) to calibrated
#' reflectance values using white and dark reference measurements. This is
#' the essential first step in hyperspectral data processing.
#'
#' @details
#' Reflectance calibration normalizes raw sensor values using reference
#' measurements to produce comparable reflectance data.
#'
#' **Important**: All three inputs (sample, white reference, dark reference)
#' must have:
#' - Same spatial resolution
#' - Same number of bands
#' - Same wavelength labels
#' - Compatible spatial extents (vertical stacking is allowed)
#' - If reading .raw ESRI data use terra::rast(x, noflip = TRUE)
#' - If you are using external white reference with different integration time, also use darkreference from the same session. Especially with SWIR use of darkref from actual data capture might lead to negative values where oversaturated darkref is higher than undersaturated whiteref
#'
#' @return A terra SpatRaster with normalized reflectance values.
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("capture/testdata.tif")
#' whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
#' darkref <- terra::rast("capture/DARKREF_testdata.tif")
#'
#' # Optionally crop to extent of choice
#'
#' # Calculate reflectance
#' x_reflectance <- hsi_calc_reflectance(
#'  x = x,
#'  whiteref = whiteref,
#'  darkref = darkref,
#'  tint = c(1, 1)
#' )
#'
#' # Save to file
#' x_reflectance <- hsi_calc_reflectance(
#'  x = x,
#'  whiteref = whiteref,
#'  darkref = darkref,
#'  tint = c(1, 1),
#'  filename = "output.tif",
#'  overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_reflectance <- function(
  x,
  whiteref,
  darkref,
  tint = c(1, 1),
  in_memory = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Processing logic by Jakub Nowosad - add to contributors at some point.
  # Needs cleanup
  # IMPORTANT Needs to properly handle temporary files, otherwise it clogs up the drive almost imediately

  # Validate inputs are SpatRasters
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  if (!inherits(whiteref, "SpatRaster")) {
    cli::cli_abort("Input {.arg whiteref} must be a terra SpatRaster.")
  }

  if (!inherits(darkref, "SpatRaster")) {
    cli::cli_abort("Input {.arg darkref} must be a terra SpatRaster.")
  }

  # Validate tint parameter
  if (!is.numeric(tint) || length(tint) != 2) {
    cli::cli_abort(
      c(
        "{.arg tint} must be a numeric vector of length 2.",
        "i" = "Format: c(white_integration_time, sample_integration_time)"
      )
    )
  }

  if (any(tint <= 0)) {
    cli::cli_abort("Integration times in {.arg tint} must be positive values.")
  }

  # Check that all inputs have the same number of bands
  n_bands_x <- terra::nlyr(x)
  n_bands_white <- terra::nlyr(whiteref)
  n_bands_dark <- terra::nlyr(darkref)

  if (n_bands_x != n_bands_white || n_bands_x != n_bands_dark) {
    cli::cli_abort(
      c(
        "All inputs must have the same number of bands.",
        "x" = "Sample: {n_bands_x} band{?s}",
        "x" = "White reference: {n_bands_white} band{?s}",
        "x" = "Dark reference: {n_bands_dark} band{?s}"
      )
    )
  }

  # Check that band names match
  bands_x <- terra::names(x)
  bands_white <- terra::names(whiteref)
  bands_dark <- terra::names(darkref)

  if (!identical(bands_x, bands_white) || !identical(bands_x, bands_dark)) {
    cli::cli_alert_warning(
      "Band names don't match across inputs. Proceeding with band-by-band processing."
    )
  }

  # Store user input in a spliceable list -> probably not needed
  wopt_user <- rlang::list2(...)

  # # Named list with write options -> probably not needed
  wopt_default <- list(
    # names = band_names
  )

  # # Splice wopt defaults with user input if any -> probably not needed
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Perform normalization
  # In memory
  result <- list(
    hsi_data = terra::as.list(x),
    whiteref = terra::as.list(whiteref),
    darkref = terra::as.list(darkref),
    tint = list(tint)
  ) |>
    purrr::pmap(\(hsi_data, whiteref, darkref, tint) {
      hsi_normalize(
        hsi_data = hsi_data,
        whiteref = whiteref,
        darkref = darkref,
        tint = tint,
        in_memory = in_memory
      )
    }) |>
    terra::rast()

  # If saving to file, pass to writeRaster with user options
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return SpatRaster
  return(result)
}
