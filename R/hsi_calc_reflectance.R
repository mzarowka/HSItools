#' Normalize hyperspectral raster
#'
#' @noRd
#'
#' @param hsi_data A [`SpatRaster`][terra::SpatRaster-class] with raw
#'   hyperspectral sample data. Band names must be numeric wavelengths in nm.
#' @param whiteref A [`SpatRaster`][terra::SpatRaster-class] with white
#'   reference data. Must have the same bands and wavelengths as `hsi_data`.
#' @param darkref  A [`SpatRaster`][terra::SpatRaster-class] with dark
#'   reference data. Must have the same bands and wavelengths as `hsi_data`.
#' @param tint Numeric vector of length 2. Integration times for white
#'   reference and sample capture, in that order.
#' @param in_memory Logical. Process entirely in RAM. Default `FALSE`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with normalized reflectance values.
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
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with raw
#'   hyperspectral sample data. Band names must be numeric wavelengths in nm.
#' @param whiteref A [`SpatRaster`][terra::SpatRaster-class] with white
#'   reference data. Must have the same bands and wavelengths as `x`.
#' @param darkref A [`SpatRaster`][terra::SpatRaster-class] with dark
#'   reference data. Must have the same bands and wavelengths as `x`.
#' @param tint Numeric vector of length 2. Integration times for white
#'   reference and sample capture, in that order. Default `c(1, 1)` assumes
#'   equal integration times.
#' @param in_memory Logical. Process entirely in RAM. Default `FALSE`.
#'   Set `TRUE` only when data fits comfortably in available memory. When
#'   `FALSE`, writes one temporary file per band.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with normalized reflectance values.
#'
#' @description
#' Convert raw hyperspectral imaging data (digital numbers) to calibrated
#' reflectance values using white and dark reference measurements. This is
#' the essential first step in hyperspectral data processing.
#'
#' @details
#' All three inputs must share the same spatial resolution, number of bands,
#' wavelength labels, and compatible spatial extents. When reading `.raw` ESRI
#' data, load with `terra::rast(x, noflip = TRUE)`.
#'
#' When using an external white reference captured at a different integration
#' time, supply a dark reference from the same session. With SWIR data in
#' particular, using a dark reference from a different capture can produce
#' negative reflectance where an oversaturated dark reference exceeds the
#' white reference signal.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("capture/testdata.tif")
#' whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
#' darkref <- terra::rast("capture/DARKREF_testdata.tif")
#'
#' x_reflectance <- hsi_calc_reflectance(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   tint = c(1, 1)
#' )
#'
#' x_reflectance <- hsi_calc_reflectance(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   tint = c(1, 1),
#'   filename = "output_reflectance.tif",
#'   overwrite = TRUE
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
  # Needs cleanup
  # IMPORTANT Needs to properly handle temporary files, otherwise it clogs up the drive almost imediately

  # Validate input
  check_spatraster(x)

  check_spatraster(whiteref)

  check_spatraster(darkref)

  # Validate tint
  check_numeric(tint, len = 2, positive = TRUE)

  # Check that all inputs have the same number of bands
  n_bands_x <- terra::nlyr(x)

  n_bands_white <- terra::nlyr(whiteref)

  n_bands_dark <- terra::nlyr(darkref)

  # Check number of bands, must be equal in all inputs
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

  # Named list with write options -> probably not needed
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

  # Return
  result
}
