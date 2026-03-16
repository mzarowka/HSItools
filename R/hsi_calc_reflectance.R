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
#'   reference and sample capture, in that order. The ratio `tint[2] / tint[1]`
#'   scales the dark reference before numerator subtraction to account for
#'   additional dark current accumulated during the longer specimen exposure.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with normalized reflectance values.
hsi_normalize <- function(
  hsi_data,
  whiteref,
  darkref,
  tint,
  filename = ""
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
  dark_scaled <- darkref_onecol_vector * (tint[2] / tint[1])
  numerator <- sweep(hsi_data_matrix, 2, dark_scaled, FUN = "-")

  # Subtract the dark reference from the white reference for each column
  denominator <- whiteref_onecol_vector - darkref_onecol_vector

  # Calculate tint fraction
  f_tint <- tint[1] / tint[2]

  # Divide the numerator by the denominator for each column and multiply by the tint factor
  result <- sweep(numerator, 2, denominator, "/") * f_tint

  # Set the result to NA if the denominator is lower than 0
  result[is.na(result) | result < 0] <- 0

  # Create temporary SpatRaster with results
  result <- terra::init(hsi_data, t(result), filename = filename)

  # Return result
  result
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
  # Validate inputs
  check_spatraster(x)

  check_spatraster(whiteref)

  check_spatraster(darkref)

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
  wavelengths <- suppressWarnings(as.numeric(names(x)))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(names(x), 5)}}..."
      )
    )
  }

  # Check that band names match
  bands_x <- names(x)

  bands_white <- names(whiteref)

  bands_dark <- names(darkref)

  if (!identical(bands_x, bands_white) || !identical(bands_x, bands_dark)) {
    cli::cli_warn(
      "Band names don't match across inputs. Proceeding with band-by-band processing."
    )
  }

  # Build write options
  wopt_user <- rlang::list2(...)

  wopt_default <- list(names = names(x))

  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # File handling logic
  if (in_memory) {
    # Generate all needed empty paths
    temp_paths <- rep("", terra::nlyr(x))
  } else if (filename != "") {
    # If working with files and writing to file
    # Create self-destructing tempdir
    temp_dir <- withr::local_tempdir()

    # Generate all needed file paths
    temp_paths <- file.path(
      temp_dir,
      paste0("band_", seq_len(terra::nlyr(x)), ".tif")
    )
  } else {
    # If working with files and not writing to file
    # An exception to the withr rule: when in_memory = FALSE and no filename is
    # provided, the temp files ARE the backing storage of the returned SpatRaster
    # and cannot be cleaned up before the caller is done with the object.
    # withr::local_tempdir() would delete them on function exit, orphaning the
    # SpatRaster. Plain tempfile() is intentional here.
    temp_dir <- tempfile()
    dir.create(temp_dir)

    temp_paths <- file.path(
      temp_dir,
      paste0("band_", seq_len(terra::nlyr(x)), ".tif")
    )

    cli::cli_warn(
      c(
        "{.arg in_memory} is {.val FALSE} but no {.arg filename} was provided.",
        "i" = "Temporary files will not be cleaned up until the R session ends. Files might persist."
      )
    )
  }

  # Perform normalization
  result <- list(
    hsi_data = terra::as.list(x),
    whiteref = terra::as.list(whiteref),
    darkref = terra::as.list(darkref),
    tint = list(tint),
    filename = temp_paths
  ) |>
    purrr::pmap(\(hsi_data, whiteref, darkref, tint, filename) {
      hsi_normalize(
        hsi_data = hsi_data,
        whiteref = whiteref,
        darkref = darkref,
        tint = tint,
        filename = filename
      )
    }) |>
    terra::rast()

  # Write to file
  if (filename != "") {
    result <- terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return result
  result
}
