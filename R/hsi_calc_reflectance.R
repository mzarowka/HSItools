#' Normalize hyperspectral raster
#'
#' @noRd
#'
#' @param specimen A [`SpatRaster`][terra::SpatRaster-class] with a single band
#'   of raw hyperspectral sample data.
#' @param whiteref A [`SpatRaster`][terra::SpatRaster-class] with a single band
#'   of white reference data.
#' @param darkref_num A [`SpatRaster`][terra::SpatRaster-class] with a single
#'   band of dark reference data for the numerator subtraction (specimen side).
#'   Already scaled by the caller if needed.
#' @param darkref_den A [`SpatRaster`][terra::SpatRaster-class] with a single
#'   band of dark reference data for the denominator subtraction (white
#'   reference side).
#' @param f_tint Numeric scalar. Integration time ratio `tint_white / tint_specimen`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with normalized reflectance values.
hsi_normalize <- function(
  specimen,
  whiteref,
  darkref_num,
  darkref_den,
  f_tint,
  filename = ""
) {
  # Get the average value of the white reference for each column
  whiteref_onecol_raster <- terra::aggregate(
    whiteref,
    fact = c(terra::nrow(whiteref), 1),
    fun = "mean"
  )

  # Store in a vector
  whiteref_onecol_vector <- as.vector(whiteref_onecol_raster)

  # Get the average value of the numerator dark reference for each column
  darknum_onecol_raster <- terra::aggregate(
    darkref_num,
    fact = c(terra::nrow(darkref_num), 1),
    fun = "mean"
  )

  # Store in a vector
  darknum_onecol_vector <- as.vector(darknum_onecol_raster)

  # Get the average value of the denominator dark reference for each column
  darkden_onecol_raster <- terra::aggregate(
    darkref_den,
    fact = c(terra::nrow(darkref_den), 1),
    fun = "mean"
  )

  # Store in a vector
  darkden_onecol_vector <- as.vector(darkden_onecol_raster)

  # Convert the raster to a matrix
  specimen_matrix <- terra::as.matrix(specimen, wide = TRUE)

  # Subtract numerator dark from specimen
  numerator <- sweep(specimen_matrix, 2, darknum_onecol_vector, FUN = "-")

  # Subtract denominator dark from white reference
  denominator <- whiteref_onecol_vector - darkden_onecol_vector

  # Divide and apply integration time correction
  result <- sweep(numerator, 2, denominator, "/") * f_tint

  # Set the result to NA if the denominator is zero or negative
  result[, denominator <= 0] <- NA

  # Create temporary SpatRaster with results
  result <- terra::init(specimen, t(result), filename = filename)

  # Return result
  result
}

#' Calculate hyperspectral reflectance
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with raw
#'   hyperspectral sample data. Band names must be numeric wavelengths in nm.
#' @param whiteref A [`SpatRaster`][terra::SpatRaster-class] with white
#'   reference data. Must have the same bands and wavelengths as `x`.
#' @param darkref A [`SpatRaster`][terra::SpatRaster-class] with dark
#'   reference data from the white reference session. Must have the same bands
#'   and wavelengths as `x`.
#' @param darkspec A [`SpatRaster`][terra::SpatRaster-class] with dark
#'   reference data from the specimen session. Default `NULL`. Required for
#'   dual-exposure workflows where `tint` values differ. When `NULL` and
#'   `tint = c(1, 1)`, a single dark reference is sufficient.
#' @param tint Numeric vector of length 2. Integration times for white
#'   reference and specimen capture, in that order. Default `c(1, 1)` assumes
#'   equal integration times.
#' @param in_memory Logical. Process entirely in RAM. Default `FALSE`.
#'   Set `TRUE` only when data fits comfortably in available memory.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with reflectance values.
#'
#' @description
#' Convert raw hyperspectral imaging data (digital numbers) to calibrated
#' reflectance values using white and dark reference measurements. This is
#' the essential first step in hyperspectral data processing.
#'
#' @details
#' All inputs must share the same spatial resolution, number of bands,
#' wavelength labels, and compatible spatial extents. When reading `.raw` ESRI
#' data, load with `terra::rast(x, noflip = TRUE)`.
#'
#' Three calibration paths are supported:
#'
#' **Single session** (`darkspec = NULL`, `tint = c(1, 1)`): specimen, white
#' reference, and dark reference all share the same integration time. No
#' scaling is needed. This is the simplest workflow and produces correct
#' reflectance, though signal-to-noise is lower than with a dual-exposure
#' strategy.
#'
#' **Matched darks** (`darkspec` provided): a dual-exposure workflow where the
#' specimen is overexposed relative to the white reference to maximise signal.
#' Each subtraction uses the dark reference captured at the matching
#' integration time. This is the recommended approach for dual-exposure
#' scanning. Many scanners capture a dark reference per session, so matched
#' darks are typically available for standard workflows.
#'
#' \deqn{R(\lambda) = \frac{specimen - dark_{specimen}}{white - dark_{white}}
#'   \times \frac{t_{white}}{t_{specimen}}}
#'
#' **Scaled dark** (`darkspec = NULL`, `tint` values differ): fallback for
#' dual-exposure workflows when only the white-session dark reference is
#' available. The dark reference is scaled by the integration time ratio
#' before numerator subtraction. This assumes dark current scales linearly
#' with integration time. In practice, some detectors have a large
#' fixed-pattern noise component that does not scale with exposure time.
#' Scaling overestimates the specimen dark current, producing severely
#' degraded reflectance — often negative across entire spectra. Use only as
#' a last resort.
#'
#' \deqn{R(\lambda) = \frac{specimen - dark_{white} \times
#'   \frac{t_{specimen}}{t_{white}}}{white - dark_{white}} \times
#'   \frac{t_{white}}{t_{specimen}}}
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("capture/testdata.tif")
#' whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
#' darkref <- terra::rast("capture/DARKREF_testdata.tif")
#'
#' # Path 1: single session, equal integration times
#' x_reflectance <- hsi_calc_reflectance(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref
#' )
#'
#' # Path 2a: matched darks (recommended)
#' darkspec <- terra::rast("specimen/DARKREF_testdata.tif")
#'
#' x_reflectance <- hsi_calc_reflectance(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   darkspec = darkspec,
#'   tint = c(3, 9)
#' )
#'
#' # Path 2b: scaled dark (single dark, different integration times)
#' x_reflectance <- hsi_calc_reflectance(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   tint = c(3, 9),
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
  darkspec = NULL,
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
      ),
      class = "hsitools_error"
    )
  }

  # Validate darkspec if provided
  if (!is.null(darkspec)) {
    check_spatraster(darkspec)

    n_bands_darkspec <- terra::nlyr(darkspec)

    if (n_bands_x != n_bands_darkspec) {
      cli::cli_abort(
        c(
          "Specimen dark reference must have the same number of bands as {.arg x}.",
          "x" = "Sample: {n_bands_x} band{?s}",
          "x" = "Specimen dark reference: {n_bands_darkspec} band{?s}"
        ),
        class = "hsitools_error"
      )
    }
  }

  # Validate band names are numeric wavelengths
  wavelengths <- suppressWarnings(as.numeric(names(x)))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(names(x), 5)}}..."
      ),
      class = "hsitools_error"
    )
  }

  # Check that band names match
  bands_x <- names(x)

  bands_white <- names(whiteref)

  bands_dark <- names(darkref)

  if (!identical(bands_x, bands_white) || !identical(bands_x, bands_dark)) {
    cli::cli_warn(
      "Band names don't match across inputs. Proceeding with band-by-band processing.",
      class = "hsitools_warning"
    )
  }

  # Route dark reference for numerator subtraction
  if (!is.null(darkspec)) {
    # Path 2a: matched darks — no scaling needed
    darkref_num_list <- terra::as.list(darkspec)
    dark_scale <- 1
  } else {
    # Path 1 or 2b: single dark for both
    darkref_num_list <- terra::as.list(darkref)
    dark_scale <- tint[2] / tint[1]

    if (dark_scale != 1) {
      cli::cli_warn(
        c(
          "Scaling dark reference by integration time ratio ({.val {dark_scale}}).",
          "i" = "For best accuracy, provide {.arg darkspec} from the specimen session."
        ),
        class = "hsitools_warning"
      )
    }
  }

  f_tint <- tint[1] / tint[2]

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
      ),
      class = "hsitools_warning"
    )
  }

  # Perform normalization
  result <- list(
    specimen = terra::as.list(x),
    whiteref = terra::as.list(whiteref),
    darkref_num = darkref_num_list,
    darkref_den = terra::as.list(darkref),
    filename = temp_paths
  ) |>
    purrr::pmap(\(specimen, whiteref, darkref_num, darkref_den, filename) {
      # Scale numerator dark per-band if needed (path 2b)
      if (dark_scale != 1) {
        darkref_num <- darkref_num * dark_scale
      }

      hsi_normalize(
        specimen = specimen,
        whiteref = whiteref,
        darkref_num = darkref_num,
        darkref_den = darkref_den,
        f_tint = f_tint,
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
