#' Calibrate a three-band preview composite
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
#' @param type Character or numeric. A predefined band combination
#'   (`"RGB"`, `"CIR"`, `"NIR"`, `"SWIR"`) or a numeric vector of exactly 3
#'   wavelengths in nm.
#' @param tol Numeric. Wavelength tolerance for band matching in nm. Default `25`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with 3 reflectance bands,
#'   named by the wavelengths of the matched bands.
#'
#' @description
#' Calibrate only the three bands needed for a false-colour composite, instead
#' of converting an entire cube to reflectance and discarding all but three
#' bands afterwards. Intended for rapid visual inspection and markup.
#'
#' @details
#' The requested wavelengths are resolved against the band grid of `x` once,
#' and the resulting band indices are applied to `x`, `whiteref`, `darkref`,
#' and `darkspec` alike. Resolving each raster independently would allow
#' rounding differences between band labels to pull the specimen and its
#' references onto different bands, silently calibrating one wavelength
#' against another. All inputs must therefore share the same number of bands.
#'
#' Calibration is delegated to [`hsi_calc_reflectance()`] and follows the same
#' three paths, selected by `darkspec` and `tint`; see its documentation for
#' the formulas. Processing is forced in memory, since three bands are small
#' by construction.
#'
#' Bands are matched to the nearest available wavelength, but a match further
#' than `tol` from the request is an error rather than a silent substitution —
#' without it, asking for a `"SWIR"` composite of a VNIR capture would return
#' the nearest edge band three times over. The result is a reflectance raster,
#' not a stretched one; pipe it through [`hsi_calc_stretch()`] for display.
#'
#' @seealso
#' [`hsi_calc_reflectance()`] for calibrating a full cube.
#' [`hsi_calc_stretch()`] for stretching the result to a displayable range.
#' [`hsi_plot_raster_rgb()`] for rendering the composite.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("capture/testdata.tif")
#' whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
#' darkref <- terra::rast("capture/DARKREF_testdata.tif")
#'
#' x_preview <- hsi_calc_preview(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   type = "RGB"
#' )
#'
#' # Ready to display
#' x_preview |>
#'   hsi_calc_stretch(type = "RGB") |>
#'   hsi_plot_raster_rgb()
#'
#' # Custom wavelengths, matched darks, written to disk
#' darkspec <- terra::rast("specimen/DARKREF_testdata.tif")
#'
#' x_preview <- hsi_calc_preview(
#'   x = x,
#'   whiteref = whiteref,
#'   darkref = darkref,
#'   darkspec = darkspec,
#'   tint = c(3, 9),
#'   type = c(700, 620, 540),
#'   filename = "output_preview.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_preview <- function(
  x,
  whiteref,
  darkref,
  darkspec = NULL,
  tint = c(1, 1),
  type,
  tol = 25,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

  check_spatraster(whiteref)

  check_spatraster(darkref)

  check_spatraster(darkspec, allow_null = TRUE)

  check_numeric(tint, len = 2, positive = TRUE)

  # Resolve the band combination to three wavelengths
  if (is.character(type)) {
    rlang::check_string(type)

    check_one_of(type, c("RGB", "NIR", "CIR", "SWIR"))

    spectra <- switch(
      type,
      RGB = c(650, 550, 450),
      NIR = c(900, 800, 700),
      CIR = c(860, 650, 555),
      SWIR = c(1650, 1100, 2200)
    )
  } else if (is.numeric(type)) {
    check_numeric(type, len = 3, positive = TRUE)

    spectra <- type
  } else {
    cli::cli_abort(
      c(
        "{.arg type} must be a band combination name or three wavelengths.",
        "i" = "Got a {.cls {class(type)[[1]]}} instead."
      ),
      class = "hsitools_error"
    )
  }

  check_numeric(tol, len = 1, positive = TRUE)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Sharing band positions across inputs is only valid when they share a band
  # grid, so band counts must agree before anything is subset.
  n_bands <- terra::nlyr(x)

  references <- purrr::compact(list(
    whiteref = whiteref,
    darkref = darkref,
    darkspec = darkspec
  ))

  mismatched <- purrr::keep(references, \(i) terra::nlyr(i) != n_bands)

  if (length(mismatched) > 0) {
    cli::cli_abort(
      c(
        "Reference rasters must have the same number of bands as {.arg x}.",
        "i" = "{.arg x} has {n_bands} band{?s}.",
        "x" = "Mismatched: {.arg {names(mismatched)}}."
      ),
      class = "hsitools_error"
    )
  }

  # Resolve requested wavelengths against the band grid of x only, so every
  # input is subset by an identical set of band indices.
  wavelength_tbl <- wavelength_position(x, spectra)

  # wavelength_position() matches unconditionally to the nearest band, so a
  # combination outside the range of x would otherwise resolve to whichever
  # edge band happens to be closest rather than failing.
  off_target <- dplyr::filter(
    wavelength_tbl,
    abs(wavelength - band_wavelength) > tol
  )

  if (nrow(off_target) > 0) {
    cli::cli_abort(
      c(
        "Cannot find all requested bands within {tol} nm.",
        "x" = "No band near: {dplyr::pull(off_target, wavelength)} nm.",
        "i" = "{.arg x} covers {min(check_wavelengths(x))}-{max(check_wavelengths(x))} nm."
      ),
      class = "hsitools_error"
    )
  }

  # wavelength_position() also drops duplicate band indices, so wavelengths
  # spaced more finely than the bands of x collapse into fewer than three
  # layers instead of erroring.
  if (nrow(wavelength_tbl) != 3) {
    cli::cli_abort(
      c(
        "The three requested wavelengths must resolve to three distinct bands.",
        "x" = "{.val {spectra}} nm resolve to {nrow(wavelength_tbl)} band{?s}.",
        "i" = "The requested wavelengths are closer together than the band spacing of {.arg x}."
      ),
      class = "hsitools_error"
    )
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  check_dots_write(wopt_user, filename)

  # Subset every input by the same band positions
  x_bands <- wavelength_sub(x, wavelength_tbl)

  whiteref_bands <- wavelength_sub(whiteref, wavelength_tbl)

  darkref_bands <- wavelength_sub(darkref, wavelength_tbl)

  darkspec_bands <- if (is.null(darkspec)) {
    NULL
  } else {
    wavelength_sub(darkspec, wavelength_tbl)
  }

  # Three bands are small by construction, which is the point of the preview,
  # so calibration runs in memory rather than exposing in_memory to the caller.
  result <- hsi_calc_reflectance(
    x = x_bands,
    whiteref = whiteref_bands,
    darkref = darkref_bands,
    darkspec = darkspec_bands,
    tint = tint,
    in_memory = TRUE
  )

  # Name layers after the bands actually matched, not the ones requested
  band_names <- as.character(dplyr::pull(wavelength_tbl, band_wavelength))

  names(result) <- band_names

  # Build write options
  wopt_default <- list(names = band_names)

  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

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
