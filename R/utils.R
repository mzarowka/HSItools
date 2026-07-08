#' Find position of selected wavelengths
#'
#' @family Utilities
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param wavelength Numeric vector. Desired wavelengths in nm.
#'
#' @returns A [tibble][tibble::tibble] with columns:
#'   \item{wavelength}{Numeric. Requested wavelength in nm.}
#'   \item{position}{Integer. Corresponding band index in `x`.}
#'   \item{band_wavelength}{Numeric. Actual wavelength of the matched band in nm.}
#'
#' @description
#' Find band index positions by matching requested wavelengths to the nearest
#' available band. When multiple requested wavelengths resolve to the same band
#' index, only the last is retained.
#'
#' @examples
#' \dontrun{
#' r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 5)
#' names(r) <- c("400", "500", "600", "700", "800")
#'
#' wavelength_position(r, c(450, 650))
#' }
#'
#' @export
wavelength_position <- function(
  x,
  wavelength
) {
  # Validate input
  check_spatraster(x)

  # Validate type
  check_numeric(wavelength)

  # Check if there is at least one layer
  if (length(wavelength) == 0) {
    cli::cli_abort("{.arg wavelength} must not be empty.")
  }

  # Check wavelengths
  band_wavelengths <- check_wavelengths(x)

  # Find index (position) of selected wavelength by comparing choice and names
  wavelength_index <- purrr::map_int(
    wavelength,
    \(i) terra::which.min(abs(i - band_wavelengths))
  )

  # Create tibble with wavelength of choice and respective position
  wavelength_table <- dplyr::tibble(
    wavelength = wavelength,
    position = wavelength_index,
    band_wavelength = band_wavelengths[wavelength_index]
  ) |>
    # Keep last observation if there are duplicates
    dplyr::slice_tail(by = "position")

  # Return
  wavelength_table
}

#' Subset SpatRaster by wavelength
#'
#' @noRd
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param wavelength_tbl A [tibble][tibble::tibble] with wavelength positions
#'   as returned by [`wavelength_position()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] subset to the bands at
#'   the positions in `wavelength_tbl`.
#'
wavelength_sub <- function(
  x,
  wavelength_tbl
) {
  # Validate input
  check_spatraster(x)

  # Validate input
  if (!inherits(wavelength_tbl, "data.frame")) {
    cli::cli_abort(
      "Input {.arg wavelength_tbl} must be a data frame or tibble."
    )
  }

  # Get positions from tibble
  position <- dplyr::pull(wavelength_tbl, position)

  # Subset raster by position
  raster <- terra::subset(x, position)

  # Return
  raster
}

#' Subset SpatRaster by wavelength
#'
#' @family Utilities
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Band names must be numeric wavelengths in nm.
#' @param wavelength Numeric vector. Wavelength(s) to extract in nm. Nearest
#'   available band is selected for each value.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] subset to the requested
#'   wavelength(s).
#'
#' @examples
#' \dontrun{
#' x |> hsi_subset(675)
#'
#' x |> hsi_subset(c(650, 550, 450))
#'
#' x |>
#'   hsi_smooth_savgol(m = 1) |>
#'   hsi_subset(675)
#' }
#'
#' @export
hsi_subset <- function(
  x,
  wavelength,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  if (!is.numeric(wavelength) || length(wavelength) == 0) {
    cli::cli_abort("{.arg wavelength} must be a non-empty numeric vector.")
  }

  # Find and subset
  result <- wavelength_position(x, wavelength) |>
    wavelength_sub(x = x, wavelength_tbl = _)

  # Write if requested
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      ...
    )
  }

  # Return
  result
}

#' Subset SpatRaster by wavelength range
#'
#' @family Utilities
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#'   Band names must be numeric wavelengths in nm.
#' @param from Numeric. Start wavelength of the range in nm, inclusive.
#' @param to Numeric. End wavelength of the range in nm, inclusive.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with all bands within
#'   the specified wavelength range.
#'
#' @examples
#' \dontrun{
#' x |> hsi_subset_range(from = 660, to = 680)
#'
#' x |>
#'   hsi_smooth_savgol(m = 1) |>
#'   hsi_subset_range(from = 680, to = 750)
#' }
#'
#' @export
hsi_subset_range <- function(
  x,
  from,
  to,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate input
  check_numeric(from, len = 1, positive = TRUE)

  # Validate input
  check_numeric(to, len = 1, positive = TRUE)

  # Get wavelengths from band names
  wavelengths <- as.numeric(terra::names(x))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(terra::names(x), 5)}}..."
      )
    )
  }

  # Find bands within range (handle inverted from/to)
  range_min <- min(from, to)
  range_max <- max(from, to)

  indices <- which(wavelengths >= range_min & wavelengths <= range_max)

  if (length(indices) == 0) {
    cli::cli_abort(
      c(
        "No bands found in range {range_min}-{range_max} nm.",
        "i" = "Available range: {min(wavelengths)}-{max(wavelengths)} nm"
      )
    )
  }

  # Subset
  result <- terra::subset(x, indices)

  # Write if requested
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      ...
    )
  }

  # Return
  result
}

#' Convert units to micrometers
#'
#' @param value Numeric. Value to convert.
#' @param from Character. Source units. One of `"um"`, `"mm"`, or `"cm"`.
#'
#' @returns Numeric. Value in micrometers.
#'
#' @noRd
to_um <- function(value, from) {
  # Get the multiplier
  multiplier <- switch(
    from,
    "um" = 1,
    "mm" = 1000,
    "cm" = 10000,
    # Abort if none of the above is supplied
    cli::cli_abort(
      "{.val {from}} is not a supported unit. Use {.or {.val {c('um', 'mm', 'cm')}}}."
    )
  )

  # Get the correct value
  value <- value * multiplier

  # Return
  value
}

#' Convert micrometers to target units
#'
#' @param value Numeric. Value in micrometers.
#' @param to Character. Target units. One of `"um"`, `"mm"`, or `"cm"`.
#'
#' @returns Numeric. Value in target units.
#'
#' @noRd
from_um <- function(value, to) {
  # Get the multiplier
  multiplier <- switch(
    to,
    "um" = 1,
    "mm" = 0.001,
    "cm" = 0.0001,
    # Abort if none of the above is supplied
    cli::cli_abort(
      "{.val {to}} is not a supported unit. Use {.or {.val {c('um', 'mm', 'cm')}}}."
    )
  )

  # Get the correct value
  value <- value * multiplier

  # Return
  value
}


#' Construct an hsi_metadata list, unvalidated
#'
#' @param name Character. Capture name. A single non-empty string.
#' @param sensor_type Character. Sensor type. Default `NULL`.
#' @param manufacturer Character. Sensor manufacturer. Default `NULL`.
#' @param session_id Character. Session identifier grouping scans that share a white reference. Default `NULL`.
#' @param nrow Positive integer. Number of raster rows. Default `NULL`.
#' @param ncol Positive integer. Number of raster columns. Default `NULL`.
#' @param nlyr Positive integer. Number of raster layers. Default `NULL`.
#' @param xres Positive number. Pixel resolution in the x direction. Default `NULL`.
#' @param yres Positive number. Pixel resolution in the y direction. Default `NULL`.
#' @param spectral_resolution_nm Positive number. Spectral resolution in nm. Default `NULL`.
#' @param frame_rate_hz Positive number. Frame rate in Hz. Default `NULL`.
#' @param et_target_ms Positive number. Target integration time in ms. Default `NULL`.
#' @param et_white_ms Positive number. White reference integration time in ms. Default `NULL`.
#' @param target_start_mm Positive number. Motor position at scan start in mm. Default `NULL`.
#' @param target_stop_mm Positive number. Motor position at scan end in mm. Default `NULL`.
#' @param spectral_binning Positive integer. Spectral binning factor. Default `NULL`.
#' @param spatial_binning Positive integer. Spatial binning factor. Default `NULL`.
#' @param wavelengths Positive numeric vector. Band centre wavelengths in nm, one value per layer. Default `NULL`.
#' @param fwhm Positive numeric vector. Band full width at half maximum in nm, one value per layer. Default `NULL`.
#'
#' @returns An object of class `hsi_metadata`: an unvalidated list of capture
#'   metadata fields with `schema_version` stamped. Validation happens
#'   separately via `validate_hsi_metadata()`.
#'
#' @noRd
new_hsi_metadata <- function(
  name,
  sensor_type = NULL,
  manufacturer = NULL,
  session_id = NULL,
  nrow = NULL,
  ncol = NULL,
  nlyr = NULL,
  xres = NULL,
  yres = NULL,
  spectral_resolution_nm = NULL,
  frame_rate_hz = NULL,
  et_target_ms = NULL,
  et_white_ms = NULL,
  target_start_mm = NULL,
  target_stop_mm = NULL,
  spectral_binning = NULL,
  spatial_binning = NULL,
  wavelengths = NULL,
  fwhm = NULL
) {
  # Define list and class
  structure(
    list(
      schema_version = "1.0.0",
      name = name,
      sensor_type = sensor_type,
      manufacturer = manufacturer,
      session_id = session_id,
      nrow = nrow,
      ncol = ncol,
      nlyr = nlyr,
      xres = xres,
      yres = yres,
      spectral_resolution_nm = spectral_resolution_nm,
      frame_rate_hz = frame_rate_hz,
      et_target_ms = et_target_ms,
      et_white_ms = et_white_ms,
      target_start_mm = target_start_mm,
      target_stop_mm = target_stop_mm,
      spectral_binning = spectral_binning,
      spatial_binning = spatial_binning,
      wavelengths = wavelengths,
      fwhm = fwhm
    ),
    class = "hsi_metadata"
  )
}

#' Validate structured hyperspectral metadata
#'
#' @param x An object of class `hsi_metadata` to validate.
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns `x`, invisibly, if validation passes; aborts otherwise.
#'
#' @noRd
validate_hsi_metadata <- function(x, call = rlang::caller_env()) {
  # Required string
  rlang::check_string(
    x$name,
    allow_empty = FALSE,
    arg = "name",
    call = call
  )

  # Optional strings
  rlang::check_string(
    x$sensor_type,
    allow_null = TRUE,
    arg = "sensor_type",
    call = call
  )
  rlang::check_string(
    x$manufacturer,
    allow_null = TRUE,
    arg = "manufacturer",
    call = call
  )
  rlang::check_string(
    x$session_id,
    allow_null = TRUE,
    arg = "session_id",
    call = call
  )

  # Optional positive scalars
  check_numeric(
    x$nrow,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "nrow",
    call = call
  )
  check_numeric(
    x$ncol,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "ncol",
    call = call
  )
  check_numeric(
    x$nlyr,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "nlyr",
    call = call
  )
  check_numeric(
    x$xres,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "xres",
    call = call
  )
  check_numeric(
    x$yres,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "yres",
    call = call
  )
  check_numeric(
    x$spectral_resolution_nm,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "spectral_resolution_nm",
    call = call
  )
  check_numeric(
    x$frame_rate_hz,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "frame_rate_hz",
    call = call
  )
  check_numeric(
    x$et_target_ms,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "et_target_ms",
    call = call
  )
  check_numeric(
    x$et_white_ms,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "et_white_ms",
    call = call
  )
  check_numeric(
    x$target_start_mm,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "target_start_mm",
    call = call
  )
  check_numeric(
    x$target_stop_mm,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "target_stop_mm",
    call = call
  )
  check_numeric(
    x$spectral_binning,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "spectral_binning",
    call = call
  )
  check_numeric(
    x$spatial_binning,
    len = 1,
    positive = TRUE,
    allow_null = TRUE,
    arg = "spatial_binning",
    call = call
  )
  check_numeric(
    x$wavelengths,
    allow_null = TRUE,
    positive = TRUE,
    arg = "wavelengths",
    call = call
  )
  check_numeric(
    x$fwhm,
    allow_null = TRUE,
    positive = TRUE,
    arg = "fwhm",
    call = call
  )

  # Wavelengths length must match nlyr
  if (
    !is.null(x$wavelengths) &&
      !is.null(x$nlyr) &&
      length(x$wavelengths) != x$nlyr
  ) {
    cli::cli_abort(
      c(
        "{.arg wavelengths} must have one value per layer.",
        "i" = "{.arg nlyr} is {x$nlyr}, but {.arg wavelengths} has length {length(x$wavelengths)}."
      ),
      class = "hsitools_error",
      call = call
    )
  }

  # FWHM length must match nlyr
  if (
    !is.null(x$fwhm) &&
      !is.null(x$nlyr) &&
      length(x$fwhm) != x$nlyr
  ) {
    cli::cli_abort(
      c(
        "{.arg fwhm} must have one value per layer.",
        "i" = "{.arg nlyr} is {x$nlyr}, but {.arg fwhm} has length {length(x$fwhm)}."
      ),
      class = "hsitools_error",
      call = call
    )
  }

  # Return result
  invisible(x)
}
