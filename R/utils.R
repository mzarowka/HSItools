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
#' @family Utilities
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param wavelength_tbl A [tibble][tibble::tibble] with wavelength positions
#'   as returned by [`wavelength_position()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] subset to the bands at
#'   the positions in `wavelength_tbl`.
#'
#' @export
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

#' Merge SpatRasters in a stratigraphic order
#'
#' @family Utilities
#' @param x a terra SpatRaster. First in the sequence.
#' @param y a terra SpatRaster. Second in the sequence.
#' @param filename Character. Output filename. Default "" keeps in memory
#'
#' @return a terra SpatRaster. Merged inputs.
#' @export
merge_rasters <- function(x, y, filename = "") {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Validate input
  if (!inherits(y, "SpatRaster")) {
    cli::cli_abort("Input {.arg y} must be a terra SpatRaster.")
  }

  # Get extent of the first SpatRaster
  extent_1 <- terra::ext(x)

  # Get extent of the second SpatRaster
  extent_2 <- terra::ext(y)

  # Shift second extent
  # xmin and xmax stay the same
  extent_2 <- terra::ext(
    extent_2[1],
    extent_2[2],
    extent_1[3] - (extent_2[4] - extent_2[3]),
    extent_1[3]
  )

  # Update extent of second SpatRaster
  terra::ext(y) <- extent_2

  # Get merged SpatRaster
  raster <- terra::merge(x, y, filename = filename)

  # Return
  return(raster)
}

#' Find a fixed-width extent in the middle of a larger one
#'
#' @family Utilities
#'
#' @param e A [`SpatExtent`][terra::SpatExtent-class] in which to search.
#'   Created with [`terra::ext()`].
#' @param width Numeric. Width in pixels of the new extent.
#'
#' @returns A [`SpatExtent`][terra::SpatExtent-class] centered within `extent`
#'   and of the specified width.
#'
#' @export
find_fixed_extent <- function(e, width) {
  # Validate input
  if (!inherits(e, "SpatExtent")) {
    cli::cli_abort("Input {.arg e} must be a terra SpatExtent.")
  }

  # Validate input
  check_numeric(width, len = 1, positive = TRUE)

  # Get mid point
  middle_point <- terra::round((terra::xmax(e) - terra::xmin(e)) / 2)

  # New xmin
  ext.xmin <- terra::xmin(e) + middle_point - (width / 2)

  # New xmax
  ext.xmax <- terra::xmin(e) + middle_point + (width / 2)

  # Update extent
  e <- terra::ext(ext.xmin, ext.xmax, e[3], e[4])

  # Return
  e
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
