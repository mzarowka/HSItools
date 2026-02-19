#' Find position of selected wavelengths
#'
#' @family Utilities
#' @param x A terra SpatRaster with hyperspectral data
#' @param wavelength Numeric vector of desired wavelengths
#'
#' @return A tibble with columns:
#'   - `wavelength`: the requested wavelengths
#'   - `position`: the corresponding band indices in the SpatRaster
#'
#' @export
#'
#' @description Find index position of the nearest wavelength (band) in the dataset
#' by matching the smallest difference between requested and actual wavelengths.
#' If multiple wavelengths map to the same band, only the last is kept.
#'
#' @examples
#' \dontrun{
#' # Create example raster
#' r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 5)
#' names(r) <- c("400", "500", "600", "700", "800")
#'
#' # Find positions
#' wavelength_position(r, c(450, 650))
#' }
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

  # Return values
  return(wavelength_table)
}


#' Subset SpatRaster by wavelength
#'
#' @family Utilities
#'
#' @param x A terra SpatRaster to be subset
#' @param wavelength_tbl a tibble with wavelength positions from wavelength_position.
#'
#' @return SpatRaster subset to contain only required wavelengthl bands.
#' @export
#'
#' @description Subset SpatRaster using wavelength (band) positions from a lookup table.
#'
#' @description subset SpatRaster with wavelength (bands) positions.
wavelength_sub <- function(
  x,
  wavelength_tbl
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

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
  return(raster)
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
#' @param extent a terra SpatExtent in which to look, created with terra::ext()
#' @param width number of pixels of a new SpatExtent.
#'
#' @return a terra SpatExtent centered within the original SpatExtent and of a given width.
#' @export
find_fixed_extent <- function(extent, width) {
  # Check if correct class is supplied.
  # Validate input
  if (!inherits(extent, "SpatExtent")) {
    cli::cli_abort("Input {.arg extent} must be a terra SpatExtent.")
  }

  # Get mid point
  middle_point <- terra::round((terra::xmax(extent) - terra::xmin(extent)) / 2)

  # New xmin
  ext.xmin <- terra::xmin(extent) + middle_point - (width / 2)

  # New xmax
  ext.xmax <- terra::xmin(extent) + middle_point + (width / 2)

  # Update extent
  extent <- terra::ext(ext.xmin, ext.xmax, extent[3], extent[4])

  # Return
  return(extent)
}

#' Subset SpatRaster by wavelength
#'
#' @family Utilities
#'
#' @param x A terra SpatRaster with hyperspectral data. Band names must be
#'   numeric wavelengths in nm.
#' @param wavelength Numeric. Wavelength(s) to extract. Nearest available
#'   band(s) will be selected.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster subset to the requested wavelength(s)
#'
#' @description
#' Extract band(s) by wavelength value. Finds the nearest available band
#' for each requested wavelength.
#'
#' @examples
#' \dontrun{
#' # Single band
#' x |> hsi_subset(675)
#'
#' # Multiple bands
#' x |> hsi_subset(c(650, 550, 450))
#'
#' # Derivative at specific wavelength
#' x |> hsi_smooth_savgol(m = 1) |>
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
  return(result)
}

#' Subset SpatRaster by wavelength range
#'
#' @family Utilities
#'
#' @param x A terra SpatRaster with hyperspectral data. Band names must be
#'   numeric wavelengths in nm.
#' @param from Numeric. Start wavelength of range (inclusive)
#' @param to Numeric. End wavelength of range (inclusive)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with all bands within the specified range
#'
#' @description
#' Extract all bands within a wavelength range. Both boundaries are inclusive.
#'
#' @examples
#' \dontrun{
#' # Extract chlorophyll absorption region
#' x |> hsi_subset_range(from = 660, to = 680)
#'
#' # Derivative in red-edge region
#' x |>
#'   hsi_smooth_savgol(m = 1) |>
#'   hsi_subset_range(from = 680, to = 750)
#'
#' # VNIR only
#' x |> hsi_subset_range(from = 400, to = 1000)
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
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  if (!is.numeric(from) || length(from) != 1) {
    cli::cli_abort("{.arg from} must be a single numeric value.")
  }

  if (!is.numeric(to) || length(to) != 1) {
    cli::cli_abort("{.arg to} must be a single numeric value.")
  }

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

  # Return the result
  return(result)
}


#' Convert units to micrometers
#'
#' @param value Numeric. Value to convert.
#' @param from Character. Source units.
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
#' @param value Numeric. Value to convert.
#' @param to Character. Target units.
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
