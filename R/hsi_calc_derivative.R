#' Calculate spectral derivative
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param band Numeric. Wavelength (in nm) at which to calculate the derivative
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param method Character. method to use for derivative calculation. One of "central" (default), "forward", or "backward".
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with ratio values
#' @export
#'
#' @description
#' Calculate the spectral derivative (rate of change of reflectance with respect
#' to wavelength) at a specific wavelength. Derivatives are useful for identifying
#' absorption features, inflection points, and subtle spectral variations that
#' may be obscured in the original reflectance data.
#'
#' @details
#' The spectral derivative quantifies how quickly reflectance changes with
#' wavelength. Three methods are available:
#' - "central": Central difference method, \code{[f(x+h1) - f(x-h2)]/(h1+h2)}
#' - "forward": Forward difference method, \code{[f(x+h) - f(x)]/h}
#' - "backward": Backward difference method, \code{[f(x) - f(x-h)]/h}
#'
#' Where h, h1, and h2 are wavelength differences between bands.
#' The derivative provides information about the rate of change in reflectance,
#' which can be useful for identifying absorption features and inflection points.
hsi_calc_derivative <- function(
  x,
  band,
  index_name = NULL,
  method = "central",
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, what = "SpatRaster")) {
    cli::cli_abort("Input {.arg x} must be a terra SpatRaster.")
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Create empty SpatRaster template from original SpatRaster
  result <- terra::rast(
    terra::ext(x),
    resolution = terra::res(x)
  )

  # Validate method
  method <- match.arg(method, c("central", "forward", "backward"))

  # Get wavelengths from band names
  wavelengths <- x |>
    terra::names() |>
    as.numeric()

  # If wavelengths couldn't be converted, create a sequence
  if (all(is.na(wavelengths))) {
    cli::cli_alert_warning(
      "Band names couldn't be converted to wavelengths. Using band indices instead."
    )
    wavelengths <- seq_len(terra::nlyr(x))
  }

  # Find position of the requested band
  band_position <- x |>
    wavelength_position(wavelength = band) |>
    dplyr::pull(var = 2)

  # Validate band position is within range
  if (band_position < 1 || band_position > terra::nlyr(x)) {
    cli::cli_abort(
      message = paste0(
        "Requested band (",
        band,
        ") is outside the available range of ",
        "your raster (",
        min(wavelengths),
        "-",
        max(wavelengths),
        ")."
      )
    )
  }

  # Calculate derivative based on method
  if (method == "central") {
    if (band_position <= 1 || band_position >= terra::nlyr(x)) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'central' method for band at position ",
          band_position,
          ". Need bands before and after for central difference. ",
          "Try using 'forward' or 'backward' methods instead."
        )
      )
    }

    # Calculate using central difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_minus <- terra::subset(i, band_position - 1)
        band_plus <- terra::subset(i, band_position + 1)
        wave_diff <- wavelengths[band_position + 1] -
          wavelengths[band_position - 1]
        (band_plus - band_minus) / wave_diff
      })()
  } else if (method == "forward") {
    if (band_position >= terra::nlyr(x)) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'forward' method for the last band at position ",
          band_position,
          ". Try using 'backward' method instead."
        )
      )
    }

    # Calculate using forward difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_current <- terra::subset(i, band_position)
        band_plus <- terra::subset(i, band_position + 1)
        wave_diff <- wavelengths[band_position + 1] - wavelengths[band_position]
        (band_plus - band_current) / wave_diff
      })()
  } else if (method == "backward") {
    if (band_position <= 1) {
      cli::cli_abort(
        message = paste0(
          "Cannot use 'backward' method for the first band at position ",
          band_position,
          ". Try using 'forward' method instead."
        )
      )
    }

    # Calculate using backward difference
    result <- x |>
      (\(i) {
        # Extract bands and calculate difference
        band_current <- terra::subset(i, band_position)
        band_minus <- terra::subset(i, band_position - 1)
        wave_diff <- wavelengths[band_position] - wavelengths[band_position - 1]
        (band_current - band_minus) / wave_diff
      })()
  }

  # Set derivative values onto SpatRaster template
  # terra::values(result) <- derivative_values

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return
  return(result)
}