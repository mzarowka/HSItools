#' Calculate band difference
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param bands Numeric vector of length 2. Wavelengths in nm.
#' @param index_name Character. Name for the output layer. Default `NULL`.
#' @param filename  Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with band difference values.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_difference <- hsi_calc_difference(
#'   x,
#'   bands = c(620, 680)
#' )
#'
#' x_difference <- hsi_calc_difference(
#'   x,
#'   bands = c(620, 680),
#'   index_name = "diff620680",
#'   filename = "output_difference.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_difference <- function(
  x,
  bands,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate bands
  check_numeric(bands, len = 2)

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Find band positions
  band_positions <- wavelength_position(x = x, wavelength = bands) |>
    dplyr::pull(var = 2)

  # Calculate difference
  result <- terra::subset(x, band_positions[1]) -
    terra::subset(x, band_positions[2])

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  }

  # Write to file if requested
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
