#' Calculate band ratio
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param bands Numeric vector of length 2. The two wavelengths (in nm)
#'   to use for ratio calculation
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a band ratio index by dividing reflectance at one wavelength by
#' reflectance at another wavelength. Band ratios are commonly used to
#' normalize spectral data and highlight specific features such as clay
#' minerals or dust content.
#'
#' @return A terra SpatRaster with ratio values
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate band ratio between 570 and 690
#' x_ratio <- hsi_calc_ratio(
#'   x,
#'   bands = c(570, 690)
#' )
#'
#' # Save to file and provide a name
#' x_ratio <- hsi_calc_ratio(
#'   x,
#'   bands = c(570, 690),
#'   index_name = "ratio570690",
#'   filename = "output_ratio.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_ratio <- function(
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

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Find band positions
  band_positions <- wavelength_position(x = x, wavelength = bands) |>
    dplyr::pull(var = 2)

  # Calculate ratio
  result <- terra::subset(x, band_positions[1]) /
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
  return(result)
}
