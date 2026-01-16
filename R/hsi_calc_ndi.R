#' Calculate normalized difference index (NDI)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param bands Numeric vector of length 2. The two wavelengths (in nm) to use
#'   for NDI calculation
#' @param index_name Character. Name of calculated ratio. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a Normalized Difference Index (NDI), which normalizes the difference
#' between two bands by their sum. This approach is widely used in remote sensing
#' (e.g., NDVI, NDWI) as it reduces the effects of illumination and viewing
#' geometry while highlighting spectral contrasts.
#'
#' @return A terra SpatRaster with NDI values
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate NDI between 570 and 690
#' x_ndi <- hsi_calc_ndi(
#'   x,
#'   bands = c(570, 690)
#' )
#'
#' # Save to file and provide a name
#' x_ndi <- hsi_calc_ndi(
#'   x,
#'   bands = c(570, 690),
#'   index_name = "ndi570690",
#'   filename = "output_ndi.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_ndi <- function(
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

  # Get bands

  band1 <- terra::subset(x, band_positions[1])
  band2 <- terra::subset(x, band_positions[2])

  # Calculate NDI: (band1 - band2) / (band1 + band2)
  result <- (band1 - band2) / (band1 + band2)

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
