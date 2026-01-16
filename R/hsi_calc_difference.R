#' Calculate band difference
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param bands Numeric vector of length 2. The two wavelengths (in nm)
#'   to use for difference calculation
#' @param index_name Character. Name of calculated difference index. Default NULL
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Calculate a band difference index by subtracting reflectance at one wavelength
#' from reflectance at another wavelength. Band differences can highlight
#' spectral features and are commonly used to detect clay minerals, dust, and
#' other sedimentary components.
#'
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate band difference between 620 and 680
#' x_diff <- hsi_calc_difference(
#'  x,
#'  bands = c(620, 680)
#' )
#'
#' # Save to file and provide a name
#' x_dif <- hsi_calc_difference(
#'  x,
#'  bands = c(620, 680)
#'  index_name = "diff620680",
#'  filename = "output_diff.tif",
#'  overwrite = TRUE
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

  # Find edge positions
  edge_positions <- wavelength_position(x = x, wavelength = bands) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Subtract
  result <- terra::subset(x, edge_positions[1]) -
    terra::subset(x, edge_positions[2])

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
