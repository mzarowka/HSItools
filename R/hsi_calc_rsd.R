#' Calculate standard deviation of reflectance (Rsd)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param index_name Character. Name of calculated index. Default NULL
#' @param na.rm Logical. Remove NA values when calculating (default: TRUE)
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster
#'   with that many cores is created and used. You can also supply a cluster object.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with reflectance standard deviation values
#'
#' @description
#' Calculate standard deviation of reflectance across all spectral bands for each pixel in a
#' hyperspectral image. This provides a measure of overall spectral heterogeneity.
#'
#' @details
#' Standard deviation of reflectance (Rsd) is calculated as the standard deviation of reflectance
#' values across all wavelengths for each pixel.
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate standard deviation of reflectance
#' x_rsd <- hsi_calc_rsd(x)
#'
#' # Save to file
#' x_rsd <- hsi_calc_rsd(
#'   x,
#'   index_name = "sd_reflectance",
#'   filename = "output_rsd.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_rsd <- function(
  x,
  index_name = NULL,
  na.rm = TRUE,
  cores = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply sd function over entire SpatRaster
  result <- terra::app(
    x,
    fun = "sd",
    na.rm = na.rm,
    cores = cores,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set name (needed when not writing to file)
  if (!is.null(index_name) && filename == "") {
    names(result) <- index_name
  }

  # Return
  return(result)
}
