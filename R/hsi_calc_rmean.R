#' Calculate mean reflectance (Rmean)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param index_name Character. Name of calculated rmean. Default NULL
#' @param na.rm Logical. Remove NA values when calculating mean (default: TRUE)
#' @param cores positive integer. If cores > 1, a \pkg{parallel} package cluster with that many cores is created and used. You can also supply a cluster object.
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @return A terra SpatRaster with mean reflectance values
#'
#' @description
#' Calculate mean reflectance across all spectral bands for each pixel in a
#' hyperspectral image. This provides a measure of overall brightness and can
#' be useful for normalizing other spectral indices.
#'
#' @details
#' Mean reflectance (Rmean) is calculated as the arithmetic mean of reflectance
#' values across all wavelengths for each pixel
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate mean reflectance
#' x_rmean <- hsi_calc_rmean(x)
#'
#' # Save to file and provide a name
#' x_rmean <- hsi_calc_rmean(
#'  x,
#'  index_name = "mean_reflectance",
#'  filename = "output_rmean.tif",
#'  overwrite = TRUE)
#' }
#'
#' @export
hsi_calc_rmean <- function(
  x,
  index_name = NULL,
  na.rm = TRUE,
  cores = 1,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  if (!inherits(x, "SpatRaster")) {
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

  # Conditional writing can be, probably, handled a little bit better?

  # Apply mean function over entire SpatRaster
  result <- terra::app(x, fun = "mean", na.rm = na.rm, cores = cores)

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
      ...
    )
  }

  # Return
  return(result)
}
