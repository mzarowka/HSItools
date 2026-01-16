#' Calculate coefficient of variation of reflectance (Rcv)
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
#' @return A terra SpatRaster with coefficient of variation values
#'
#' @description
#' Calculate coefficient of variation (CV) of reflectance across all spectral
#' bands for each pixel. CV is a standardized, dimensionless measure of spectral
#' dispersion relative to mean brightness.
#'
#' @details
#' Coefficient of variation is calculated as the ratio of standard deviation
#' to mean reflectance (CV = sd / mean) for each pixel across all wavelengths.
#'
#' CV is useful for:
#' - Identifying pixels with strong spectral features (absorption bands increase variability)
#' - Detecting spectrally mixed pixels
#' - Comparing spectral variability across areas with different overall brightness
#' - Quality assessment (high CV in dark areas may indicate noise)
#'
#' Unlike standard deviation alone, CV normalizes for brightness differences,
#' making it comparable across pixels with different mean reflectance values.
#'
#' @seealso
#' \code{\link{hsi_calc_rsd}} for standard deviation,
#' \code{\link{hsi_calc_rmean}} for mean reflectance
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate coefficient of variation
#' x_rcv <- hsi_calc_rcv(x)
#'
#' # Save to file with custom name
#' x_rcv <- hsi_calc_rcv(
#'   x,
#'   index_name = "cv_reflectance",
#'   filename = "output_rcv.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_rcv <- function(
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

  # Calculate sd and mean
  # Note: these are intermediate results, not written to file
  x_sd <- terra::app(x, fun = "sd", na.rm = na.rm, cores = cores)
  x_mean <- terra::app(x, fun = "mean", na.rm = na.rm, cores = cores)

  # Calculate CV = sd / mean
  result <- x_sd / x_mean

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
