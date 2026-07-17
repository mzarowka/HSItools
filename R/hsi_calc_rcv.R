#' Calculate coefficient of variation of reflectance (Rcv)
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param index_name Character. Name for the output layer. Default `NULL`.
#' @param na.rm Logical. Remove `NA` values. Default `TRUE`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with coefficient of variation values.
#'
#' @description
#' Calculate the coefficient of variation (CV) of reflectance across all spectral
#' bands for each pixel. CV is a standardized, dimensionless measure of spectral
#' dispersion relative to mean brightness.
#'
#' @details
#' CV is calculated as `sd / mean` for each pixel across all wavelengths.
#' Unlike standard deviation alone, CV normalizes for brightness differences,
#' making it comparable across pixels with different mean reflectance values.
#'
#' @seealso
#' [`hsi_calc_rsd()`] for standard deviation,
#' [`hsi_calc_rmean()`] for mean reflectance.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_rcv <- hsi_calc_rcv(x)
#'
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
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

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

  # Calculate sd and mean
  # Note: these are intermediate results, not written to file
  x_sd <- terra::app(x, fun = "sd", na.rm = na.rm)
  x_mean <- terra::app(x, fun = "mean", na.rm = na.rm)

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
  result
}
