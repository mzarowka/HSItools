#' Calculate standard deviation of reflectance (Rsd)
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
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with reflectance standard deviation values.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_rsd <- hsi_calc_rsd(x)
#'
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
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Set name (needed when not writing to file)
  if (!is.null(index_name) && filename == "") {
    names(result) <- index_name
  }

  # Return
  result
}
