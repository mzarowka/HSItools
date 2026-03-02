#' Write HSI reflectance raster as scaled uint16 GeoTIFF
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data
#' @param filename Character. Output file path. Always writes to disk.
#' @param scale_factor Numeric. Scale factor applied before writing.
#'   Default 10000 gives 4 decimal places of precision. Values must not exceed 65535 / scale_factor.
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Converts a float32 reflectance SpatRaster to uint16 by applying a scale
#' factor and embedding scale metadata so \code{\link[terra]{rast}} reads back
#' as float transparently. Reduces file size by ~50% before compression.
#'
#' @details
#' The scale factor is embedded in GeoTIFF band metadata (GDAL scale/offset),
#' so \code{\link[terra]{rast}} automatically returns float values on read —
#' no manual rescaling required.
#'
#' Use \code{hsi_check_reflectance()} to validate value ranges before writing.
#'
#' @return A terra SpatRaster
#' @export
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE.tif")
#'
#' # Check before writing
#' hsi_check_reflectance(x)
#'
#' # Write scaled
#' hsi_write_scaled(
#'   x,
#'   filename = "REFLECTANCE_scaled.tif",
#'   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
#' )
#'
#' # Read back: terra applies scale automatically, returns float
#' terra::rast("REFLECTANCE_scaled.tif")
#' }
hsi_write_scaled <- function(
  x,
  filename,
  scale_factor = 10000L,
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate scale factor
  check_numeric(scale_factor, len = 1, positive = TRUE)

  # Check values fit within uint16 at given scale factor
  uint16_max_value <- 65535 / scale_factor

  # Check max value in a SpatRaster
  global_max <- terra::global(x, fun = "max", na.rm = TRUE) |>
    dplyr::pull("max") |>
    max(na.rm = TRUE)

  # Validate agains uint16 max available space
  if (global_max > uint16_max_value) {
    cli::cli_abort(c(
      "Maximum value {.val {round(global_max, 4)}} exceeds uint16 capacity \\
      at scale factor {.val {scale_factor}} (max storable: {.val {uint16_max_value}}).",
      "i" = "Run {.fn hsi_check_reflectance} before writing."
    ))
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    datatype = "INT2U",
    scale = 1 / scale_factor,
    offset = 0
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Write to file
  result <- terra::writeRaster(
    x,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return
  return(result)
}
