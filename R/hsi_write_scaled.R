#' Write HSI reflectance raster as scaled uint16 GeoTIFF
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param filename Character. Output file path. Always writes to disk.
#' @param scale_factor Numeric. Scale factor applied before writing. Default
#'   `10000` gives 4 decimal places of precision.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] written to `filename`.
#'
#' @description
#' Convert a float32 reflectance raster to uint16 by applying a scale factor
#' and embedding scale metadata so [`terra::rast()`] reads back as float
#' transparently. Reduces file size by approximately 50% before compression.
#'
#' @details
#' The scale factor is embedded in GeoTIFF band metadata (GDAL scale/offset),
#' so [`terra::rast()`] automatically returns float values on read — no manual
#' rescaling required. Values must not exceed `65535 / scale_factor` or an
#' error is raised.
#'
#' uint16 quantization is only appropriate for sensors with sufficient SNR.
#' VNIR sensors typically meet this threshold; SWIR sensors with lower SNR
#' may lose meaningful signal in the quantization step and should be written
#' as float32 instead.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE.tif")
#'
#' hsi_write_scaled(
#'   x,
#'   filename = "REFLECTANCE_scaled.tif",
#'   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
#' )
#'
#' terra::rast("REFLECTANCE_scaled.tif")
#' }
#'
#' @export
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
  result
}
