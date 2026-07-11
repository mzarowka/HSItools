#' Write HSI reflectance raster as a scaled integer or float GeoTIFF
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param filename Character. Output file path. Always writes to disk.
#' @param scale_factor Numeric. Scale factor applied before writing. Default
#'   `10000` gives 4 decimal places of precision. Ignored for float datatypes.
#' @param datatype Character. Output datatype. One of `"INT1U"`, `"INT2U"`,
#'   `"INT2S"`, `"INT4U"`, `"INT4S"`, `"FLT4S"`, `"FLT8S"`. Default `"INT2U"`.
#'   Integer types apply `scale_factor` and are range-checked before writing.
#'   Float types write values as-is with no scaling or range validation.
#'   Use `"FLT4S"` for sensors with low SNR such as SWIR.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with values written to `filename`.
#'
#' @description
#' Write a reflectance raster to a GeoTIFF, optionally scaling float values to
#' an integer datatype. The scale factor is embedded in GeoTIFF band metadata
#' so [`terra::rast()`] reads back float values transparently — no manual
#' rescaling required.
#'
#' @details
#' For integer datatypes, values are multiplied by `scale_factor` before
#' writing and the reciprocal is stored as GDAL scale metadata. An error is
#' raised if any value exceeds the maximum storable value for the chosen
#' datatype at the given scale factor. Integer storage reduces file size by
#' approximately 50% relative to float32 before compression.
#'
#' For float datatypes (`"FLT4S"`, `"FLT8S"`), `scale_factor` has no effect
#' and no range validation is performed.
#'
#' Choose the datatype based on sensor characteristics. VNIR sensors with high
#' SNR are well suited to `"INT2U"` at the default scale factor. Sensors with
#' lower SNR, such as SWIR, should use `"FLT4S"` to avoid quantization
#' degrading meaningful signal.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE.tif")
#'
#' # Default: scaled uint16 for VNIR
#' x_scaled <- hsi_write_scaled(
#'   x,
#'   filename = "REFLECTANCE_scaled.tif",
#'   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
#' )
#'
#' # Float32 for SWIR
#' x_scaled <- hsi_write_scaled(
#'   x,
#'   filename = "REFLECTANCE_swir.tif",
#'   datatype = "FLT4S"
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
  datatype = "INT2U",
  ...
) {
  # Validate inputs
  check_spatraster(x)

  check_numeric(scale_factor, len = 1, positive = TRUE)

  check_one_of(
    datatype,
    choices = c("INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT8S")
  )

  # Maximum storable value per integer datatype
  datatype_max <- c(
    INT1U = 255,
    INT2U = 65535,
    INT2S = 32767,
    INT4U = 4294967295,
    INT4S = 2147483647
  )

  # Only validate range for integers (floats have no meaningful ceiling)
  if (datatype %in% names(datatype_max)) {
    max_storable <- datatype_max[[datatype]] / scale_factor

    global_max <- terra::global(x, fun = "max", na.rm = TRUE) |>
      dplyr::pull("max") |>
      max(na.rm = TRUE)

    if (global_max > max_storable) {
      cli::cli_abort(
        c(
          "Maximum value {.val {round(global_max, 4)}} exceeds {.val {datatype}} capacity at scale factor {.val {scale_factor}} (max storable: {.val {max_storable}})."
        ),
        class = "hsitools_error"
      )
    }
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list(
    datatype = datatype,
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
  invisible(result)
}
