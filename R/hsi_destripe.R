#' Destripe hyperspectral raster
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with destriped reflectance values.
#'
#' @details
#' Pushbroom detectors produce additive (offset) and multiplicative (gain)
#' striping (Rogaß et al., 2011). Because dark current – the source of additive
#' miscalibration – is removed upstream by [`hsi_calc_reflectance()`], only
#' multiplicative residuals from uneven flat-field calibration remain. Per band,
#' each column is therefore scaled by `pixel × (global_median / column_median)`,
#' a gain-only variant of the moment-matching approach of Gadallah et al. (2000)
#' using median as a parameter-free robust estimator. Input must be cropped to
#' the specimen extent – background pixels bias column medians and corrupt the
#' correction.
#' For sources and types of detector errors look, for example, into:
#'
#' Rogaß et al. (2011) \doi{10.3390/s110606370}
#'
#' Gadallah et al. (2000) \doi{10.1080/01431160050030592}
#'
#' Input should be cropped to the specimen extent before destriping. Background
#' pixels (ruler, frame, tape) bias column medians and corrupt the correction.
#'
#' @seealso
#' [`hsi_calc_reflectance()`] for the preceding pipeline step.
#' [`hsi_smooth_median()`] for the following pipeline step.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_destripe <- hsi_destripe(x)
#'
#' x_destripe <- hsi_destripe(
#'   x,
#'   filename = "REFLECTANCE_destripe.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_destripe <- function(
  x,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Named list with write options
  wopt_default <- list()

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Compute per-column median across all rows, per band
  col_medians_raster <- terra::aggregate(
    x,
    fact = c(terra::nrow(x), 1),
    fun = "median"
  )

  # Expand column medians back to full raster extent
  col_medians_full <- terra::disagg(
    col_medians_raster,
    fact = c(terra::nrow(x), 1)
  )

  # Global reference: median of column medians, per band
  glob_median <- terra::global(col_medians_raster, median)[[1]]

  # Apply multiplicative correction
  result <- x / col_medians_full * glob_median

  # Reset names
  names(result) <- names(x)

  # Write to file if requested
  if (filename != "") {
    return(
      terra::writeRaster(
        result,
        filename = filename,
        overwrite = overwrite,
        wopt = wopt
      )
    )
  }

  # Return
  result
}
