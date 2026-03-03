#' Destripe hyperspectral raster (pmap)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data.
#' @param filename Character. Output filename. Default "" keeps in memory.
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param in_memory Logical. Should processing be done in memory.
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description Destripes a hyperspectral SpatRaster.
#'
#'
#' @return A [`SpatRaster`][terra::SpatRaster-class] with destriped reflectance values.
#'
#' @export
hsi_destripe_pmap <- function(
  x,
  filename = "",
  overwrite = FALSE,
  in_memory = FALSE,
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

  # Get the median value of the white reference for each column
  col_medians_raster <- terra::aggregate(
    x,
    fact = c(terra::nrow(x), 1),
    fun = "median"
  )

  # Calculate global median and get is a vector
  glob_median <- terra::global(col_medians_raster, "mean")

  result <- list(
    x = terra::as.list(x),
    col_medians = terra::as.list(col_medians_raster),
    glob_median = as.list(glob_median[[1]])
  ) |>
    purrr::pmap(\(x, col_medians, glob_median) {
      band <- terra::as.matrix(x, wide = TRUE)

      col_medians <- as.vector(col_medians)

      glob_median <- glob_median

      result <- sweep(band, 2, col_medians, "/") * glob_median

      # Create a temporary raster to store the result
      if (!in_memory) {
        result <- terra::init(
          x,
          t(result),
          filename = tempfile(fileext = ".tif"),
          wopt = list(gdal = c("COMPRESS=NONE"))
        )
      } else {
        result <- terra::init(
          x,
          t(result)
        )
      }
    }) |>
    terra::rast()

  # Reset names
  names(result) <- terra::names(x)

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

#' Destripe hyperspectral raster (disagg)
#'
#' @family HSI Transformations
#'
#' @param x A terra SpatRaster with hyperspectral data.
#' @param filename Character. Output filename. Default "" keeps in memory.
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param in_memory Logical. Should processing be done in memory.
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description Destripes a hyperspectral SpatRaster.
#'
#'
#' @return A [`SpatRaster`][terra::SpatRaster-class] with destriped reflectance values.
#'
#' @export
hsi_destripe_disagg <- function(
  x,
  filename = "",
  overwrite = FALSE,
  in_memory = FALSE,
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

  # Get the median value of the white reference for each column
  col_medians_raster <- terra::aggregate(
    x,
    fact = c(terra::nrow(x), 1),
    fun = "median"
  )

  col_medians_full <- terra::disagg(
    col_medians_raster,
    fact = c(terra::nrow(x), 1)
  )

  glob_median <- terra::global(col_medians_raster, median)[[1]]

  result <- x / col_medians_full * glob_median

  # Reset names
  names(result) <- terra::names(x)

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
