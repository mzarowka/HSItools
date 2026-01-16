#' Focal raster smooth with a median
#'
#' @family HSI Transformations
#' @param x A terra SpatRaster with hyperspectral data
#' @param window Focal window size, must be odd (default: 3)
#' @param filename Character. Output filename. Default "" keeps in memory
#' @param overwrite Logical. Overwrite existing file (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[terra]{writeRaster}}
#'
#' @description
#' Apply a focal (spatial) median filter to smooth hyperspectral data.
#' The median filter finds the median value within a given window and assigns
#' it to the pixel of interest, reducing noise while preserving edges.
#'
#' @details
#' The focal median filter smoothes data by finding the median value within
#' a given window and assigning its value to a pixel of interest.
#'
#' @return A terra SpatRaster with median filtered values
#'
#' @examples
#' \dontrun{
#' # Load hyperspectral data
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Calculate medianm smooth reflectance with 3 × 3 window (default)
#' x_median <- hsi_smooth_median(x)
#'
#' # Save to file
#' x_median <- hsi_smooth_median(
#'  x,
#'  filename = "output_median.tif",
#'  overwrite = TRUE)
#' }
#'
#' @export
hsi_smooth_median <- function(
  x,
  window = 3,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate input
  check_numeric(window, odd = 2)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)

  # Extract band names
  band_names <- terra::names(x)

  # Named list with write options
  wopt_default <- list(
    names = band_names
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Apply terra focal statistic
  result <- terra::focal(
    x,
    w = window,
    fun = "median",
    na.rm = TRUE,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Return raster
  return(result)
}
