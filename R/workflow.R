# General workflow
# Get capture raster from shiny output
# Get white reference raster from shiny output
# Get dark reference raster from shiny output
# Set workflow mode 1: normalize or not
# Set workflow mode 2: normalize full or cropped
# Set workflow mode 3: normalize rois only?
# Set workflow mode 4: normalize subset of layers
# Set workflow mode 5: different white reference capture time
#

#' Prepare core based on shiny output
#'
#' @param core shiny output.
#' @param .path path to the directory with captured data. Defaults to NULL and shiny output.
#' @param .layers numeric vector, selection of layers (wavelengths) to use. Defaults to NULL and shiny output.
#' @param .extent extent of the captured data. Defaults to NULL and shiny output. If "capture" then uses entire extent of captured data.
#' @param .normalize logical, should data be normalized.
#'
#' @return reflectance SpatRaster.
#' @export
prepare_core <- function(core = NULL, .path = NULL, .layers = NULL, .extent = NULL, .normalize = TRUE) {
  if (is.null(.path) == TRUE) {
    # Get path
    path <- core$directory
  } else {
    path <- .path
  }

  if (is.null(.layers) == TRUE) {
    # Get path
    layers <- core$layers
  } else {
    layers <- .layers
  }

  cli::cli_h1("{basename(path)}")

  # Create products directory and store path
  products <- fs::dir_create(paste0(path, "/products"))

  # List data files in the directory
  files <- fs::dir_ls(paste0(path, "/capture"))

  # Check if file needs to be normalized from .raw
  if (.normalize == TRUE) {
    # List files: CAPTURE, DARKREF and WHITEREF
    files <- fs::path_filter(files, regexp = ".raw")

    # SpatRaster types
    types <- list("darkref", "capture", "whiteref")

    if (is.null(.extent) == TRUE) {
      # Get path
      extent <- core$cropImage
    } else if (.extent == "capture") {
      extent <- terra::rast(files[[2]]) |>
        terra::ext()
    }

    # Extent
    big_roi <- terra::ext(extent)

    cli::cli_alert_info("{format(Sys.time())}: reading rasters.")

    # Read SpatRasters
    rasters <- files |>
      # Load SpatRasters
      purrr::map(\(x) terra::rast(x))

    # Get band positions - the same for all three SpatRasters
    band_position <- HSItools::spectra_position(rasters[[1]], layers)

    cli::cli_alert_info("{format(Sys.time())}: subsetting layers.")

    # Subset bands in the SpatRasters
    rasters_subset <- rasters |>
      purrr::map(\(x) HSItools::spectra_sub(raster = x, spectra_tbl = band_position))

    cli::cli_alert_info("{format(Sys.time())}: cropping rasters.")

    # Crop
    rasters_cropped <- purrr::map2(rasters_subset, types, \(x, y) HSItools::raster_crop(x, y, big_roi, path = path))

    cli::cli_alert_info("{format(Sys.time())}: calculating reference rasters.")

    # Prepare reference SpatRasters
    rasters_references <- purrr::map2(rasters_cropped[c(1, 3)], types[c(1, 3)], \(x, y) HSItools::create_reference_raster(raster = x, ref_type = y, roi = big_roi, path = path))

    cli::cli_alert_info("{format(Sys.time())}: calculating reflectance raster.")

    # Normalize data
    reflectance <- HSItools::create_normalized_raster(
      capture = rasters_cropped[[2]],
      whiteref = rasters_references[[2]],
      darkref = rasters_references[[1]],
      fun = normalization,
      path = path)

    cli::cli_alert_info("{format(Sys.time())}: cleaning up.")

    # Remove temporary disaggregated files
    fs::dir_ls(products, regexp = "disaggregated") |>
      fs::file_delete()

    cli::cli_alert_success("{format(Sys.time())}: finished.")

  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }

  # Return reflectance
  return(reflectance)
}
