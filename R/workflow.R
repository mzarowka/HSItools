#' Get the reflectance
#'
#' @param core shiny output.
#' @param path path to the directory with captured data. Defaults to NULL and shiny output.
#' @param layers numeric vector, selection of layers (wavelengths) to use. Defaults to NULL and shiny output.
#' @param extent extent of the captured data. Defaults to NULL and shiny output. If "capture" then uses entire extent of captured data.
#' @param normalize logical, should data be normalized.
#' @param integration logical, whether white reference was scanned with different settings.
#' @param tintw integration time of the white reference.
#' @param tintd integration time of the captured data (sample).
#'
#' @return reflectance SpatRaster.
#' @export
prepare_core <- function(core = NULL, path = NULL, layers = NULL, extent = NULL, normalize = TRUE, integration = NULL, tintw = 1, tints = 1) {
  if (!is.null(core) == TRUE) {
    # Get path
    path <- fs::path(getwd(), core$directory)

    # Get layers
    layers <- core$layers

    # Get files
    files <-  core$rasterPaths

    files <- list(
      capture = fs::path(path, files[["capture"]]),
      darkref = fs::path(path, files[["darkref"]]),
      whiteref = fs::path(path, files[["whiteref"]]))

  } else {
    # Get path
    path <- path

    # Get layers
    layers <- layers

    # Get files
    files <- fs::dir_ls(paste0(path, "/capture")) |>
      fs::path_filter(regexp = ".raw|.tif")

    # List files
    files <- list(
      capture = fs::path_filter(files, regexp = "WHITE|DARK", invert = TRUE),
      darkref = fs::path_filter(files, regexp = "DARK"),
      whiteref = fs::path_filter(files, regexp = "WHITE")
    )
  }

  # Create products directory and store path
  # If no rois are selected create only products
  products <- fs::dir_create(paste0(path, "/products"))

  # Check if file needs to be normalized from .raw
  if (normalize == TRUE) {

    # SpatRaster types
    types <- list(capture = "capture", darkref = "darkref", whiteref = "whiteref")

    if (is.character(extent) == TRUE) {
      if (extent == "capture"){
        extent <- terra::rast(files[["capture"]]) |>
          terra::ext()
      }
    } else if (is.null(extent) == TRUE) {
      extent <- core$cropImage

    } else if (inherits(extent, what = "SpatExtent") == TRUE) {
      extent <- terra::ext(extent)
    }

    big_roi <- terra::ext(extent)

    # Read SpatRasters
    rasters <- files |>
      # Load SpatRasters
      purrr::map(\(x) terra::rast(x))

    # Get band positions - the same for all three SpatRasters
    band_position <- HSItools::spectra_position(rasters[["capture"]], layers)

    # Subset bands in the SpatRasters
    rasters_subset <- rasters |>
      purrr::map(\(x) HSItools::spectra_sub(
        raster = x,
        spectra_tbl = band_position))

    # Crop
    rasters_cropped <- purrr::map2(
      rasters_subset,
      types,
      \(x, y) HSItools::raster_crop(
        raster = x,
        type = y,
        roi = big_roi))

    # Prepare reference SpatRasters
    rasters_references <- purrr::map2(
      rasters_cropped[c("darkref", "whiteref")],
      types[c("darkref", "whiteref")],
      \(x, y) HSItools::create_reference_raster(
        raster = x,
        ref_type = y,
        roi = big_roi,
        path = path))

    # Normalize data
    reflectance <- HSItools::create_normalized_raster(
      capture = rasters_cropped[["capture"]],
      whiteref = rasters_references[["whiteref"]],
      darkref = rasters_references[["darkref"]],
      tintw = tintw,
      tints = tints,
      fun = normalization,
      path = path)

    # Remove temporary files
    fs::dir_ls(products, regexp = "resampled|cropped") |>
      fs::file_delete()

  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }

  # Return reflectance
  return(reflectance)
}
