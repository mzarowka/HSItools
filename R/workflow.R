# General workflow
# Get capture raster from shiny output
# Get white reference raster from shiny output
# Get dark reference raster from shiny output
# Set workflow mode 1: normalize or not
# Set workflow mode 2: normalize full or cropped
# Set workflow mode 3: normalize rois only?
# Set workflow mode 4: normalize subset of layers
# Set workflow mode 5: different white reference capture time
get_reflectance <- function(core = NULL, path = NULL, layers = NULL, extent = NULL, normalize = TRUE, integration = NULL, roi = FALSE) {
  # Create main products dir

  # If no rois are selected create only products
  if (roi == FALSE){
    products <- fs::dir_create(paste0(path, "/products"))

    reflectance <- prepare_core(core = core, path = path, layers = layers, extent = extent, normalize = normalize, integration = integration)
    # If rois are selected, create appropriate output directories
  } else {
    # Create appopriate sub directories
    products <- fs::dir_create(paste0(path, "/products/", core$analysisRegions$roi.id))

    # Prepare core for each ROI and move to appropriate directory, rename
    core <- core

    extent =

    reflectance <- extent |>
      dplyr::group_split(roi.id) |>
      purrr::set_names(nm = core[["analysisRegions"]][["roi.id"]]) |>
      purrr::map(\(i) terra::ext(i)) |>
      purrr::map(\(i) get_and_move(prepare_core(), name = i, core = core, path = path, layers = layers, extent = i, normalize = normalize, integration = integration))
  }

  # Return
  return(reflectance)

}


#' Prepare core based on shiny output
#'
#' @param core shiny output.
#' @param path path to the directory with captured data. Defaults to NULL and shiny output.
#' @param layers numeric vector, selection of layers (wavelengths) to use. Defaults to NULL and shiny output.
#' @param extent extent of the captured data. Defaults to NULL and shiny output. If "capture" then uses entire extent of captured data.
#' @param normalize logical, should data be normalized.
#' @param integration logical, whether references were scanned with different settings.
#'
#' @return reflectance SpatRaster.
#' @export
prepare_core <- function(core = NULL, path = NULL, layers = NULL, extent = NULL, normalize = TRUE, integration = NULL) {
  if (!is.null(core) == TRUE) {
    # Get path
    path <- core$directory

    # Get layers
    layers <- core$layers

    # Get files
    files <- core$rasterPaths

    files <- list(
      capture = files[["capture"]],
      darkref = files[["darkref"]],
      whiteref = files[["whiteref"]])

  } else {
    # Get path
    path <- path

    # Get layers
    layers <- layers

    # Get files
    files <- files <- fs::dir_ls(paste0(path, "/capture")) |>
      fs::path_filter(regexp = ".raw|.tif")

    # List files
    files <- list(
      capture = fs::path_filter(files, regexp = "WHITE|DARK", invert = TRUE),
      darkref = fs::path_filter(files, regexp = "DARK"),
      whiteref = fs::path_filter(files, regexp = "WHITE")
    )
  }

  cli::cli_h1("{basename(path)}")

  # Create products directory and store path
  # If no rois are selected create only products
  products <- fs::dir_create(paste0(path, "/products"))

  # Check if file needs to be normalized from .raw
  if (normalize == TRUE) {
    # List files: CAPTURE, DARKREF and WHITEREF
    # files <- fs::path_filter(files, regexp = ".raw")

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
    # if (is.null(extent) == TRUE) {
    #   # Get path
    #   extent <- core$cropImage
    # } else if (extent == "capture") {
    #   extent <- terra::rast(files[["capture"]]) |>
    #     terra::ext()
    # } else if (!is.null(extent) == TRUE) {
    #   extent <- terra::ext(roi)
    # }

    big_roi <- terra::ext(extent)

    cli::cli_alert_info("{format(Sys.time())}: reading rasters.")

    # Get paths separated
    # files_path <-

    # Print for user to check
    cli::cli_inform("Proceeding with the following \"CAPTURE\", \"DARK reference\", and \"WHITE reference\"")
    cli::cli_li(files)

    # Read SpatRasters
    rasters <- files |>
      # Load SpatRasters
      purrr::map(\(x) terra::rast(x))

    # Get band positions - the same for all three SpatRasters
    band_position <- HSItools::spectra_position(rasters[["capture"]], layers)

    cli::cli_alert_info("{format(Sys.time())}: subsetting layers.")

    # Subset bands in the SpatRasters
    rasters_subset <- rasters |>
      purrr::map(\(x) HSItools::spectra_sub(
        raster = x,
        spectra_tbl = band_position))

    cli::cli_alert_info("{format(Sys.time())}: cropping rasters.")

    # Crop
    rasters_cropped <- purrr::map2(
      rasters_subset,
      types,
      \(x, y) HSItools::raster_crop(
        raster = x,
        type = y,
        roi = big_roi))

    cli::cli_alert_info("{format(Sys.time())}: calculating reference rasters.")

    # Prepare reference SpatRasters
    rasters_references <- purrr::map2(
      rasters_cropped[c("darkref", "whiteref")],
      types[c("darkref", "whiteref")],
      \(x, y) HSItools::create_reference_raster(
        raster = x,
        ref_type = y,
        roi = big_roi,
        path = path))

    cli::cli_alert_info("{format(Sys.time())}: calculating reflectance raster.")

    # Normalize data
    reflectance <- HSItools::create_normalized_raster(
      capture = rasters_cropped[["capture"]],
      whiteref = rasters_references[["whiteref"]],
      darkref = rasters_references[["darkref"]],
      fun = normalization,
      path = path)

    cli::cli_alert_info("{format(Sys.time())}: cleaning up.")

    # Remove temporary disaggregated files
    fs::dir_ls(products, regexp = "disaggregated|cropped") |>
      fs::file_delete()

    cli::cli_alert_success("{format(Sys.time())}: finished.")

  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }

  # Return reflectance
  return(reflectance)
}
