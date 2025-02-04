#' Get the reflectance
#'
#' @param core shiny output.
#' @param path path to the directory with captured data. Defaults to NULL and shiny output.
#' @param layers numeric vector, selection of layers (wavelengths) to use. Defaults to NULL and shiny output.
#' @param extent extent of the captured data. Defaults to NULL and shiny output. If "capture" then uses entire extent of captured data.
#' @param normalize logical, should data be normalized.
#' @param integration logical, whether white reference was scanned with different settings.
#' @param tintw integration time of the white reference.
#' @param tints integration time of the captured data (sample).
#' @param flip logical, wheter output should be flipped. terra flips unprojected rasters (or rather, unprojected rasters are flipped from the beginning). Defaults to TRUE.
#' @param verbose logica, should additional informatiob be printed to the console. Defaults to FALSE.
#'
#' @return reflectance SpatRaster.
#' @export
prepare_core <- function(
  core = NULL,
  path = NULL,
  layers = NULL,
  extent = NULL,
  normalize = TRUE,
  integration = NULL,
  tintw = 1,
  tints = 1,
  flip = TRUE,
  verbose = FALSE
) {
  
  if (!is.null(core)) {
    # Get path
    path <- fs::path(getwd(), core$directory)

    # Get layers
    layers <- core$layers

    # Get files
    files <- core$rasterPaths

    files <- list(
      capture = fs::path(path, files[["capture"]]),
      darkref = fs::path(path, files[["darkref"]]),
      whiteref = fs::path(path, files[["whiteref"]])
    )
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

  if (verbose == TRUE) {
    cli::cli_alert_info("{format(Sys.time())} Creating products directory")
  }

  # Create products directory and store path
  # If no rois are selected create only products
  products <- fs::dir_create(paste0(path, "/products"))

  # Check if file needs to be normalized from .raw
  if (normalize == TRUE) {
    # SpatRaster types
    types <- list(
      capture = "capture",
      darkref = "darkref",
      whiteref = "whiteref"
    )

    if (is.character(extent) == TRUE) {
      if (extent == "capture") {
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
      purrr::map(
        \(x)
          HSItools::spectra_sub(
            raster = x,
            spectra_tbl = band_position
          )
      )

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Cropping rasters")
    }

    # Crop
    if (extent == "capture") {
      rasters_cropped <- purrr::map2(rasters_subset, {\(raster) 
        # Raster source directory
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()

    filename <- paste0(
      raster_src,
      "/products/",
      raster_name,
      "_cropped.tif"
    )
        
        terra::writeRaster(raster, filename = filename, wopt= list(steps = terra::nlyr(raster) * terra::ncell(raster)))})
    } else {
    rasters_cropped <- purrr::map2(
      rasters_subset,
      types,
      \(x, y)
        HSItools::raster_crop(
          raster = x,
          type = y,
          roi = big_roi
        )
    )
    }

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Resampling references")
    }

    # Prepare reference SpatRasters
    rasters_references <- purrr::map2(
      rasters_cropped[c("darkref", "whiteref")],
      types[c("darkref", "whiteref")],
      \(x, y)
        HSItools::create_reference_raster(
          raster = x,
          ref_type = y,
          roi = big_roi,
          path = path
        )
    )

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Cleaning up cropped references")
    }

    # Remove temporary files
    fs::dir_ls(products, regexp = "DARKREF.*cropped|WHITEREF.*cropped") |>
      fs::file_delete()

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Calculating reflectance")
    }

    # Normalize data
    reflectance <- HSItools::create_normalized_raster(
      capture = rasters_cropped[["capture"]],
      whiteref = rasters_references[["whiteref"]],
      darkref = rasters_references[["darkref"]],
      tintw = tintw,
      tints = tints,
      fun = normalization,
      path = path
    )

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Cleaning up")
    }

    # Remove temporary files
    fs::dir_ls(products, regexp = "resampled|cropped") |>
      fs::file_delete()

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Flipping reflectance")
    }

    # Finally flip because of terra handling of unprojected rasters
    if (flip == TRUE) {
    reflectance_flip <- reflectance |>
      {
        \(i)
          terra::flip(
            x = i,
            direction = "vertical",
            filename = gsub(
              pattern = "REFLECTANCE_",
              replacement = "REFLECTANCE_flip",
              x = terra::sources(i)
            ),
            overwrite = TRUE
          )
      }()

      # Delete REFLECTANCE
      fs::file_delete(terra::sources(reflectance))

      # Rename REFLECTANCE flipped
      new_path <- fs::file_move(terra::sources(reflectance_flip), sub(pattern = "REFLECTANCE_flip", replacement = "REFLECTANCE_", x = terra::sources(reflectance_flip)))

      # Get REFLECTANCE back
      reflectance <- terra::rast(new_path)


    } else {
      reflectance <- reflectance
    }

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Cleaning up")
    }

    # Remove temporary files
    fs::dir_ls(products, regexp = "_rev") |>
      fs::file_delete()
    
  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }

  # Return reflectance
  # return(reflectance)

  if (verbose == TRUE) {
    cli::cli_alert_success("{format(Sys.time())} Finished")
  }
}
