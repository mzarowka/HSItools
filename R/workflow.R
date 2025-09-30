#' Get the reflectance
#'
#' @param core shiny output.
#' @param path path to the directory with captured data. Defaults to NULL and shiny output.
#' @param layers numeric vector, selection of layers (wavelengths) to use. Defaults to NULL and shiny output or NULL and all layers.
#' @param extent extent of the captured data. Defaults to NULL and shiny output. If "capture" then uses entire extent of captured data.
#' @param normalize logical, should data be normalized.
#' @param integration logical, whether white reference was scanned with different settings.
#' @param tintw integration time of the white reference.
#' @param tints integration time of the captured data (sample).
#' @param flip logical, wheter output should be flipped. Defaults to FALSE.
#' @param verbose logical, should additional information be printed to the console. Defaults to FALSE.
#'
#' @return reflectance SpatRaster.
#' @export
get_reflectance <- function(
  core = NULL,
  path = NULL,
  layers = NULL,
  extent = NULL,
  normalize = TRUE,
  integration = NULL,
  tintw = 1,
  tints = 1,
  flip = FALSE,
  verbose = FALSE
) {
  if (!is.null(core)) {
    # Get path
    path <- fs::path(core$directory)

    # Get layers
    layers <- core$layers

    # Get files
    files <- core$rasterPaths

    files <- list(
      capture = fs::path(path,"capture", files[["capture"]]),
      darkref = fs::path(path, "capture",files[["darkref"]]),
      whiteref = fs::path(path,"capture",files[["whiteref"]])
    )
  } else {
    # Get path
    path <- path

    # Get files
    files <- fs::dir_ls(paste0(path, "/capture")) |>
      fs::path_filter(regexp = ".raw|.tif")

    # List files
    files <- list(
      capture = fs::path_filter(files, regexp = "WHITE|DARK", invert = TRUE),
      darkref = fs::path_filter(files, regexp = "DARK"),
      whiteref = fs::path_filter(files, regexp = "WHITE")
    )

    # # Get layers, if nothing provided use all
    # layers <- layers %||%
    #   as.numeric(terra::names(terra::rast(files[["capture"]]))) |>
    #   {
    #     \(raster) c(min(raster):max(raster))
    #   }()
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

    # Handle extent
    if (is.null(extent)) {
      # If no extent is provided, use core$cropImage if available
      ext <- if (!is.null(core)) core$cropImage %||% NULL

      # Crop type
      crop <- "shiny"
    } else {
      # Check if extent is a character string
      if (length(extent) == 1 && is.character(extent)) {
        if (extent == "capture") {
          # Use entire extent of captured data
          ext <- terra::rast(files[["capture"]]) |>
            terra::ext()

          # Crop type
          crop <- "capture"
        } else {
          rlang::abort(
            "Invalid character extent specification. Use 'capture' or NULL."
          )
        }
      } else if (inherits(extent, "SpatExtent")) {
        # If a SpatExtent object is provided, use it directly
        ext <- terra::ext(extent)

        # Crop type
        crop <- "extent"
      } else {
        # Unexpected outputs
        rlang::abort(
          "Invalid extent specification. Must be NULL, 'capture', or a SpatExtent object."
        )
      }
    }
    # Get big roi = extent of the entire core
    big_roi <- terra::ext(ext)

    if (is.null(layers)) {
      # Keep layers NULL to signal we want all layers
      rasters <- files |>
        purrr::map(\(x) terra::rast(x, noflip = TRUE))

      # Use rasters directly without subsetting
      rasters_subset <- rasters
    } else {
      # If specific layers are requested, either from parameters or shiny input

      # Read SpatRasters
      rasters <- files |>
        purrr::map(\(x) terra::rast(x, noflip = TRUE))

      # Get band positions and subset
      band_position <- spectra_position(rasters[["capture"]], layers)

      # Subset bands in the SpatRasters
      rasters_subset <- rasters |>
        purrr::map(
          \(x)
            spectra_sub(
              raster = x,
              spectra_tbl = band_position
            )
        )
    }

    if (verbose == TRUE) {
      cli::cli_alert_info("{format(Sys.time())} Cropping rasters")
    }

    # Crop
    # If expected REFLECTANCE has the same extent as captured data
    if (terra::ext(ext) == terra::ext(rasters[["capture"]])) {
      if (verbose) {
        cli::cli_alert_info(
          "Original raster layers: {terra::nlyr(rasters[['capture']])}"
        )
        cli::cli_alert_info(
          "Subset raster layers: {terra::nlyr(rasters_subset[['capture']])}"
        )
        cli::cli_alert_info(
          "Are layer counts equal? {terra::nlyr(rasters[['capture']]) == terra::nlyr(rasters_subset[['capture']])}"
        )
      }

      # If no layer subsetting
      if (
        terra::nlyr(rasters[["capture"]]) ==
          terra::nlyr(rasters_subset[["capture"]])
      ) {
        rasters_cropped <- rasters
        # If subsetting layers
      } else {
        rasters_cropped <- purrr::map(rasters_subset, \(raster) {
          # Extract source information
          raster_src <- dirname(dirname(terra::sources(raster)))

          # Extract file name
          raster_name <- tools::file_path_sans_ext(
            basename(terra::sources(raster))
          )

          # Create products directory if it doesn't exist
          products_dir <- fs::path(raster_src, "products")
          if (!dir.exists(products_dir)) {
            dir.create(products_dir, recursive = TRUE)
          }

          # Construct default filename (without leading/trailing slashes)
          filename <- fs::path(
            raster_src,
            "products", # Remove slashes
            paste0(raster_name, "_cropped.tif")
          )

          if (verbose) {
            cli::cli_alert_info("Writing raster to: {filename}")
          }

          terra::writeRaster(
            raster,
            filename = filename,
            wopt = list(
              steps = terra::nlyr(raster) * terra::ncell(raster),
              overwrite = TRUE
            )
          )
        })
      }
      # If crop is needed
    } else {
      rasters_cropped <- purrr::map2(
        rasters_subset,
        types,
        \(x, y)
          raster_crop(
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
        create_reference_raster(
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
    reflectance <- create_normalized_raster(
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

    # Finally flip because of terra handling of unprojected rasters
    if (flip == TRUE) {
      if (verbose == TRUE) {
        cli::cli_alert_info("{format(Sys.time())} Flipping reflectance")
      }
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
      new_path <- fs::file_move(
        terra::sources(reflectance_flip),
        sub(
          pattern = "REFLECTANCE_flip",
          replacement = "REFLECTANCE_",
          x = terra::sources(reflectance_flip)
        )
      )

      # Get REFLECTANCE back
      reflectance <- terra::rast(new_path)
    } else if (verbose == FALSE) {
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

  if (verbose == TRUE) {
    cli::cli_alert_success("{format(Sys.time())} Finished")

    # Return reflectance
    return(reflectance)
  }
}



#' A standard workflow for typical analysis
#'
#' @param core output of shiny process, typically loaded in with terra::readRDS()
#' @param verbose verbose output - TRUE (default) or FALSE
#' @param smooth.win smoothing window
#'
#' @returns
#' @export
standard_workflow <- function(core,
                              verbose = TRUE,
                              smooth.win = NA){

  if(!dir.exists(core$directory)){
    message("Cannot find the directory. Choose the HSItools_core.rds file associated with this core")
    newdir <- dirname(file.choose())
    core$directory <- newdir
  }

  if(all(is.na(smooth.win))){
    smooth.win <- round(.2/core$distances$pixelRatio)
  }

  reflectance <- get_reflectance(core,verbose = verbose)

  #find the normalized reflectance file we need
  refl_file <- list.files(file.path(core$directory,"products"),pattern = "^REFLECTANCE.*[0-9]\\.*tif$",full.names = TRUE)

  #load that back in.
  refl <- terra::rast(refl_file[length(refl_file)])

  # Save the images... ------------------------------------------------------

  message("Creating images...")
  if(!dir.exists(file.path(core$directory,"photos"))){
    dir.create(file.path(core$directory,"photos"))
  }
  # create the RGB file
  message("Creating RGB image...")
  rgb <- refl |>
    stretch_raster_full(
      type = "RGB",
      filename = file.path(core$directory,"photos","fullImage_RGB.png"))

  #create the CIR file
  message("Creating CIR image...")
  CIR <- refl |>
    stretch_raster_full(
      type = "CIR",
      filename = file.path(core$directory,"photos","fullImage_CIR.png"))


  #create the NIR file
  message("Creating NIR image...")
  NIR <- refl |>
    stretch_raster_full(
      type = "NIR",
      filename = file.path(core$directory,"photos","fullImage_NIR.png"))


  #Process data through the ROIs
  rois <- terra::vect(core$analysisRegions)

  for(r in 1:nrow(rois)){

    #check to make sure it's not all zeroes
    if(sum(sum(terra::ext(rois[r,]))) == 0){
      next
    }

    roi <- try(terra::crop(refl,y = rois[r,]))
    if(is(roi,"try-error")){
      refl2 <- terra::vect(refl)
      roi <- try(terra::crop(refl2,y = rois[r,]))
    }
    if(is(roi,"try-error")){
      refl2 <- terra::rast(refl)
      roi <- try(terra::crop(refl2,y = rois[r,]))
    }
    if(is(roi,"try-error")){
      newExt <- terra::ext(rois[r,])
      roi <- try(terra::crop(refl,newExt))
    }

    if(is(roi,"try-error")){
      stop("Still having issue with the crop.")
    }

    if(!dir.exists(file.path(core$directory,"photos"))){
      dir.create(file.path(core$directory,"photos"))
    }

    names(roi) <- names(refl)
    rgb_roi <- roi |>
      stretch_raster_full(type = "RGB",
                          filename = file.path(core$directory,"photos",paste0("roi",r,"_rgb.png")))

    rgb_cir <- roi |>
      stretch_raster_full(type = "CIR",
                          filename = file.path(core$directory,"photos",paste0("roi",r,"_cir.png")))

    rgb_nir <- roi |>
      stretch_raster_full(type = "NIR",
                          filename = file.path(core$directory,"photos",paste0("roi",r,"_nir.png")))


    # Calculate indices
    rabd_max <- roi |>
      calculate_rabd(
        edges = c(590,730),
        trough = c(660:670),
        rabd_name = "rabd660670",
        rabd_type = "max",
        filename = file.path(core$directory,"products",paste0("rabd660670_roi",r)))

    ind <- list(rabd_max)

    #create downcore csv files



    # create plots
    names(ind) <- purrr::map_chr(ind,names)
    plotSpectralDashboard(core,
                          ind,
                          roi_i = r,
                          smooth.win = smooth.win,
                          output.file.path = file.path(core$directory,"products",paste0("roi",r,"-dashboard.pdf")))

  }
}
