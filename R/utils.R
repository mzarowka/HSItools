# Split job by ROIs
split_by_roi <- function(core, roi){
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }
}

#' Create SpatVector from Shiny ROIs
#'
#' @param data \code{\link{run_core}} output with ROIs.
#'
#' @return SpatVector object suitable for plotting and setting extents.
roi_to_vect <- function(data) {
  # Check number of ROIs
  if (sum(is.na(data)) > 0) {
  data <- data

  } else {
  # Probably can do it quicker by bounding box of the points
  # Remove some redundancies
  # Create polygons
  data <- data |>
    # Add grouping variable
    dplyr::mutate(
      roi.id = paste0("ROI_", 1:nrow(data)),
      .before = 1
    ) |>
    # Group by
    dplyr::group_by(.data$roi.id) |>
    # Split
    dplyr::group_split() |>
    # Set names
    purrr::set_names(nm = paste0("ROI_", 1:nrow(data))) |>
    # Drop id
    purrr::map(\(i) dplyr::select(i, -.data$roi.id)) |>
    # Pivot X
    purrr::map(\(i) tidyr::pivot_longer(
      i,
      .data$xmin:.data$xmax,
      names_to = "xcor",
      values_to = "v1"
    )) |>
    # Pivot Y
    purrr::map(\(i) tidyr::pivot_longer(
      i,
      .data$ymin:.data$ymax,
      names_to = "ycor",
      values_to = "v2"
    )) |>
    # Close polygon - duplicate first vertex
    purrr::map(\(i) tibble::add_row(
      i,
      dplyr::slice_head(i, n = 1)
    )) |>
    # Select only x and y
    purrr::map(\(i) dplyr::select(i, .data$v1, .data$v2)) |>
    # To matrix for polygon
    purrr::map(\(i) as.matrix(i)) |>
    # Create polygon
    purrr::map(\(i) sf::st_polygon(list(i))) |>
    # Polygon is intersecting, get bounding box
    purrr::map(\(i) sf::st_bbox(i)) |>
    # Coerce to sfc
    purrr::map(\(i) sf::st_as_sfc(i)) |>
    # Coerce to sf
    purrr::map(\(i) sf::st_as_sf(i)) |>
    # Set names
    purrr::set_names(nm = paste0("ROI_", 1:nrow(data))) |>
    # Bind by row
    purrr::list_rbind(names_to = "roi.id") |>
    # Rename
    dplyr::rename(geometry = .data$x) |>
    # To one sf
    sf::st_as_sf()
  }

  # Return SpatVector
  return(data)
}

#' Get depth in metric units
#'
#' @param core \code{\link{run_core}} output. If provided fills pixel_ratio, sample_start and sample_end. Exclusive with pixel_ratio.
#' @param pixel_ratio a source of conversion factor, manually input. Exclusive with pixel_ratio.
#' @param ymax pixel value of the top.
#' @param ymin pixel value of the bottom, default to 0.
#' @param sample_start position of the sample beginning (point zero), either from \code{\link{run_core}} output or manually input.
#' @param sample_end position of the sample end, either from \code{\link{run_core}} output or manually input.
#' @param extent a terra extent or terra SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return lookup table with depths.
#' @export
pixel_to_distance <- function(core, pixel_ratio, ymax, ymin = 0, sample_start, sample_end, extent = NULL) {

  # Check if only one argument is provided
  rlang::check_exclusive(core, pixel_ratio, .require = TRUE)

  # Calculate mm distance and depths

  # Using run_core output
  if (is.null(core) == FALSE) {
    # Here check if optional core is shiny output-like, S3 class
    # if (!inherits(core, what = "CLASS-HERE")) {
    #   rlang::abort(message = "Supplied \"core\" name is not a valid output of run_core().")
    # }

    # Set core to run_core output
    core <- core

    # Extract pixel ratio
    pixel_ratio <- core$distances$pixelRatio

    # Extract sample_start
    sample_start <- core$distances$startCore

    # Extract sample_end
    sample_end <- core$distances$endCore

    # Extract full extent of the captured data
    extent <- terra::ext(core$simpleRGB$ext)

    # Get the full capture distance
    distance <- (terra::ymax(extent) - terra::ymin(extent)) * (pixel_ratio)
  } else {

    # Get the full capture distance
    distance <- (ymax - ymin) * (pixel_ratio)
  }

  # Reverse values, get metric zero at the capture top
  capture_top <- c(y = (terra::ymax(extent) * pixel_ratio) - distance)

  # Reverse values, get metric max at the capture bottom
  capture_bottom <- c(y = (terra::ymin(extent) * pixel_ratio) + distance)

  # Get the metric point of the sample beginning
  point_zero <- capture_top - (sample_start[2] * pixel_ratio) + distance

  # Return
  return(list(
    distance = distance,
    capture_top = capture_top,
    capture_bottom = capture_bottom,
    point_zero = point_zero,
    pixel_ratio = pixel_ratio))
}


#' Adjust paths from Shiny output
#'
#' @param run_core_output
#'
#' @return run_core_output
#' @export
#'
#' @examples
#' if (interactive() == TRUE) {
#' a1 <- readRDS(file.path(system.file(package = "HSItools"),"extdata/HSItools_core.rds"))
#' change_output_dir(a1)
#' }
#'
change_output_dir = function(run_core_output){
  #currentRoot <- rprojroot::find_root(rprojroot::criteria$is_rstudio_project)
  currentRoot <- getwd()
  shinyRoot <- run_core_output$directory
  if (currentRoot == shinyRoot){
    message("Current path is consistent with with Shiny output!")
  } else {
    #Set the paths in the core output to mesh with the user's current root
    newDir <- choose.dir(caption = paste0("Find the directory with the name: ", basename(run_core_output$directory)))
    if (basename(run_core_output$directory) != basename(newDir)){
      rlang::abort("The directory names must match!")
    } else {
      run_core_output$directory <- newDir
      regex1 <- paste0(".*", basename(newDir))
      fileNames <- gsub(regex1,"", run_core_output$rasterPaths)
      fullPaths <- file.path(newDir, fileNames)
      run_core_output$rasterPaths <- fullPaths
    }
  }
  return(run_core_output)
}

# Function to run other function and then move output, useful for mapping over rois
get_and_move <- function(fun, name, ...) {
  parameters <- list(...)

  if (fun == prepare_core()) {
    object <- prepare_core(core = parameters$core, path = parameters$path, layers = parameters$layers, extent = parameters$extent, normalize = parameters$normalize, integration = integration)
  }

  object_source <- terra::sources(object)

  object_destination <- sub(pattern = "products/", replacement = paste0("products/", name)) |>
    sub(pattern = ".tif", replacement = paste0("_", name, ".tif"))

  fs::file_move(object_source, object_destination)

}
