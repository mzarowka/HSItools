#' Find position of selected spectra
#'
#' @family Utilities
#' @param raster a terra SpatRaster.
#' @param spectra vector with choice of desired spectra.
#'
#' @return positions (indices) of desired spectra in SpatRaster
#' @export
#'
#' @description find index position of the nearest spectra (band) in the dataset.
#' Match for the lowest difference between integer band and actual SpatRaster band.
#' This will produce duplicates with multiple bands. Drop.
spectra_position <- function(
  raster,
  spectra
) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Find index (position) of selected spectra by comparing choice and names
  spectraIndex <- purrr::map(
    spectra,
    \(x) terra::which.min(abs(x - as.numeric(terra::names(raster))))
  ) |>
    # Get positions
    purrr::as_vector()

  # Create tibble with spectra of choice and respective position
  spectraIndex <- dplyr::tibble(
    spectra = spectra,
    position = spectraIndex
  ) |>
    # Keep second observation if duplicates are present
    # From experience closer to desired product
    dplyr::slice_tail(by = .data$position)

  # Return values
  return(spectraIndex)
}


#' Subset SpatRaster by spectra
#'
#' @family Utilities
#' @param raster a terra SpatRaster to be subset.
#' @param spectra_tbl a tibble with spectra positions from spectra_position.
#'
#' @return SpatRaster subset to contain only required spectral bands.
#' @export
#'
#' @description subset SpatRaster with spectra (bands) positions.
spectra_sub <- function(
    raster,
    spectra_tbl) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Get spectra from tibble
  spectra <- dplyr::pull(spectra_tbl, 1)

  # Get positions from tibble
  position <- dplyr::pull(spectra_tbl, 2)

  # Subset raster by position
  raster <- terra::subset(raster, position)

  # Set raster names to match spectra
  # terra::names(raster) <- as.character(spectra)

  # Return raster
  return(raster)
}

#' Create SpatVector from Shiny ROIs
#'
#' @family Utilities
#' @param data \code{\link{run_core}} output with ROIs.
#' @export
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
      roi.id = paste0("ROI_", 1:terra::nrow(data)),
      .before = 1
    ) |>
    # Group by
    dplyr::group_by(.data$roi.id) |>
    # Split
    dplyr::group_split() |>
    # Set names
    purrr::set_names(nm = paste0("ROI_", 1:terra::nrow(data))) |>
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
    purrr::map(\(i) terra::as.matrix(i)) |>
    # Create polygon
    purrr::map(\(i) sf::st_polygon(list(i))) |>
    # Polygon is intersecting, get bounding box
    purrr::map(\(i) sf::st_bbox(i)) |>
    # Coerce to sfc
    purrr::map(\(i) sf::st_as_sfc(i)) |>
    # Coerce to sf
    purrr::map(\(i) sf::st_as_sf(i)) |>
    # Set names
    purrr::set_names(nm = paste0("ROI_", 1:terra::nrow(data))) |>
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
#' @family Utilities
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
pixel_to_distance <- function(
    core,
    pixel_ratio,
    ymax,
    ymin = 0,
    sample_start,
    sample_end,
    extent = NULL) {

  # Check if only one argument is provided
  rlang::check_exclusive(core, pixel_ratio, .require = TRUE)

  # Calculate mm distance and depths

  # Using run_core output
  if (is.null(core) == FALSE) {
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
#' Merge SpatRasters in a stratigraphic order
#'
#' @family Utilities
#' @param raster_1 a terra SpatRaster. First in the sequence.
#' @param raster_2 a terra SpatRaster. Second in the sequence.
#' @param filename a path to save file (with extension). Defaultys to NULL and processing in memory.
#'
#' @return a terra SpatRaster. Merged inputs.
#' @export
merge_rasters <- function(raster_1, raster_2, filename = NULL){
  # Check if correct class is supplied.
  if (!inherits(raster_1, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Check if correct class is supplied.
  if (!inherits(raster_2, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Get extent of the first SpatRaster
  extent_1 <- terra::ext(raster_1)

  # Get extent of the second SpatRaster
  extent_2 <- terra::ext(raster_2)

  # Shift second extent
  # xmin and xmax stay the same
  extent_2 <- terra::ext(extent_2[1], extent_2[2], extent_1[3] - (extent_2[4] - extent_2[3]), extent_1[3])

  # Update extent of second SpatRaster
  terra::ext(raster_2) <- extent_2

  # Get merged SpatRaster
  raster <- terra::merge(raster_1, raster_2, filename = filename)

  # Return
  raster
}

#' Find a fixed-width extent in the middle of a larger one
#'
#' @family Utilities
#' @param extent a terra SpatExtent in which to look, created with terra::ext()
#' @param width number of pixels of a new SpatExtent.
#'
#' @return a terra SpatExtent centered within the original SpatExtent and of a given width.
#' @export
find_fixed_extent <- function(extent, width){
    # Check if correct class is supplied.
    if (!inherits(extent, what = "SpatExtent")) {
      rlang::abort(message = "Supplied data is not a terra SpatExtent.")
    }

  # Get mid point
  middle_point <- round((terra::xmax(extent) - terra::xmin(extent)) / 2)

  # New xmin
  ext.xmin <- terra::xmin(extent) + middle_point - (width / 2)

  # New xmax
  ext.xmax <- terra::xmin(extent) + middle_point + (width / 2)

  # Update extent
  extent <- terra::ext(ext.xmin, ext.xmax, extent[3], extent[4])

  # Return
  extent
}