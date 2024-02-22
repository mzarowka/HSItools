#' Create SpatVector from Shiny ROIs
#'
#' @param data \code{\link{run_core}} output with ROIs.
#'
#' @return SpatVector object suitable for plotting and setting extents.
roi_to_vect <- function(data) {

  # Probably can do it quicker by bounding box of the points
  # Remove some redundancies
  # Create polygons
  data <- data |>
    # Add grouping variable
    dplyr::mutate(
      roi.id = paste0("ROI_", 1:dplyr::n()),
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

  # Return SpatVector
  return(data)
}

#' Get depth in metric units
#'
#' @param core \code{\link{run_core}} output. If provided fills pixel_ratio, sample_start and sample_end. Exclusive with pixel_ratio.
#' @param pixel_ratio a source of conversion factor, manually input. Exclusive with pixel_ratio.
#' @param unit metric unit to convert into, defaults to "mm". Accepts also "cm" and "um".
#' @param sample_start position of the sample beginning, either from \code{\link{run_core}} output or manually input.
#' @param sample_end position of the sample end, either from \code{\link{run_core}} output or manually input.
#' @param extent a terra extent or terra SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#'
#' @return lookup table with depths.
#' @export
pixel_to_distance <- function(core, pixel_ratio, unit = "mm", sample_start, sample_end, extent = NULL) {
  # Here check if optional core is shiny output-like, S3 class
  # if (!inherits(core, what = "CLASS-HERE")) {
  #   rlang::abort(message = "Supplied \"core\" name is not a valid output of run_core().")
  # }

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

    # Get depth
    pixel_depth <- "dupa"
  } else {
    depth <- "dupa"
  }
}
