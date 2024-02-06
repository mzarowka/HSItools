#' Create SpatVector from Shiny ROIs
#'
#' @param data run_core() output with ROIs.
#'
#' @return SpatVector object suitable for plotting and setting extents.
#' @export
roi_to_vect <- function(data) {
  # Extract data frame with ROIs
  data <- data[["analysisRegions"]]

  # Create polygons
  rois_vect <- data |>
    # Add grouping variable
    dplyr::mutate(
      roi.id = paste0("ROI_", 1:dplyr::n()),
      .before = 1) |>
    # Group by
    dplyr::group_by(roi.id) |>
    # Split
    dplyr::group_split() |>
    # Set names
    purrr::set_names(nm = paste0("ROI_", 1:nrow(data))) |>
    # Drop id
    purrr::map(\(i) dplyr::select(i, -roi.id)) |>
    # Pivot X
    purrr::map(\(i) tidyr::pivot_longer(i, xmin:xmax, names_to = "xcor", values_to = "v1")) |>
    # Pivot Y
    purrr::map(\(i) tidyr::pivot_longer(i, ymin:ymax, names_to = "ycor", values_to = "v2")) |>
    # Close polygon - duplicate first vertex
    purrr::map(\(i) tibble::add_row(i, dplyr::slice_head(i, n = 1))) |>
    # Select only x and y
    purrr::map(\(i) dplyr::select(i, v1, v2)) |>
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
    dplyr::rename(geometry = x) |>
    # To one sf
    sf::st_as_sf() |>
    # To SpatVector
    terra::vect()

  # Return SpatVector
  return(rois_vect)
}

#' Get depth in metric units
#'
#' @param pixel_ratio A source of conversion factor, either run_core() output or manually input.
#' @param .unit metric unit to convert into, defaults to "mm". Accepts also "cm" and "um".
#' @param .sample_start position of the sample beginning, from run_core().
#' @param .sample_end position of the sample end, from run_core().
#'
#' @return lookup table with depths.
#' @export
px_to_metric <- function(pixel_ratio = NULL, .unit = "mm", .sample_start = NULL, .sample_end = NULL) {


}
