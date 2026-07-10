#' Match ground control points between two SpatVectors
#'
#' @family HSI Co-registration
#'
#' @param source A [`SpatVector`][terra::SpatVector-class] of points with
#'   source GCPs. Must contain an identifier column.
#' @param target A [`SpatVector`][terra::SpatVector-class] of points with
#'   target GCPs. Must contain an identifier column.
#' @param id_col Character. Column name containing GCP identifiers.
#'   Default `"gcp_id"`.
#'
#' @returns A [tibble][tibble::tibble] with columns:
#'   \item{gcp_id}{GCP identifier, or column named by `id_col`.}
#'   \item{source_x}{X pixel coordinate in source raster.}
#'   \item{source_y}{Y pixel coordinate in source raster.}
#'   \item{target_x}{X pixel coordinate in target raster.}
#'   \item{target_y}{Y pixel coordinate in target raster.}
#'
#' @description
#' Match ground control points from source and target vectors by a shared
#' identifier. CRS is stripped from both inputs to work in pixel coordinate
#' space. Points present in only one input are dropped with a warning.
#'
#' @seealso
#' [`hsi_check_gcp()`] for assessing transformation quality,
#' [`hsi_coregister()`] for warping.
#'
#' @examples
#' \dontrun{
#' # Get SWIR GCPs
#' swir_gcps <- terra::vect("swir_preview.gpkg", layer = "gcp")
#'
#' # Get VNIR GCPs
#' vnir_gcps <- terra::vect("vnir_preview.gpkg", layer = "gcp")
#'
#' # See if there is a match between the GCPs
#' matched <- hsi_match_gcp(swir_gcps, vnir_gcps)
#' }
#'
#' @export
hsi_match_gcp <- function(
  source,
  target,
  id_col = "gcp_id"
) {
  # Validate inputs
  check_spatvector(source)

  check_spatvector(target)

  # Validate geometries
  check_geom_type(source, allowed = "points")

  check_geom_type(target, allowed = "points")

  # Validate columns
  check_has_cols(source, cols = id_col)

  check_has_cols(target, cols = id_col)

  # Strip CRS
  # For example, in new projects QGIS assigns WGS84 to pixel-space data by default
  source <- hsi_drop_crs(source)
  target <- hsi_drop_crs(target)

  # Extract coordinates and ID
  # From source
  source_df <- terra::as.data.frame(source, geom = "xy") |>
    dplyr::select(
      dplyr::all_of(id_col),
      source_x = "x",
      source_y = "y"
    ) |>
    # Excplicit tibble
    tibble::as_tibble()

  # From target
  target_df <- terra::as.data.frame(target, geom = "xy") |>
    dplyr::select(
      dplyr::all_of(id_col),
      target_x = "x",
      target_y = "y"
    ) |>
    # Excplicit tibble
    tibble::as_tibble()

  # Check for duplicated IDs in both source and target
  # In source
  source_dups <- source_df[[id_col]][duplicated(source_df[[id_col]])]

  # In target
  target_dups <- target_df[[id_col]][duplicated(target_df[[id_col]])]

  # Validate and abort if there are duplicates in source
  if (length(source_dups) > 0) {
    cli::cli_abort(
      "Duplicate {.val {id_col}} in {.arg source}: {.val {unique(source_dups)}}.",
      class = "hsitools_error"
    )
  }

  # Validate and abort if there are duplicates in target
  if (length(target_dups) > 0) {
    cli::cli_abort(
      "Duplicate {.val {id_col}} in {.arg target}: {.val {unique(target_dups)}}.",
      class = "hsitools_error"
    )
  }

  # Match both tibbles by id_col
  matched <- dplyr::inner_join(source_df, target_df, by = id_col)

  # Check minimum of 3 GCP
  if (nrow(matched) < 3) {
    cli::cli_abort(
      "Need at least 3 matched GCPs, found {nrow(matched)}.",
      class = "hsitools_error"
    )
  }

  # Report dropped GCPs
  # In source
  n_source_only <- nrow(source_df) - nrow(matched)

  # In target
  n_target_only <- nrow(target_df) - nrow(matched)

  # Warning on dropped (GCP only in one data frame)
  if (n_source_only > 0 || n_target_only > 0) {
    cli::cli_warn(
      "Dropped unmatched GCPs: {n_source_only} source-only, {n_target_only} target-only.",
      class = "hsitools_warning"
    )
  }

  # Return
  matched
}
