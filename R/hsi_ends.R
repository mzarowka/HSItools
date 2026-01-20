#' Extract sample boundary positions from reference points
#'
#' @family HSI Calibration
#'
#' @param x A terra SpatVector containing exactly 2 points defining sample
#'   boundaries. Must have required attributes (see Details).
#'
#' @description
#' Extract and validate sample boundary points with their physical position
#' metadata. This is the second step in spatial calibration, establishing
#' where the sample starts and ends in both pixel and physical coordinates.
#'
#' @details
#' The input SpatVector must:
#' - Contain exactly 2 points
#' - Have no CRS (pixel coordinates)
#' - Include required attributes in the schema
#'
#' **Required attributes:**
#' \describe{
#'   \item{label}{Character. Must be "start" or "end"}
#'   \item{physical_position}{Numeric. Scale/tape reading at this point}
#' }
#'
#' **Optional attributes:**
#' \describe{
#'   \item{sample_position}{Numeric. Position in sample coordinates
#'     (e.g., 0 at sample top)}
#'   \item{splice_position}{Numeric. Position in spliced/composite sequence}
#' }
#'
#' @return A list containing:
#'   \item{start}{List with pixel_x, pixel_y, physical_position, and optional
#'     sample_position and splice_position (NULL if not provided)}
#'   \item{end}{List with same structure as start}
#'   \item{geometry}{SpatVector. Original input geometry for provenance}
#'
#' @seealso
#' \code{\link{hsi_scale}} for extracting pixel-to-unit ratio,
#' \code{\link{hsi_spatial_calibration}} for combining scale and ends
#'
#' @examples
#' \dontrun{
#' # Load ends from geopackage
#' ends_pts <- terra::vect("calibration.gpkg", layer = "ends")
#'
#' # Extract boundary information
#' ends_info <- hsi_ends(ends_pts)
#'
#' # Access start position
#' ends_info$start$physical_position
#' ends_info$start$pixel_y
#' }
#'
#' @export
hsi_ends <- function(
  x
) {
  # Validate input
  check_spatvector(x)

  # Check the CRS
  check_crs_null(x)

  # Validate geometry type
  check_geom_type(x, allowed = "points")

  # Validate point count
  if (nrow(x) != 2) {
    cli::cli_abort(
      "{.arg x} must contain exactly 2 points, not {nrow(x)}."
    )
  }

  # Validate required schema
  check_has_cols(x, cols = c("label", "physical_position"))

  # Validate labels are exactly "start" and "end"
  labels <- x$label

  if (!setequal(labels, c("start", "end"))) {
    cli::cli_abort(
      "{.arg x} must have exactly one {.val start} and one {.val end} label, not {.val {labels}}."
    )
  }

  # Validate physical_position is numeric with no NAs
  if (!is.numeric(x$physical_position) || anyNA(x$physical_position)) {
    cli::cli_abort(
      "Column {.val physical_position} must be numeric with no missing values."
    )
  }

  # Validate optional columns if present
  # Get the columns
  optional_cols <- c("sample_position", "splice_position")

  # Run validation
  purrr::walk(optional_cols, \(col) {
    if (col %in% terra::names(x)) {
      vals <- x[[col]]
      if (!is.numeric(vals) || anyNA(vals)) {
        cli::cli_abort(
          "Column {.val {col}} must be numeric with no missing values."
        )
      }
    }
  })

  # Build data frame with coordinates
  x_df <- terra::as.data.frame(x, geom = "xy") |>
    dplyr::rename(pixel_x = .data$x, pixel_y = .data$y)

  # Helper to build single endpoint list
  build_endpoint <- \(df) {
    list(
      pixel_x = df$pixel_x,
      pixel_y = df$pixel_y,
      physical_position = df$physical_position,
      sample_position = purrr::pluck(df, "sample_position", .default = NULL),
      splice_position = purrr::pluck(df, "splice_position", .default = NULL)
    )
  }

  # Split by label and build "endpoints"
  endpoints <- x_df |>
    split(x_df$label) |>
    purrr::map(build_endpoint)

  # Return with geometry (with the correct start/end order)
  list(
    start = endpoints$start,
    end = endpoints$end,
    geometry = x
  )
}
