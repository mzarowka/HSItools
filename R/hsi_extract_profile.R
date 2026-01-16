#' Extract value profile along an axis
#'
#' @family HSI Extraction
#'
#' @param x A terra SpatRaster with single or multiple bands
#' @param direction Character. Direction of profile extraction. Either
#'   "vertical" (default, profile along Y-axis) or "horizontal" (profile along X-axis)
#' @param fun Character. Aggregation function passed to \code{\link[terra]{aggregate}}.
#'   Default "mean". Use "modal" for categorical/classified data.
#'
#' @return A tibble with columns:
#'   \item{position}{Numeric. Pixel coordinate along the profile axis}
#'   \item{...}{Additional columns, one per input band, named after band names}
#'
#' @description
#' Aggregate a raster perpendicular to the specified direction, producing a
#' 1D profile of values. Commonly used for extracting downcore profiles from
#' sediment core scans.
#'
#' @details
#' The function aggregates pixels perpendicular to the profile direction:
#' - `"vertical"`: aggregates across columns (X), returns profile along rows (Y)
#' - `"horizontal"`: aggregates across rows (Y), returns profile along columns (X)
#'
#' For classified/categorical rasters, use `fun = "modal"` to get the most
#' frequent class at each position.
#'
#' If you need a profile from a specific region, crop the raster first with
#' \code{\link[terra]{crop}}. To convert pixel positions to real-world units,
#' use \code{\link{hsi_pixels_to_units}} afterward.
#'
#' @seealso
#' \code{\link{hsi_extract_spectrum}} for extracting averaged spectra,
#' \code{\link{hsi_pixels_to_units}} for converting positions to depth units
#'
#' @examples
#' \dontrun{
#' # Load calculated index
#' x <- terra::rast("RABD_index.tif")
#'
#' # Extract vertical profile (downcore)
#' profile <- hsi_extract_profile(x)
#'
#' # Extract from specific region
#' profile_roi <- x |>
#'   terra::crop(my_extent) |>
#'   hsi_extract_profile()
#'
#' # Horizontal profile
#' profile_h <- hsi_extract_profile(x, direction = "horizontal")
#'
#' # Multi-band input returns one column per band
#' multi <- terra::rast(c("RABD.tif", "RABA.tif"))
#' profiles <- hsi_extract_profile(multi)
#'
#' # For classified raster, use modal
#' # Load calculated index
#' x_class <- terra::rast("classified.tif")
#'
#' # Extract vertical profile (downcore)
#' profile_class <- hsi_extract_profile(classification, fun = "modal")
#' }
#'
#' @export
hsi_extract_profile <- function(
  x,
  direction = "vertical",
  fun = "mean"
) {
  # Validate input
  check_spatraster(x)

  # Validate direction
  direction <- match.arg(direction, c("vertical", "horizontal"))

  # Get band names and check for conflicts with coordinate columns
  band_names <- terra::names(x)
  reserved <- c("x", "y", "position")
  conflicts <- band_names[band_names %in% reserved]

  if (length(conflicts) > 0) {
    cli::cli_warn(
      c(
        "Band names conflict with coordinate columns.",
        "i" = "Conflicting names: {.val {conflicts}}",
        "i" = "Prefixing with 'band_' to avoid conflicts."
      )
    )

    # Rename conflicting bands
    band_names <- dplyr::if_else(
      band_names %in% reserved,
      paste0("band_", band_names),
      band_names
    )
    names(x) <- band_names
  }

  # Set aggregation factor based on direction
  if (direction == "vertical") {
    # Aggregate across columns, keep rows
    agg_fact <- c(1, terra::ncol(x))
    position_col <- "y"
  } else {
    # Aggregate across rows, keep columns
    agg_fact <- c(terra::nrow(x), 1)
    position_col <- "x"
  }

  # Aggregate perpendicular to profile direction
  profile <- terra::aggregate(
    x,
    fact = agg_fact,
    fun = fun,
    na.rm = TRUE
  ) |>
    # Coerce to data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # To tibble
    dplyr::tibble() |>
    # Select position and all band columns
    dplyr::select(
      position = dplyr::all_of(position_col),
      dplyr::all_of(band_names)
    )

  # Return
  return(profile)
}
