#' Extract value profile along an axis
#'
#' @family HSI Extraction
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param fun Character. Aggregation function passed to [`terra::aggregate()`].
#'   Default `"mean"`. Use `"modal"` for categorical data.
#' @param direction Character. Direction of profile extraction. Either `"vertical"`
#'   (aggregates across columns, profile along rows) or `"horizontal"` (aggregates
#'   across rows, profile along columns). Default `"vertical"`.
#' @param na.rm Logical. Remove `NA` values. Default `TRUE`.
#' @param y A [`SpatRaster`][terra::SpatRaster-class] with layers `row_um`
#'   and `col_um`. When provided, the `position` column is expressed in physical
#'   units rather than pixel coordinates. Default `NULL`.
#'
#' @returns A [tibble][tibble::tibble] with columns:
#'   \item{position}{Numeric. Position along the profile axis, in pixel coordinates
#'     or physical units when `y` is supplied or `x` carries unit metadata.}
#'   \item{...}{One column per input layer, named after band names.}
#'
#' @details
#' Aggregation is performed perpendicular to the profile direction using
#' [`terra::aggregate()`]. Crop `x` with [`terra::crop()`] before calling this
#' function to restrict extraction to a region of interest.
#'
#' When `y` is provided, the relevant coordinate layer (`row_um` for vertical,
#' `col_um` for horizontal) is aggregated with `fun = "mean"` independently of
#' `fun`, as physical position is a geometric property not a statistical summary.
#'
#' Band names conflicting with reserved column names (`"x"`, `"y"`,
#' `"position"`) are prefixed with `"band_"` and a warning is emitted.
#'
#' @seealso
#' [`hsi_extract_spectrum()`] for extracting an averaged spectrum.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("RABD_index.tif")
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#'
#' # Pixel-space profile
#' x_profile <- hsi_extract_profile(x)
#'
#' # Physical-space profile
#' ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
#' x_cal <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
#' x_profile <- hsi_extract_profile(x_cal)
#'
#' # Region of interest
#' x_profile <- x |>
#'   terra::crop(my_extent) |>
#'   hsi_extract_profile()
#'
#' # Horizontal profile
#' x_profile <- hsi_extract_profile(x_cal, direction = "horizontal")
#'
#' # Classified raster
#' x_class <- terra::rast("classified.tif")
#' x_profile <- hsi_extract_profile(x_class, fun = "modal")
#' }
#'
#' @export
hsi_extract_profile <- function(
  x,
  fun = "mean",
  direction = "vertical",
  na.rm = TRUE,
  y = NULL
) {
  # Validate input
  check_spatraster(x)

  check_crs_null(x)

  # Check alowed direction types
  check_one_of(direction, c("vertical", "horizontal"))

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
    agg_factor <- c(1, terra::ncol(x))

    # Set positioning column
    position_col <- "y"
  } else {
    # Aggregate across rows, keep columns
    agg_factor <- c(terra::nrow(x), 1)

    # Set positioning column
    position_col <- "x"
  }

  # Aggregate x perpendicular to profile direction
  x_agg <- terra::aggregate(
    x,
    fact = agg_factor,
    fun = fun,
    na.rm = na.rm
  ) |>
    # Coerce to data frame with coordinates
    terra::as.data.frame(xy = TRUE) |>
    # Keep terra coordinate as join key (rename deferred)
    dplyr::select(
      dplyr::all_of(position_col),
      dplyr::all_of(band_names)
    ) |>
    # Coerce tibble
    tibble::as_tibble()

  # Replace pixel position with physical coordinates
  if (!is.null(y)) {
    # Validate input
    check_spatraster(y)

    check_crs_null(y)

    # Validate SpatRaster layers
    check_list_has(
      terra::as.list(y) |> stats::setNames(terra::names(y)),
      elements = c("row_um", "col_um")
    )

    # Subset y
    if (direction == "vertical") {
      lyr <- "row_um"
    } else {
      lyr <- "col_um"
    }

    y_agg <- y |>
      terra::subset(lyr) |>
      terra::aggregate(
        fact = agg_factor,
        fun = "mean",
        na.rm = na.rm
      ) |>
      # Coerce to data frame with coordinates
      terra::as.data.frame(xy = TRUE) |>
      # Keep terra coordinate as join key plus physical position
      dplyr::select(
        dplyr::all_of(position_col),
        position = dplyr::all_of(lyr)
      ) |>
      # Coerce tibble
      tibble::as_tibble()

    x_agg <- x_agg |>
      dplyr::left_join(y_agg, by = position_col) |>
      dplyr::select(-dplyr::all_of(position_col))
  } else {
    # No physical coords — rename terra coordinate to position
    x_agg <- x_agg |>
      dplyr::rename(position = dplyr::all_of(position_col))
  }

  units <- hsi_get_units(y)
  if (is.null(units)) {
    units <- hsi_get_units(x)
  }

  attr(x_agg, "hsi_units") <- units

  # Return result
  x_agg
}
