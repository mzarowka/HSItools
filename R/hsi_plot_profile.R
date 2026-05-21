#' Plot a depth profile
#'
#' @family Plotting
#'
#' @param x A [tibble][tibble::tibble] with columns `position` and exactly one
#'   value column, as produced by [`hsi_extract_profile()`].
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal stratigraphic profile plot. Position is mapped to the
#' x-axis as the independent variable — ensuring that stats like
#' [`ggplot2::geom_smooth()`] work correctly — then [`ggplot2::coord_flip()`]
#' rotates the plot so that depth runs top-to-bottom visually.
#' [`ggplot2::scale_x_reverse()`] places shallow positions at the top.
#'
#' When profile unit metadata is present, tick labels include the unit suffix
#' such as `0 mm` or `1.5 cm`. If no unit metadata exists, ggplot2 default
#' labels are used, showing pixel coordinates or raw position values.
#'
#' The returned ggplot carries no theme or axis labels — add these with `+`
#' using standard ggplot2 conventions.
#'
#' @seealso
#' [`hsi_extract_profile()`] to produce the input tibble.
#' [`hsi_plot_spectrum()`] for spectral plots.
#' [`hsi_plot_spatraster()`] for spatial raster maps.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("RABD_testdata.tif") |> terra::subset(1)
#' x_profile <- hsi_extract_profile(x)
#'
#' # Quick pixel-space profile
#' x_plot <- hsi_plot_profile(x_profile)
#'
#' # Physical-space profile
#' um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
#' ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
#' x_cal <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
#' x_profile <- hsi_extract_profile(x_cal)
#' x_plot <- hsi_plot_profile(x_profile)
#'
#' # Add labels and theme with ggplot2
#' x_plot +
#'   ggplot2::labs(x = "Depth (cm)", y = "RABD") +
#'   ggplot2::theme_minimal()
#' }
#' 
#' @importFrom rlang .data
#'
#' @export
hsi_plot_profile <- function(x) {
  # Validate inputs
  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} is a {.class {class(x)}} not a data frame or tibble"
    )
  }

  var_name <- setdiff(names(x), "position")

  if (length(var_name) != 1) {
    cli::cli_abort(
      "{.arg x} must have exactly one value column beside {.val position}, not {.val {length(var_name)}}."
    )
  }

  units <- attr(x, "hsi_units", exact = TRUE)

  # Create ggplot object
  result <- ggplot2::ggplot(data = x) +
    ggplot2::aes(x = .data$position, y = .data[[var_name]]) +
    ggplot2::geom_line() +
    ggplot2::scale_x_reverse(
      labels = if (!is.null(units)) hsi_unit_label_fun(units) else ggplot2::waiver()
    ) +
    ggplot2::coord_flip()

  # Return result
  result
}
