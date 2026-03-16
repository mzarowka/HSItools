#' Plot a depth profile
#'
#' @family Plotting
#'
#' @param x A [tibble][tibble::tibble] with columns `position` and exactly one
#'   value column, as produced by [`hsi_extract_profile()`].
#' @param physical Logical. When `TRUE`, negates x-axis tick labels to display
#'   positive physical distances. Set `TRUE` when `x` was extracted with a
#'   coordinate raster supplied to [`hsi_extract_profile()`]. Default `FALSE`.
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
#' When `physical = TRUE`, `labels = \(i) -i` is applied to
#' [`ggplot2::scale_x_reverse()`] to strip the negation introduced by the
#' coordinate system, so that axes read as positive physical distances.
#' When `physical = FALSE`, ggplot2 default labels are used, showing pixel
#' coordinates or raw position values.
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
#' x_coords <- hsi_calc_coords(x, um_per_pixel = um)
#' x_profile <- hsi_extract_profile(x, y = x_coords)
#' x_plot <- hsi_plot_profile(x_profile, physical = TRUE)
#'
#' # Add labels and theme with ggplot2
#' x_plot +
#'   ggplot2::labs(x = "Depth (mm)", y = "RABD") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @export
hsi_plot_profile <- function(
  x,
  physical = FALSE
) {
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

  # Create ggplot object
  result <- ggplot2::ggplot(data = x) +
    ggplot2::aes(x = .data$position, y = .data[[var_name]]) +
    ggplot2::geom_line() +
    ggplot2::scale_x_reverse(
      labels = if (physical) \(i) -i else ggplot2::waiver()
    ) +
    ggplot2::coord_flip()

  # Return result
  result
}
