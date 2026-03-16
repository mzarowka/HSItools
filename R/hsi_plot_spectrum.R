#' Plot a reflectance spectrum
#'
#' @family Plotting
#'
#' @param x A [tibble][tibble::tibble] with columns `wavelength` and `value`,
#'   as produced by [`hsi_extract_spectrum()`].
#'
#' @returns A [`ggplot2::ggplot`] object. Extend with `+` to add labels,
#'   themes, or colour scales.
#'
#' @details
#' Produces a minimal line plot of reflectance against wavelength. The
#' returned ggplot carries no theme or axis labels — add these with `+`
#' using standard ggplot2 conventions.
#'
#' @seealso
#' [`hsi_extract_spectrum()`] to produce the input tibble.
#' [`hsi_plot_profile()`] for 1-D depth profiles.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' x_spectrum <- hsi_extract_spectrum(x)
#'
#' # Quick plot
#' x_plot <- hsi_plot_spectrum(x_spectrum)
#'
#' # Add labels and theme with ggplot2
#' x_plot +
#'   ggplot2::labs(x = "Wavelength (nm)", y = "Reflectance") +
#'   ggplot2::theme_minimal()
#' }
#'
#' @importFrom rlang .data
#' 
#' @export
hsi_plot_spectrum <- function(
  x
) {
  # Validate inputs
  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} is a {.class {class(x)}} not a data frame or tibble"
    )
  }

  # Create ggplot object
  result <- ggplot2::ggplot(data = x) +
    ggplot2::aes(x = .data$wavelength, y = .data$value) +
    ggplot2::geom_line()

  # Return result
  result
}
