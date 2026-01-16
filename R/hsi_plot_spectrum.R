#' Line plot of spectral profile from the ROI
#'
#' @family Plotting
#' @param x Reflectance SpatRaster.
#' @param extent extent to work over.
#' @param ... other arguments.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_profile <- function(
  x,
  extent = NULL,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Clean data
  data <- x |>
    HSItools::extract_spectral_profile() |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "Wavelength.nm",
      names_transform = as.numeric,
      values_to = "Reflectance"
    )

  # Create a plot
  plot <- data |>
    # Pass to plot
    ggplot2::ggplot() +
    # Add aes
    ggplot2::aes(
      x = .data$Wavelength.nm,
      y = .data$Reflectance
    ) +
    # Add geom
    ggplot2::geom_line() +
    # Modify theme
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.line.y.left = ggplot2::element_line(color = "black"),
      axis.line.x.bottom = ggplot2::element_line(color = "black")
    ) +
    # Add labels
    ggplot2::labs(
      x = "Wavelength (nm)",
      y = "Reflectance"
    )

  # Return plot as an object
  return(plot)
}
