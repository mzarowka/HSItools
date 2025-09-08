#' Plot spatial map plots of calculated proxies, and optionally save to file
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param ... additional arguments.
#'
#' @importFrom ggplot2 theme
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_proxy <- function(
  x,
  index,
  calibration = NULL,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Subset SpatRaster
  hsi_layer <- x |>
    terra::subset(index)

  if (is.null(calibration)) {
    # Plot SpatRaster
    plot <- ggplot2::ggplot() +
      # Add raster layer
      tidyterra::geom_spatraster(data = hsi_layer) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = index,
        y = "Depth (px)",
        fill = "Value"
      )
  } else {
    # Plot SpatRaster
    plot <- ggplot2::ggplot() +
      # Add raster layer
      tidyterra::geom_spatraster(data = hsi_layer) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) {
          format(
            terra::round(
              -1 *
                i *
                calibration$pixel_ratio +
                calibration$distance -
                calibration$point_zero
            )
          )
        },
        breaks = scales::breaks_pretty()
      ) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = index,
        y = "Depth (mm)",
        fill = "Value"
      )
  }

  # Return plot as an object
  return(plot)
}

#' Spatial map plots of RGB image
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers or just RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param ... additional arguments.
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_rgb <- function(
  x,
  calibration = NULL,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Check if there are values close to RGB, within the 25 nm.
  if (
    all(
      any(
        purrr::list_c(
          purrr::map(
            c(640, 545, 460),
            \(i) dplyr::near(i, as.numeric(terra::names(x)), tol = 25)
          )
        )
      )
    ) ==
      TRUE
  ) {
    spectra <- c(640, 545, 460)
  } else {
    cli::cli_alert_warning(
      "No layers matching the RGB. Using the first, middle and last available layers."
    )
    wavelength <- c(
      min(1:terra::nlyr(x)),
      terra::median(1:terra::nlyr(x)),
      max(1:terra::nlyr(x))
    ) |>
      (\(i) as.numeric(terra::names(terra::subset(x, i))))()
  }

  # Prepare SpatRaster
  x <- HSItools::wavelength_position(
    x = x,
    wavelength = wavelength
  ) |>
    HSItools::wavelength_sub(
      x = x,
      wavelength_tbl = _
    )

  # Stretch SpatRaster
  x <- terra::stretch(x)

  if (is.null(calibration)) {
    # Plot SpatRaster
    plot <- ggplot2::ggplot() +
      # Add RGB raster layer
      tidyterra::geom_spatraster_rgb(
        data = x,
        r = 1,
        g = 2,
        b = 3,
        interpolate = TRUE
      ) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "RGB",
        y = "Depth (px)"
      )
  } else {
    plot <- ggplot2::ggplot() +
      # Add RGB raster layer
      tidyterra::geom_spatraster_rgb(
        data = x,
        r = 1,
        g = 2,
        b = 3,
        interpolate = TRUE
      ) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) {
          format(
            terra::round(
              -1 *
                i *
                calibration$pixel_ratio +
                calibration$distance -
                calibration$point_zero
            )
          )
        },
        breaks = scales::breaks_pretty()
      ) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "RGB",
        y = "Depth (mm)"
      )
  }

  # Return plot as an object
  return(plot)
}

#' Overlay color plot of proxy on RGB
#'
#' @family Plotting
#' @param x raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param alpha a number in (0, 1) controlling transparency.
#' @param ... additional arguments.
#'
#' @return a plot with color map of selected hyperspectral index overlain on RGB image.
#' @export
plot_raster_overlay <- function(
  x,
  index,
  alpha = 0.5,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Subset SpatRaster
  hsi_layer <- x |>
    terra::subset(index)

  x <- HSItools::wavelength_position(
    x = x,
    wavelength = c(650, 550, 450)
  ) |>
    HSItools::wavelength_sub(
      x = x,
      wavelength_tbl = _
    )

  # Stretch SpatRaster
  x <- terra::stretch(x)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add RGB raster layer
    tidyterra::geom_spatraster_rgb(
      data = x,
      r = 1,
      g = 2,
      b = 3,
      interpolate = TRUE
    ) +
    # Add raster layer
    tidyterra::geom_spatraster(
      data = hsi_layer,
      interpolate = TRUE
    ) +
    # Define fill colors
    ggplot2::scale_fill_viridis_c(
      alpha = alpha,
      guide = ggplot2::guide_colorbar(
        title = index,
        title.position = "bottom",
        ticks = FALSE
      )
    ) +
    # Fix the coordinates
    ggplot2::coord_fixed() +
    # Modify theme
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.line.y.left = ggplot2::element_line(color = "black"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "RGB",
      y = "Depth"
    )

  # Return plot as an object
  return(plot)
}


#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @family Plotting
#' @param x a SpatRaster with REFLECTANCE file. Used for correct placement.
#' @param plots a list of plots.
#' @param ... additional arguments.
#'
#' @return a plot.
#' @export
plot_composite <- function(
  x,
  plots,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Check if correct class is supplied.
  if (!inherits(plots, what = "list")) {
    rlang::abort(message = "Supplied data is not a list.")
  }

  # Create a plot composed from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect"
    )

  # Return plot as an object
  return(plot)
}

#' Line plots of calculated proxies series
#'
#' @family Plotting
#' @param x a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param index a character indicating hyperspectral index layer to plot.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param ... additional arguments.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_series <- function(
  x,
  index,
  calibration = NULL,
  ...
) {
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Subset SpatRaster
  hsi_layer <- x |>
    terra::subset(index)

  # Clean data
  data <- x |>
    HSItools::extract_spectral_series() |>
    dplyr::select(
      .data$y,
      {{ index }}
    ) |>
    dplyr::rename(
      y = .data$y,
      proxy = {{ index }}
    )

  # Proxy name
  proxy_name <- rlang::as_label(rlang::enquo(index))

  if (is.null(calibration)) {
    # Create a plot
    plot <- data |>
      # Pass to plot
      ggplot2::ggplot() +
      # Add aes
      ggplot2::aes(
        x = .data$proxy,
        y = .data$y
      ) +
      # Add geom
      ggplot2::geom_path() +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black"),
        panel.border = ggplot2::element_rect(color = "black", fill = NA),
        legend.text.position = "bottom"
      ) +
      # Add labels
      ggplot2::labs(
        x = proxy_name,
        y = "Depth (px)"
      )
  } else {
    # Create a plot
    plot <- data |>
      # Pass to plot
      ggplot2::ggplot() +
      # Add aes
      ggplot2::aes(
        x = .data$proxy,
        y = .data$y
      ) +
      # Add geom
      ggplot2::geom_path() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) {
          format(
            terra::round(
              -1 *
                i *
                calibration$pixel_ratio +
                calibration$distance -
                calibration$point_zero
            )
          )
        },
        breaks = scales::breaks_pretty()
      ) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black"),
        panel.border = ggplot2::element_rect(color = "black", fill = NA),
        legend.text.position = "bottom"
      ) +
      # Add labels
      ggplot2::labs(
        x = proxy_name,
        y = "Depth (mm)"
      )
  }

  # Return plot as an object
  return(plot)
}

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
  # Check if correct class is supplied.
  if (!inherits(x, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

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
