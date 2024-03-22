#' Stretch and optionally save full RGB preview of SpatRaster
#'
#' @family Plotting
#' @param raster a SpatRaster, preferably reflectance file.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting SpatRaster be written to file.
#'
#' @export
stretch_raster_full <- function(
    raster,
    ext = NULL,
    write = TRUE) {

  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Check if SpatRaster has bands close to RGB. Important for longer wavelengths.
  # if ()
  #  rlang::warn(message = "Supplied SpatRaster has no wavelengths close to RGB.")

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  # Prepare name
  filename <- paste0(raster_src, "/RGB_", raster_name, ".", ext)

  # Check if there are values close to RGB, within the 25 nm.
  if (all(purrr::list_c(purrr::map(c(450, 550, 650), \(i) dplyr::near(i, as.numeric(names(raster)), tol = 25)))) == TRUE) {
    spectra <- c(450, 550, 650)
  } else {
    rlang::warn("No layers matching the RGB. Using the first, middle and last available layers.")
    spectra <- c(
      min(1:terra::nlyr(raster)),
      terra::median(1:terra::nlyr(raster)),
      max(terra::nlyr(raster))) |>
      (\(i) as.numeric(names(1:terra::subset(raster, i))))()
  }

  # Check if raster is written to file
  if (write == FALSE) {
    # Subset and write to RGB
    raster <- HSItools::spectra_position(
      raster,
      spectra = spectra
    ) |>
      HSItools::spectra_sub(
        raster = raster,
        spectra_tbl = _
      ) |>
      terra::stretch()
  } else {

    # Subset and write to RGB
    raster <- HSItools::spectra_position(
      raster,
      spectra = spectra
    ) |>
      HSItools::spectra_sub(
        raster = raster,
        spectra_tbl = _
      ) |>
      terra::stretch(
        filename = filename,
        overwrite = TRUE
      )
  }

  # Return SpatRaster
  return(raster)
}

#' Plot spatial map plots of calculated proxies, and optionally save to file
#'
#' @family Plotting
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param palette a character indicating one of \pkg{viridis} palettes of choice: "viridis", "magma", "plasma", "inferno", "civids", "mako", "rocket" and "turbo”.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting SpatRaster be written to file.
#'
#' @importFrom ggplot2 theme
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_proxy <- function(
    raster,
    hsi_index,
    calibration = NULL,
    palette = c("viridis", "magma", "plasma", "inferno", "civids", "mako", "rocket", "turbo"),
    extent = NULL,
    ext = NULL,
    write = FALSE,
    ...) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(hsi_index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  filename <- paste0(raster_src, "/", hsi_index, "_", raster_name, ".", ext)

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(hsi_index)

  if (is.null(extent)) {
    # Set window of interest
    terra::window(hsi_layer) <- terra::ext(hsi_layer)
  } else {
    # Set window of interest
    terra::window(hsi_layer) <- terra::ext(extent)
  }

  if (is.null(calibration)) {

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add raster layer
    tidyterra::geom_spatraster(data = hsi_layer) +
    # Define fill colors
    ggplot2::scale_fill_viridis_c(
      option = palette,
      guide = ggplot2::guide_colorbar(
        theme = ggplot2::theme(
          title = ggplot2::element_text(hsi_index),
          legend.position = "bottom",
          legend.ticks = ggplot2::element_blank()))
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
      x = hsi_index,
      y = "Depth (px)",
      fill = "Value"
    )

  } else {

    # Plot SpatRaster
    plot <- ggplot2::ggplot() +
      # Add raster layer
      tidyterra::geom_spatraster(data = hsi_layer) +
      # Define fill colors
      ggplot2::scale_fill_viridis_c(
        option = palette,
        guide = ggplot2::guide_colorbar(
          theme = ggplot2::theme(
            title = ggplot2::element_text(hsi_index),
            legend.position = "bottom",
            legend.ticks = ggplot2::element_blank()))
      ) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) format(round(-1 * i * calibration$pixel_ratio + calibration$distance - calibration$point_zero)),
        breaks = scales::breaks_pretty()) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = hsi_index,
        y = "Depth (mm)",
        fill = "Value"
      )
  }

  # Reset window
  terra::window(raster) <- NULL

  if (write == TRUE) {
    cli::cli_alert("Writing {hsi_index} SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Spatial map plots of RGB image
#'
#' @family Plotting
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers or just RGB layers.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting SpatRaster be written to file.
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_rgb <- function(
    raster,
    calibration = NULL,
    extent = NULL,
    ext = NULL,
    write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  # Prepare filename
  filename <- paste0(raster_src, "/RGB_GG_", raster_name, ".", ext)

  # Check if there are values close to RGB, within the 25 nm.
  if (all(purrr::list_c(purrr::map(c(450, 550, 650), \(i) dplyr::near(i, as.numeric(names(raster)), tol = 25)))) == TRUE) {
    spectra <- c(450, 550, 650)
  } else {
    rlang::warn("No layers matching the RGB. Using the first, middle and last available layers.")
    spectra <- c(
      min(1:terra::nlyr(raster)),
      terra::median(1:terra::nlyr(raster)),
      max(1:terra::nlyr(raster))) |>
      (\(i) as.numeric(names(terra::subset(raster, i))))()
  }

  # Prepare SpatRaster
  raster <- HSItools::spectra_position(
    raster,
    spectra = spectra) |>
    HSItools::spectra_sub(
      raster = raster,
      spectra_tbl = _)

  # Stretch SpatRaster
  raster <- terra::stretch(raster)

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  if (is.null(calibration)) {
    # Plot SpatRaster
    plot <- ggplot2::ggplot() +
      # Add RGB raster layer
      tidyterra::geom_spatraster_rgb(
        data = raster,
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
        data = raster,
        r = 1,
        g = 2,
        b = 3,
        interpolate = TRUE
      ) +
      # Fix the coordinates
      ggplot2::coord_fixed() +
      # Modify Y scale
      ggplot2::scale_y_continuous(
        labels = \(i) format(round(-1 * i * calibration$pixel_ratio + calibration$distance - calibration$point_zero)),
        breaks = scales::breaks_pretty()) +
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

  # Reset window
  terra::window(raster) <- NULL

  if (write == TRUE) {
    cli::cli_alert("Writing RGB SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Overlay color plot of proxy on RGB
#'
#' @family Plotting
#' @param raster raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param palette a character indicating one of \pkg{viridis} palettes of choice: "viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket” and “turbo”.
#' @param alpha a number in (0, 1) controlling transparency.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting SpatRaster be written to file.
#'
#' @return a plot with color map of selected hyperspectral index overlain on RGB image.
#' @export
plot_raster_overlay <- function(
    raster,
    hsi_index,
    palette = c("viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket”, “turbo"),
    alpha = 0.5,
    extent = NULL,
    ext = NULL,
    write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(hsi_index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  filename <- paste0(raster_src, "/OVERLAY_", hsi_index, "_", raster_name, ".", ext)

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(hsi_index)

  raster <- HSItools::spectra_position(
    raster,
    spectra = c(650, 550, 450)) |>
    HSItools::spectra_sub(
      raster = raster,
      spectra_tbl = _)

  # Stretch SpatRaster
  raster <- terra::stretch(raster)

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add RGB raster layer
    tidyterra::geom_spatraster_rgb(
      data = raster,
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
      option = palette,
      guide = ggplot2::guide_colorbar(
        title = hsi_index,
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

  # Reset window
  terra::window(raster) <- NULL

  if (write == TRUE) {
    cli::cli_alert("Writing {hsi_index} overlay on RGB SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @family Plotting
#' @param raster a SpatRaster with REFLECTANCE file. Used for correct placement.
#' @param plots a list of plots.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting \pkg{ggplot2} object be written to file.
#'
#' @return a plot.
#' @export
plot_composite <- function(raster, plots, ext = NULL, write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Check if correct class is supplied.
  if (!inherits(plots, what = "list")) {
    rlang::abort(message = "Supplied data is not a list.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  filename <- paste0(raster_src, "/COMPOSITE_", raster_name, ".", ext)

  # Create a plot composed from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect"
    )

  if (write == TRUE) {
    cli::cli_alert("Writing stacked plots to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Line plots of calculated proxies series
#'
#' @family Plotting
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param calibration result of pixel_to_distance or actual call to pixel_to_distance with appropriate input.
#' @param extent a terra extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename empty = in memory, TRUE = guess name and attempt write, or user specified path to glue with ext.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_series <- function(raster, hsi_index, calibration = NULL, extent = NULL, ext = NULL, filename = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(hsi_index, what = "character")) {
    rlang::abort(message = "Supplied index name is not a character.")
  }

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(hsi_index)

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Clean data
  data <- raster |>
    HSItools::extract_spectral_series() |>
    dplyr::select(
      .data$y,
      {{ hsi_index }}) |>
    dplyr::rename(
      y = .data$y,
      proxy = {{ hsi_index }})

  # Proxy name
  proxy_name <- rlang::as_label(rlang::enquo(hsi_index))

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
      axis.line.y.left = ggplot2::element_line(color = "black"),
      axis.line.x.bottom = ggplot2::element_line(color = "black")
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
        labels = \(i) format(round(-1 * i * calibration$pixel_ratio + calibration$distance - calibration$point_zero)),
        breaks = scales::breaks_pretty()) +
      # Modify theme
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.line.y.left = ggplot2::element_line(color = "black"),
        axis.line.x.bottom = ggplot2::element_line(color = "black")
      ) +
      # Add labels
      ggplot2::labs(
        x = proxy_name,
        y = "Depth (mm)"
      )
  }

  terra::window(raster) <- NULL

  if (filename == TRUE) {
    # Check source
    if (terra::sources(raster) == "") {
      rlang::abort(message = "In memory object, can't guess the name. Please provide filename.")
    }

    # Raster source directory
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()

    filename <- paste0(raster_src, "/", hsi_index, "_line_", raster_name, ".", ext)

    cli::cli_alert("Writing {hsi_index} plot to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )

  } else if (is.character(filename) & !is.null(filename)) {
    filename <- paste0(filename, ".", ext)

    cli::cli_alert("Writing {hsi_index} plot to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )

    } else {
    plot
    }

  # Return plot as an object
  return(plot)
}

#' Line plot of spectral profile from the ROI
#'
#' @family Plotting
#' @param raster Reflectance SpatRaster.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param write logical, should resulting SpatRaster be written to file.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_profile <- function(raster, extent = NULL, ext = NULL, write = FALSE) {

  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Raster source directory
  raster_src <- raster |>
    terra::sources() |>
    fs::path_dir()

  # Raster source name
  raster_name <- raster |>
    terra::sources() |>
    fs::path_file() |>
    fs::path_ext_remove()

  filename <- paste0(raster_src, "/SPECTRAL_PROFILE_", raster_name, ".", ext)

  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Clean data
  data <- raster |>
    HSItools::extract_spectral_profile() |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "Wavelength.nm",
      names_transform = as.numeric,
      values_to = "Reflectance")

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

  terra::window(raster) <- NULL

  if (write == TRUE) {
    cli::cli_alert("Writing spectral profile plot to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = ext
    )
  }

  # Return plot as an object
  return(plot)
}
