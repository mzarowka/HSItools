#' Stretch and optionally save full RGB preview of SpatRaster
#'
#' @param raster a SpatRaster, preferably reflectance file.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @export
stretch_raster_full <- function(raster, .ext = NULL, .write = TRUE) {
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

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/RGB_", raster_name, ".", .ext)

  # Check if raster is written to file
  if (.write == FALSE) {
    # Subset and write to RGB
    raster <- HSItools::spectra_position(
      raster,
      spectra = c(650, 550, 450)
    ) |>
      HSItools::spectra_sub(
        raster = raster,
        spectra_tbl = _
      ) |>
      terra::stretch()
  } else {
    cli::cli_alert("Writing RGB SpatRaster to {filename}")

    # Subset and write to RGB
    raster <- HSItools::spectra_position(
      raster,
      spectra = c(650, 550, 450)
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
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param .hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of \pkg{viridis} palettes of choice: "viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket” and “turbo”.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @importFrom ggplot2 theme
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_proxy <- function(raster, .hsi_index, .palette = c("viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket”, “turbo"), .extent = NULL, .ext = NULL, .write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(.hsi_index, what = "character")) {
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

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/", .hsi_index, "_", raster_name, ".", .ext)

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(.hsi_index)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add raster layer
    tidyterra::geom_spatraster(data = hsi_layer) +
    # Define fill colors
    ggplot2::scale_fill_viridis_c(
      option = .palette,
      guide = ggplot2::guide_colorbar(
        title = .hsi_index,
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
      x = .hsi_index,
      y = "Depth"
    )

  if (.write == TRUE) {
    cli::cli_alert("Writing {(.hsi_index)} SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Reset window
  terra::window(raster) <- NULL

  # Return plot as an object
  return(plot)
}

#' Spatial map plots of RGB image
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers or just RGB layers.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_rgb <- function(raster, .extent = NULL, .ext = NULL, .write = FALSE) {
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

  if (is.null(.extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(.extent)
  }

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/RGB_GG_", raster_name, ".", .ext)

  raster <- HSItools::spectra_position(raster, spectra = c(650, 550, 450)) |>
    HSItools::spectra_sub(raster = raster, spectra_tbl = _)

  # Stretch SpatRaster
  raster <- terra::stretch(raster)

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
      y = "Depth"
    )

  if (.write == TRUE) {
    cli::cli_alert("Writing RGB SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Reset window
  terra::window(raster) <- NULL

  # Return plot as an object
  return(plot)
}

#' Overlay color plot of proxy on RGB
#'
#' @param raster raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param .hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of \pkg{viridis} palettes of choice: "viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket” and “turbo”.
#' @param .alpha a number in [0, 1] controlling transparency.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @return a plot with color map of selected hyperspectral index overlain on RGB image.
#' @export
plot_raster_overlay <- function(raster, .hsi_index, .palette = c("viridis”, “magma”, “plasma”, “inferno”, “civids”, “mako”, “rocket”, “turbo"), .alpha = 0.5, .ext = NULL, .write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(.hsi_index, what = "character")) {
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

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/OVERLAY_", .hsi_index, "_", raster_name, ".", .ext)

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(.hsi_index)

  raster <- HSItools::spectra_position(raster, spectra = c(650, 550, 450)) |>
    HSItools::spectra_sub(raster = raster, spectra_tbl = _)

  # Stretch SpatRaster
  raster <- terra::stretch(raster)

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
      alpha = .alpha,
      option = .palette,
      guide = ggplot2::guide_colorbar(
        title = .hsi_index,
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

  if (.write == TRUE) {
    cli::cli_alert("Writing {(.hsi_index)} overlay on RGB SpatRaster to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @param raster a SpatRaster with REFLECTANCE file. Used for correct placement.
#' @param plots a list of plots.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting \pkg{ggplot2} object be written to file.
#'
#' @return a plot.
#' @export
plot_composite <- function(raster, plots, .ext = NULL, .write = FALSE) {
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

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/COMPOSITE_", raster_name, ".", .ext)

  # Create a plot composed from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect"
    )

  if (.write == TRUE) {
    cli::cli_alert("Writing stacked plots to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Line plots of calculated proxies series
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param .hsi_index a character indicating hyperspectral index layer to plot.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_series <- function(raster, .hsi_index, .extent = NULL, .ext = NULL, .write = FALSE) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (!inherits(.hsi_index, what = "character")) {
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

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/", .hsi_index, "_line_", raster_name, ".", .ext)

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(.hsi_index)

  # Clean data
  data <- raster |>
    HSItools::extract_spectral_series(.extent = .extent) |>
    dplyr::select(.data$y, {{ .hsi_index }}) |>
    dplyr::rename(y = .data$y, proxy = {{ .hsi_index }})

  # Proxy name
  proxy_name <- rlang::as_label(rlang::enquo(.hsi_index))

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
      y = "Depth"
    )

  if (.write == TRUE) {
    cli::cli_alert("Writing {(.hsi_index)} plot to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Return plot as an object
  return(plot)
}

#' Line plot of spectral profile from the ROI
#'
#' @param raster Reflectance SpatRaster.
#' @param .extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param .ext character, a graphic format extension.
#' @param .write logical, should resulting SpatRaster be written to file.
#'
#' @importFrom rlang .data
#'
#' @return line plot with of selected hyperspectral index.
#' @export
plot_profile_spectral_profile <- function(raster, .extent = NULL, .ext = NULL, .write = FALSE) {
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

  cli::cli_h1("{raster_name}")

  filename <- paste0(raster_src, "/SPECTRAL_PROFILE_", raster_name, ".", .ext)

  # Clean data
  data <- raster |>
    HSItools::extract_spectral_profile(.extent = .extent) |>
    dplyr::select(-c(.data$x, .data$y)) |>
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

  if (.write == TRUE) {
    cli::cli_alert("Writing spectral profile plot to {filename}")

    ggplot2::ggsave(
      plot = plot,
      filename = filename,
      device = .ext
    )
  }

  # Return plot as an object
  return(plot)
}
