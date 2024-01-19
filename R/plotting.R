# Plotting
# Plotting RGB file
# Plotting colored map of proxy (RABD, Ratio etc)
# Plotting profile
# Put core and profile/map pairs along - let user choose

#' Save full SpatRaster of RGB preview
#'
#' @param raster a SpatRaster.
#' @param .ext character, a graphic format extension, one of "jpg", "png", "tif".
#' @param ... extra parameters, such as path.
#'
#' @export
plot_raster_full <- function(raster, ext, product = TRUE, ...) {
  # Store additional parameters
  params <- list(...)

  # Choose write mode
  if (product == TRUE) {
    filename <- paste0(params$path, "/products/RGB_", basename(params$path), ".", ext)
  } else {
    filename <- paste0(basename(params$path), ".", ext)
  }

  # Subset and write to RGB
  raster <- HSItools::spectra_position(raster, spectra = c(650, 550, 450)) |>
    HSItools::spectra_sub(raster = raster, spectra_tbl = _) |>
    terra::stretch(filename = filename,
                   overwrite = TRUE)

  # Return plot
  return(raster)
}

#' Spatial map plots of calculated proxies
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of color palettes of choice. One of: "red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta".
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_proxy <- function(raster, hsi_index, .palette) {
  # Choose palette
  proxy_palette <- list(
    red = c(high = "#AF3029", mid = "#D14D41", low = "#F2F0E5"),
    orange = c(high = "#BC5215", mid = "#DA702C", low = "#F2F0E5"),
    yellow = c(high = "#AD8301", mid = "#D0A215", low = "#F2F0E5"),
    green = c(high = "#66800B", mid = "#879A39", low = "#F2F0E5"),
    cyan = c(high = "#24837B", mid = "#3AA99F", low = "#F2F0E5"),
    blue = c(high = "#205EA6", mid = "#4385BE", low = "#F2F0E5"),
    purple = c(high = "#5E409D", mid = "#8B7EC8", low = "#F2F0E5"),
    magenta = c(high = "#A02F6F", mid = "#CE5D97", low = "#F2F0E5"))

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(hsi_index)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add raster layer
    tidyterra::geom_spatraster(data = hsi_layer) +
    # Define fill colors
    ggplot2::scale_fill_gradientn(
      colors = c(proxy_palette[[.palette]][3], mid = proxy_palette[[.palette]][2], proxy_palette[[.palette]][1]),
      guide = guide_colorbar(
        title = hsi_index,
        title.position = "bottom",
        ticks = FALSE)) +
    # Fix the coordinates
    ggplot2::coord_fixed() +
    # Modify theme
    ggplot2::theme(
      panel.background = element_blank(),
      axis.line.y.left = element_line(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom") +
    ggplot2::labs(x = hsi_index,
                  y = "Depth")

  # Return plot as an object
  return(plot)
}

#' Spatial map plots of RGB image
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers or just RGB layers.
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_rgb <- function(raster){
  raster <- HSItools::spectra_position(raster, spectra = c(650, 550, 450)) |>
    HSItools::spectra_sub(raster = raster, spectra_tbl = _)

  raster <- terra::stretch(raster)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add RGB raster layer
    tidyterra::geom_spatraster_rgb(data = raster,
                                   r = 1,
                                   g = 2,
                                   b = 3,
                                   interpolate = TRUE) +
    # Fix the coordinates
    ggplot2::coord_fixed() +
    # Modify theme
    ggplot2::theme(
      panel.background = element_blank(),
      axis.line.y.left = element_line(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom") +
    ggplot2::labs(x = "RGB",
                  y = "Depth")

  # Return plot as an object
  return(plot)
}

# Overlay color plot of proxy on RGB
plot_raster_overlay <- function(data, hsi_index){

}

#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @param plots a list of plots.
#'
#' @return a plot.
#' @export
plot_composite <- function(plots) {
  # Create a plot composed from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect")

  # Return plot as an object
  return(plot)
}

#' Line plots of calculated proxies
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of color palettes of choice. One of: "red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta".
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_profile_proxy <- function(data, hsi_index, .palette){
  # Clean data
  data <- data |>
    dplyr::mutate(y = y,
                  proxy = {{ hsi_index }},
                  .keep = "none")

  # Proxy name
  proxy_name <- rlang::as_label(rlang::enquo(hsi_index))

  # Choose palette
  proxy_palette <- list(
    red = "#AF3029",
    orange = "#BC5215",
    yellow = "#AD8301",
    green = "#66800B",
    cyan = "#24837B",
    blue = "#205EA6",
    purple = "#5E409D",
    magenta = "#A02F6F")

  # Create a plot
  plot <- data |>
    # Pass to plot
    ggplot2::ggplot() +
    # Add aes
    ggplot2::aes(x = proxy, y = y) +
    # Add geom
    ggplot2::geom_path(color = proxy_palette[[.palette]]) +
    # Modify theme
    ggplot2::theme(
      panel.background = element_blank(),
      axis.line.y.left = element_line(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom") +
    # Add labels
    ggplot2::labs(x = proxy_name,
                  y = "Depth")

  # Return plot as an object
  return(plot)
}
