#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param continuum_edges Numeric vector of length 2. Continuum anchor wavelengths in nm.
#' @param absorption_band Numeric vector. Wavelength(s) in nm to search for
#'   trough (absorption feature). Single value for `"strict"` type.
#' @param index_type Character. Type of RABD index. One of:
#'   \describe{
#'     \item{`"max"`}{Flexibly find the lowest reflectance within the trough range.}
#'     \item{`"strict"`}{Use a specific wavelength as the trough.}
#'     \item{`"mid"`}{Use the midpoint between the min and max trough wavelength.}
#'   }
#' @param index_name Character. Name for the output layer. Default `NULL`.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with RABD values.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' x_rabd <- hsi_calc_rabd(
#'   x,
#'   continuum_edges = c(590, 730),
#'   absorption_band = 660:680,
#'   index_type = "max"
#' )
#'
#' x_rabd <- hsi_calc_rabd(
#'   x,
#'   continuum_edges = c(590, 730),
#'   absorption_band = 673,
#'   index_type = "strict",
#'   index_name = "rabd673",
#'   filename = "output_rabd.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_calc_rabd <- function(
  x,
  continuum_edges,
  absorption_band,
  index_type,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate input
  check_spatraster(x)

  # Validate bands
  check_numeric(continuum_edges, len = 2)

  # Validate index type
  if (!index_type %in% c("strict", "mid", "max")) {
    cli::cli_abort(
      "Input {.arg index_type} must be one of 'strict', 'mid' or 'max'.",
      class = "hsitools_error"
    )
  }

  # Validate bands for strict
  if (index_type %in% c("strict") && length(absorption_band) > 1) {
    check_numeric(absorption_band, len = 1)
  }

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)

  # Named list with write options
  wopt_default <- list(
    names = index_name
  )

  # Splice wopt defaults with user input if any
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Get the trough shoulders ----
  ## Left shoulder ----
  # Get the position tibble
  l_edge_tibble <- wavelength_position(
    x = x,
    wavelength = continuum_edges[1]
  )

  # Subset SpatRaster by position for reflectance value
  l_edge_subset <- terra::subset(
    x = x,
    l_edge_tibble$position
  )

  # Get the SpatRaster with wavelength
  l_edge_wavelength <- l_edge_subset |>
    terra::init(l_edge_tibble$band_wavelength)

  ## Right shoulder ----
  # Get the position tibble
  r_edge_tibble <- wavelength_position(
    x = x,
    wavelength = continuum_edges[2]
  )

  # Subset SpatRaster by position for reflectance value
  r_edge_subset <- terra::subset(
    x = x,
    r_edge_tibble$position
  )

  # Get the SpatRaster with wavelength
  r_edge_wavelength <- r_edge_subset |>
    terra::init(r_edge_tibble$band_wavelength)

  # Get the trough ----
  ## "max" variant ----
  if (index_type == "max") {
    # Get the position tibble
    trough_tibble <- wavelength_position(
      x = x,
      wavelength = absorption_band
    ) |>
      # Add the row id for later substitution
      dplyr::mutate(id = dplyr::row_number())

    # Subset SpatRaster by position
    trough_subset <- terra::subset(
      x = x,
      trough_tibble$position
    )

    # Find the position of minimum reflectance
    trough_position <- terra::which.min(x = trough_subset)

    # Get the min reflectance in each cell
    trough_reflectance <- terra::selectRange(trough_subset, trough_position)

    # Get the SpatRaster with wavelength
    trough_wavelength <- trough_position |>
      terra::subst(trough_tibble$id, trough_tibble$band_wavelength)

    ## "strict" variant ----
  } else if (index_type == "strict") {
    # Get the position tibble
    trough_tibble <- wavelength_position(
      x = x,
      wavelength = absorption_band
    )

    # Subset SpatRaster by position
    trough_reflectance <- terra::subset(
      x = x,
      trough_tibble$position
    )

    # Get the SpatRaster with wavelength
    trough_wavelength <- trough_reflectance |>
      terra::init(trough_tibble$band_wavelength)

    ## "mid" variant ----
  } else if (index_type == "mid") {
    absorption_band <- stats::median(absorption_band)

    # Get the position tibble
    trough_tibble <- wavelength_position(
      x = x,
      wavelength = absorption_band
    )

    # Subset SpatRaster by position
    trough_reflectance <- terra::subset(
      x = x,
      trough_tibble$position
    )

    # Get the SpatRaster with wavelength
    trough_wavelength <- trough_reflectance |>
      terra::init(trough_tibble$band_wavelength)
  }

  # RABD calculation ----
  ## Get the distances (width) ----
  # Left
  l_edge_width <- trough_wavelength - l_edge_wavelength

  # Right
  r_edge_width <- r_edge_wavelength - trough_wavelength

  ## Equation numerator ----
  numerator <- (r_edge_width *
    l_edge_subset +
    l_edge_width * r_edge_subset) /
    (r_edge_width + l_edge_width)

  ## Calculate RABD ----
  result <- numerator / trough_reflectance

  # Set name
  if (!is.null(index_name)) {
    names(result) <- index_name
  } else {
    names(result) <- NULL
  }

  # Write new raster to file based on user input
  if (filename != "") {
    terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return raster
  result
}
