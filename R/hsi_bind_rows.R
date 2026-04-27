#' Bind hyperspectral SpatRasters along the y-axis
#'
#' @family Utilities
#'
#' @param x List of [`SpatRaster`][terra::SpatRaster-class] objects with
#'   hyperspectral data, in the desired top-to-bottom order. All elements must
#'   share the same number of layers, layer names, and spatial resolution.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::merge()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with all inputs bound
#'   along the y-axis in the order provided.
#'
#' @details
#' Concatenates SpatRasters along the y-axis in the order provided. All
#' rasters are left-aligned to `xmin = 0`. Rasters narrower than the widest
#' input are padded with `NA` columns on the right via [`terra::extend()`],
#' so the output forms a rectangular block rather than a Tetris-like shape.
#'
#' Resolution, layer count, and layer names (wavelengths) must be identical
#' across all inputs — the function aborts if any mismatch is detected.
#'
#' @seealso
#' [`hsi_bind_layers()`] for concatenation along the spectral (layer) axis.
#'
#' @examples
#' \dontrun{
#' sections <- list(section_01, section_02, section_03, section_04)
#' x_bound <- hsi_bind_rows(sections, filename = "bound.tif", overwrite = TRUE)
#' }
#'
#' @export
hsi_bind_rows <- function(
  x,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls list} of {.cls SpatRaster} objects, not {.cls {class(x)[[1]]}}."
    )
  }

  if (length(x) < 2) {
    cli::cli_abort(
      "{.arg x} must contain at least 2 {.cls SpatRaster} objects, not {length(x)}."
    )
  }

  purrr::iwalk(x, \(r, i) {
    if (!inherits(r, "SpatRaster")) {
      cli::cli_abort(
        "Element {i} of {.arg x} must be a {.cls SpatRaster}, not {.cls {class(r)[[1]]}}."
      )
    }
  })

  # Validate consistent layer count
  n_layers <- purrr::map_int(x, terra::nlyr)

  if (length(unique(n_layers)) > 1) {
    cli::cli_abort(
      c(
        "All elements of {.arg x} must have the same number of layers.",
        "i" = "Found: {.val {unique(n_layers)}}."
      )
    )
  }

  # Validate consistent layer names (wavelengths)
  ref_names <- names(x[[1]])
  names_match <- purrr::map_lgl(x[-1], \(r) identical(names(r), ref_names))

  if (!all(names_match)) {
    mismatched <- which(!names_match) + 1L
    cli::cli_abort(
      c(
        "All elements of {.arg x} must have identical layer names.",
        "i" = "Mismatch in element{?s}: {.val {mismatched}}."
      )
    )
  }

  # Validate consistent resolution
  ref_res <- terra::res(x[[1]])
  res_match <- purrr::map_lgl(x[-1], \(r) identical(terra::res(r), ref_res))

  if (!all(res_match)) {
    mismatched <- which(!res_match) + 1L
    cli::cli_abort(
      c(
        "All elements of {.arg x} must have the same spatial resolution.",
        "i" = "Mismatch in element{?s}: {.val {mismatched}}."
      )
    )
  }

  # Compute geometry for each element
  geom <- tibble::tibble(
    idx = seq_along(x),
    nrow = purrr::map_int(x, terra::nrow),
    ncol = purrr::map_int(x, terra::ncol)
  )

  # Compute target width and cumulative y-offsets
  res_x <- ref_res[[1]]
  res_y <- ref_res[[2]]
  max_ncol <- max(geom$ncol)
  target_xmax <- max_ncol * res_x

  # Cumulative y-offsets: first element starts at ymax = 0, subsequent
  # elements stack below (negative y direction)
  row_heights <- geom$nrow * res_y
  y_offsets <- c(0, cumsum(row_heights[-length(row_heights)]))

  # Align each raster: left-align to xmin = 0, stack vertically, extend to common width
  aligned <- purrr::pmap(
    list(r = x, ncol_i = geom$ncol, y_off = y_offsets),
    \(r, ncol_i, y_off) {
      # Set left-aligned extent
      core_nrow <- terra::nrow(r)
      core_xmax <- ncol_i * res_x
      core_ymax <- -y_off
      core_ymin <- core_ymax - (core_nrow * res_y)

      terra::ext(r) <- terra::ext(0, core_xmax, core_ymin, core_ymax)

      # Extend narrower rasters to target width with NA padding on the right
      if (ncol_i < max_ncol) {
        target_ext <- terra::ext(0, target_xmax, core_ymin, core_ymax)
        r <- terra::extend(r, target_ext)
      }

      r
    }
  )

  # Bind rasters along y-axis
  collection <- terra::sprc(aligned)

  if (filename != "") {
    result <- terra::merge(
      collection,
      filename = filename,
      overwrite = overwrite,
      ...
    )
  } else {
    result <- terra::merge(collection, ...)
  }

  # Return result
  result
}
