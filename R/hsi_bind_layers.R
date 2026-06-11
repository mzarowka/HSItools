#' Bind hyperspectral SpatRasters along the spectral axis
#'
#' @family Utilities
#'
#' @param x List of [`SpatRaster`][terra::SpatRaster-class] objects with
#'   hyperspectral data. All elements must
#'   share the same spatial resolution and spatial extent (pixel to pixel coverage).
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::merge()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with all inputs bound
#'   along the spectral axis in the order provided.

hsi_bind_layers <- function(
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

  # Here goes logic
  # Get Raster 1 and 2
  # Apply function (toDo) where reflectance is corrected in respect to reference raster
  # Concatenate
  # Write if needed

  # Return result
  result
}
