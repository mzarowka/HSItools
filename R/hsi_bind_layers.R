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
  check_spatraster_list(x)

  if (length(x) < 2) {
    cli::cli_abort(
      "{.arg x} must contain at least 2 {.cls SpatRaster} objects, not {length(x)}.",
      class = "hsitools_error"
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
      ),
      class = "hsitools_error"
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
