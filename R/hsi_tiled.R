#' Process a SpatRaster in parallel tiles
#'
#' @family HSI Transformations
#'
#' @param fun Function. Applied to each tile. Must be written as an anonymous
#'   function with explicit `HSItools::` namespacing, e.g.
#'   `\(tile) HSItools::hsi_smooth_savgol(tile, p = 3, n = 17)`. Only suitable
#'   for per-pixel operations with no spatial neighbourhood dependency. All
#'   parameters must be supplied as literal values — variables from the calling
#'   environment are not visible to parallel workers.
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param n_tiles Integer or integer vector of length 1 or 2. Number of tiles
#'   to split `x` into. A single integer creates row strips (e.g. `30`). A
#'   length-2 vector creates a 2D tile grid (e.g. `c(8, 8)`). For best
#'   performance, match to the number of available `mirai` daemons.
#' @param filename Character. Output filename. Default `""` writes the result
#'   to a session-scoped temporary file and emits a warning. Providing a path
#'   is strongly recommended. Unlike other `HSItools` functions, `filename = ""`
#'   never keeps the result in memory — see Details.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] merged from processed tiles.
#'
#' @details
#' Parallelism is provided by [`mirai::mirai_map()`] and [`mirai::daemons()`].
#' Daemons must be initialised by the caller before invoking this function via
#' `mirai::daemons(n)`. If no daemons are active, the function errors.
#'
#' **This function does not support in-memory processing.** [`terra::makeTiles()`]
#' requires a filename and errors if one is not provided — tiles are always
#' written to disk. As a consequence, the `in_memory` parameter present in
#' other `HSItools` functions is intentionally absent here. The merged result
#' is always file-backed: either the path supplied via `filename`, or a
#' session-scoped temporary file when `filename = ""`. In the latter case a
#' warning is emitted and the temporary file persists until the R session ends.
#'
#' Intermediate tiles are written to a session-scoped temporary directory that
#' is not cleaned up on function exit. This is intentional: persistent `mirai`
#' daemon processes may hold open GDAL file handles to tile files after the
#' function returns, and premature cleanup causes access violations on Windows.
#'
#' @examples
#' \dontrun{
#' mirai::daemons(30)
#'
#' # Recommended: always provide a filename
#' hsi_tiled(
#'   fun = \(tile) HSItools::hsi_smooth_savgol(tile, p = 3, n = 17),
#'   x = my_raster,
#'   n_tiles = 30,
#'   filename = "output.tif",
#'   overwrite = TRUE
#' )
#'
#' # Bad: variables from calling environment are not visible to workers
#' p <- 3
#' n <- 17
#' hsi_tiled(
#'   fun = \(tile) HSItools::hsi_smooth_savgol(tile, p = p, n = n),
#'   x = my_raster,
#'   n_tiles = 30
#' )
#'
#' mirai::daemons(0)
#' }
#'
#' @export
hsi_tiled <- function(
  fun,
  x,
  n_tiles,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)
  rlang::check_installed("mirai", reason = "to use `hsi_tiled()`.")

  if (!length(n_tiles) %in% c(1, 2)) {
    cli::cli_abort(
      "{.arg n_tiles} must be a single integer or a vector of length 2 {.code c(nrow, ncol)}.",
      class = "hsitools_error"
    )
  }

  rlang::check_string(filename)

  rlang::check_bool(overwrite)

  # Determine merge target path.
  # Exception to the withr rule: the merge output IS the backing store of the
  # returned SpatRaster. withr::local_tempdir() would delete it on function
  # exit, orphaning the object. Plain tempfile() is intentional here — it
  # persists for the session lifetime. See hsi_calc_reflectance for reference.
  if (filename != "") {
    merge_target <- normalizePath(filename, mustWork = FALSE)
  } else {
    merge_target <- tempfile(fileext = ".tif")
    cli::cli_warn(
      c(
        "No {.arg filename} provided.",
        "i" = "Result is backed by a temporary file that will persist until the R session ends.",
        "i" = "Provide {.arg filename} to write to a permanent location."
      ),
      class = "hsitools_warning"
    )
  }

  # Capture writeRaster options before mirai_map
  wopt_user <- rlang::list2(...)

  # Session-lifetime temp dir for tiles.
  # Exception to the withr rule: daemon processes may hold open GDAL handles
  # after this function returns. See hsi_calc_reflectance for same pattern.
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  # Convert n_tiles (desired count) to rows/cols per tile for makeTiles.
  # makeTiles interprets y as rows-per-tile, not tile count.
  if (length(n_tiles) == 1) {
    n_tiles <- c(n_tiles, 1L)
  }

  tile_size <- c(
    ceiling(terra::nrow(x) / n_tiles[1]),
    ceiling(terra::ncol(x) / n_tiles[2])
  )

  # Create tiles
  tile_paths <- terra::makeTiles(
    x,
    y = tile_size,
    filename = file.path(tmp_dir, "tile_.tif")
  )

  # Process tiles in parallel
  result_paths <- mirai::mirai_map(
    tile_paths,
    \(path, fun) {
      out_path <- file.path(
        dirname(path),
        paste0("result_", basename(path))
      )
      fun(terra::rast(path)) |>
        terra::writeRaster(filename = out_path, overwrite = TRUE)
      out_path
    },
    .args = list(fun = fun)
  )[] |>
    unlist()

  # Build write options from a result tile — fun determines output structure
  result_names <- names(terra::rast(result_paths[[1]]))
  wopt_default <- list(names = result_names)
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Build VRT mosaic and write final output
  terra::vrt(
    result_paths,
    filename = normalizePath(
      file.path(tmp_dir, "mosaic.vrt"),
      mustWork = FALSE
    ),
    overwrite = TRUE
  ) |>
    terra::writeRaster(
      filename = merge_target,
      overwrite = overwrite,
      wopt = wopt,
      gdal = "BIGTIFF=YES"
    )

  # Return result
  terra::rast(merge_target)
}
