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
#'   to split `x` into. A single integer creates row strips (e.g. `60`). A
#'   length-2 vector creates a 2D tile grid (e.g. `c(8, 8)`). For best
#'   performance, match to the number of available `mirai` daemons.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] merged from processed tiles.
#'
#' @details
#' Parallelism is provided by [`purrr::in_parallel()`] and [`mirai::daemons()`].
#' Daemons must be initialised by the caller before invoking this function via
#' `mirai::daemons(n)`. If no daemons are active, processing falls back to
#' sequential automatically. Intermediate tiles are written to a temporary
#' directory and cleaned up on exit, even if the function errors.
#'
#' @examples
#' \dontrun{
#' mirai::daemons(30)
#'
#' # Good: literal values baked into the lambda
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
  overwrite = FALSE
) {
  # Validate inputs
  check_spatraster(x)

  # This validator should accept either 1 integer for rows only, or c(x, y) for nrows, ncols or 2D
  if (!length(n_tiles) %in% c(1, 2)) {
    cli::cli_abort(
      "{.arg n_tiles} must be a single integer or a vector of length 2 {.code c(nrow, ncol)}."
    )
  }

  # Create a self-cleanin tempdir
  tmp_dir <- withr::local_tempdir()

  # Create tiles
  tile_paths <- terra::makeTiles(
    x,
    y = n_tiles,
    filename = file.path(tmp_dir, "tile_.tif")
  )

  raster <- tile_paths |>
    purrr::map(
      purrr::in_parallel(
        \(path) {
          # Create path for result tile
          out_path <- file.path(
            dirname(path),
            paste0("result_", basename(path))
          )
          # Apply function, write result separately
          fun(terra::rast(path)) |>
            terra::writeRaster(filename = out_path, overwrite = TRUE)
          out_path
        },
        # Self contained, carry fun into worker namespace
        fun = fun
      )
    ) |>
    # Read all tiles into a list
    purrr::map(\(path) terra::rast(path)) |>
    # Create a SpatRaster Collection
    terra::sprc() |>
    # Merge tiles
    terra::merge(filename = filename, overwrite = overwrite)

  # Return
  raster
}
