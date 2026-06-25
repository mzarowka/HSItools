# Process a SpatRaster in parallel tiles

Process a SpatRaster in parallel tiles

## Usage

``` r
hsi_tiled(fun, x, n_tiles, filename = "", overwrite = FALSE, ...)
```

## Arguments

- fun:

  Function. Applied to each tile. Must be written as an anonymous
  function with explicit `HSItools::` namespacing, e.g.
  `\(tile) HSItools::hsi_smooth_savgol(tile, p = 3, n = 17)`. Only
  suitable for per-pixel operations with no spatial neighbourhood
  dependency. All parameters must be supplied as literal values —
  variables from the calling environment are not visible to parallel
  workers.

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- n_tiles:

  Integer or integer vector of length 1 or 2. Number of tiles to split
  `x` into. A single integer creates row strips (e.g. `30`). A length-2
  vector creates a 2D tile grid (e.g. `c(8, 8)`). For best performance,
  match to the number of available `mirai` daemons.

- filename:

  Character. Output filename. Default `""` writes the result to a
  session-scoped temporary file and emits a warning. Providing a path is
  strongly recommended. Unlike other `HSItools` functions,
  `filename = ""` never keeps the result in memory — see Details.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
merged from processed tiles.

## Details

Parallelism is provided by
[`mirai::mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html)
and
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
Daemons must be initialised by the caller before invoking this function
via `mirai::daemons(n)`. If no daemons are active, the function errors.

**This function does not support in-memory processing.**
[`terra::makeTiles()`](https://rspatial.github.io/terra/reference/makeTiles.html)
requires a filename and errors if one is not provided — tiles are always
written to disk. As a consequence, the `in_memory` parameter present in
other `HSItools` functions is intentionally absent here. The merged
result is always file-backed: either the path supplied via `filename`,
or a session-scoped temporary file when `filename = ""`. In the latter
case a warning is emitted and the temporary file persists until the R
session ends.

Intermediate tiles are written to a session-scoped temporary directory
that is not cleaned up on function exit. This is intentional: persistent
`mirai` daemon processes may hold open GDAL file handles to tile files
after the function returns, and premature cleanup causes access
violations on Windows.

## See also

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ndi.md),
[`hsi_calc_raba()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_raba.md),
[`hsi_calc_rabd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rabd.md),
[`hsi_calc_ratio()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ratio.md),
[`hsi_calc_rcv()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rcv.md),
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md),
[`hsi_calc_remp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_remp.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmedian.md),
[`hsi_calc_rsd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rsd.md),
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md),
[`hsi_destripe()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_destripe.md),
[`hsi_remove_continuum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_remove_continuum.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mirai::daemons(30)

# Recommended: always provide a filename
hsi_tiled(
  fun = \(tile) HSItools::hsi_smooth_savgol(tile, p = 3, n = 17),
  x = my_raster,
  n_tiles = 30,
  filename = "output.tif",
  overwrite = TRUE
)

# Bad: variables from calling environment are not visible to workers
p <- 3
n <- 17
hsi_tiled(
  fun = \(tile) HSItools::hsi_smooth_savgol(tile, p = p, n = n),
  x = my_raster,
  n_tiles = 30
)

mirai::daemons(0)
} # }
```
