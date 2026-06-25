# Co-register source raster to target raster grid

Warp a source raster onto the target grid using matched ground control
points via GDAL. Areas without source coverage are `NA`.

## Usage

``` r
hsi_coregister(
  x,
  y,
  gcp,
  method = "bilinear",
  filename = "",
  overwrite = FALSE
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  to warp. Must have a file source on disk.

- y:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  defining the output grid. Output extent, resolution, and dimensions
  are taken from this raster.

- gcp:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
  matched GCPs from
  [`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md).
  Must contain columns `source_x`, `source_y`, `target_x`, `target_y`.

- method:

  Character. Resampling method. One of `"near"`, `"bilinear"`,
  `"cubic"`, `"cubicspline"`, `"lanczos"`. Default `"bilinear"`.

- filename:

  Character. Output filename. Default `""` keeps result in memory.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
aligned to the `y` grid.

## Details

Uses
[`sf::gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html)
internally. The source GCPs are embedded in a lightweight VRT (no data
duplication), then GDAL warps onto the target grid with a first-order
polynomial (affine) transformation. Band names from `x` are preserved in
the output file.

Requires the sf package.

## See also

[`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md)
for matching GCPs,
[`hsi_check_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_check_gcp.md)
for checking residuals first.

Other HSI Co-registration:
[`hsi_check_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_check_gcp.md),
[`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
matched <- hsi_match_gcp(swir_gcps, vnir_gcps)

x_coregistered <- hsi_coregister(
  x = swir,
  y = vnir,
  gcp = matched,
  filename = "SWIR_coregistered.tif"
)
} # }
```
