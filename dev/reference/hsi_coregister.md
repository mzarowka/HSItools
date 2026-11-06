# Co-register source raster to target raster grid

Warp a source raster onto the target grid using matched control points.
Areas without source coverage are `NA`.

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
  to warp.

- y:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  defining the output grid. Output extent, resolution, and dimensions
  are taken from this raster.

- gcp:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
  matched control points from
  [`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md).
  Each row identifies the same point in both images through `source_x`,
  `source_y`, `target_x`, and `target_y`.

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

A control point (GCP) is the same recognizable feature marked in both
images.

The function fits a first-order affine transformation from the points,
then uses
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
to warp directly onto the grid defined by `y`. Band names from `x` are
preserved.

## See also

[`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md)
for matching control points,
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
