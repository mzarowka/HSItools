# Match control points between two SpatVectors

Match control points from source and target vectors by a shared
identifier. Each matched pair represents the same recognizable landmark
in both images. CRS is stripped from both inputs to work in
pixel-coordinate space. Points present in only one input are dropped
with a warning.

## Usage

``` r
hsi_match_gcp(source, target, id_col = "gcp_id")
```

## Arguments

- source:

  A
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  containing control point locations in the source image. Must contain
  an identifier column.

- target:

  A
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  containing the corresponding control point locations in the target
  image. Must contain an identifier column.

- id_col:

  Character. Column identifying the same landmark in `source` and
  `target`. Default `"gcp_id"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- gcp_id:

  Control point identifier, or column named by `id_col`.

- source_x:

  X pixel coordinate in source raster.

- source_y:

  Y pixel coordinate in source raster.

- target_x:

  X pixel coordinate in target raster.

- target_y:

  Y pixel coordinate in target raster.

## Details

A control point (GCP) is the same recognizable feature marked in both
images.

## See also

[`hsi_check_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_check_gcp.md)
for assessing transformation quality,
[`hsi_coregister()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_coregister.md)
for warping.

Other HSI Co-registration:
[`hsi_check_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_check_gcp.md),
[`hsi_coregister()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_coregister.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get SWIR control points
swir_gcps <- terra::vect("swir_preview.gpkg", layer = "gcp")

# Get VNIR control points
vnir_gcps <- terra::vect("vnir_preview.gpkg", layer = "gcp")

# Match corresponding control points
matched <- hsi_match_gcp(swir_gcps, vnir_gcps)
} # }
```
