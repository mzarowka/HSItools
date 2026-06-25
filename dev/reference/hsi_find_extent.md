# Find a fixed-width extent from reference points

Find a fixed-width extent from reference points

Find a fixed-width extent

## Usage

``` r
hsi_find_extent(e, width)

hsi_find_extent(e, width)
```

## Arguments

- e:

  A
  [`SpatExtent`](https://rspatial.github.io/terra/reference/SpatExtent-class.html)
  in which to search. Created with
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html).

- width:

  Numeric. Width in pixels of the new extent.

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- points:

  A
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with exactly 2 point geometries marking the vertical extent of the
  region of interest. Must be in the same coordinate space as `x`.

- filename:

  Character. Output filename. Default `""` skips writing.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

- insert:

  Logical. Insert layer into an existing file. Default `FALSE`.

- layer:

  Character. Layer name for vector output. Default `""`.

- ...:

  Additional arguments passed to
  [`terra::writeVector()`](https://rspatial.github.io/terra/reference/writeVector.html).

## Value

A
[`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
polygon snapped to the grid of `x`.

A
[`SpatExtent`](https://rspatial.github.io/terra/reference/SpatExtent-class.html)
centered within `extent` and of the specified width.

## Details

The output polygon spans vertically between the two reference points and
horizontally by `width` pixels centered on the mean x-coordinate of the
points. The extent is snapped to cell boundaries of `x` with
[`terra::align()`](https://rspatial.github.io/terra/reference/align.html)
using `snap = "near"`.

The function aborts if the requested extent falls outside the raster
bounds. Reduce `width` or adjust the reference points to fit within `x`.

## See also

[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_set_extent.md)
to assign physical units to a raster extent.

Other Utilities:
[`hsi_bind_layers()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_layers.md),
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_merge_rasters()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_merge_rasters.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

ends <- terra::vect("spatials.gpkg", layer = "ends")

x_extent <- hsi_find_extent(x, points = ends, width = 100)
x_cropped <- terra::crop(x, x_extent)

x_extent <- hsi_find_extent(
  x,
  points = ends,
  width = 900,
  filename = "spatials.gpkg",
  insert = TRUE,
  layer = "extent"
)
} # }
```
