# Create spatial calibration from a digitized scale reference

Create a spatial calibration by measuring a known-length reference
digitized in GIS software. The function calculates pixel distance from
the geometry and derives resolution from the provided physical distance.

## Usage

``` r
hsi_calibration_from_scale(x, distance = 10000, units = "um")
```

## Arguments

- x:

  A
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  representing a digitized scale reference. Must be either a line
  geometry or exactly 2 points. CRS must be `NULL` (pixel coordinates).

- distance:

  Numeric. Physical distance the reference represents. Single positive
  value. Default `10000`.

- units:

  Character. Units of the provided distance. One of `"um"`
  (micrometers), `"mm"`, or `"cm"`. Default `"um"`.

## Value

A named numeric with resolution in µm/px, named `"um_per_px"`.

## Details

The input
[`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
can be a line drawn along the reference, or two points placed at the
reference endpoints. CRS must be `NULL` because coordinates are in pixel
space.

## See also

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_dims.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/reference/hsi_drop_crs.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/reference/hsi_pixels_to_units.md),
[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/reference/hsi_set_extent.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scale_line <- terra::vect("scale.gpkg", layer = "scale")

calibration <- hsi_calibration_from_scale(
  scale_line,
  distance = 10,
  units = "mm"
)

scale_points <- terra::vect("points.gpkg", layer = "scale_points")

calibration <- hsi_calibration_from_scale(
  scale_points,
  distance = 10000,
  units = "um"
)
} # }
```
