# Drop CRS from a SpatRaster or SpatVector

Drop CRS from a SpatRaster or SpatVector

## Usage

``` r
hsi_drop_crs(x)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

## Value

`x` with CRS set to `""`.

## Details

GIS software (e.g. QGIS) often assigns a default CRS such as WGS84 to
data that is in pixel coordinate space. HSItools calibration and
co-registration functions require a `NULL` CRS to avoid
misinterpretation of pixel coordinates as geographic coordinates. Use
this function to strip an unwanted CRS before passing data to those
functions.

## See also

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_dims.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_pixels_to_units.md),
[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_set_extent.md)

## Examples

``` r
x <- terra::rast(nrows = 9, ncols = 9, nlyr = 3)
terra::crs(x) <- "EPSG:4326"

x_stripped <- hsi_drop_crs(x)
```
