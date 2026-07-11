# Create spatial calibration from known resolution

Create a spatial calibration when the pixel size is known directly. This
is the simplest calibration path. The value is stored internally in
micrometers per pixel.

## Usage

``` r
hsi_calibration_direct(resolution, units = "um")
```

## Arguments

- resolution:

  Numeric. Pixel size in the specified units. Single positive value.

- units:

  Character. Units of the provided resolution. One of `"um"`
  (micrometers), `"mm"`, or `"cm"`. Default `"um"`.

## Value

A named numeric with resolution in µm/px, named `"um_per_px"`.

## See also

Other HSI Calibration:
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_dims.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_scale.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/reference/hsi_drop_crs.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/reference/hsi_pixels_to_units.md),
[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/reference/hsi_set_extent.md)

## Examples

``` r
calibration <- hsi_calibration_direct(60)

calibration <- hsi_calibration_direct(0.06, units = "mm")
```
