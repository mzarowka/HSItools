# Set raster extent to physical units

Set raster extent to physical units

## Usage

``` r
hsi_set_extent(raster, reference, um_per_pixel, origin = 0, units = "cm")
```

## Arguments

- raster:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- reference:

  A
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with a single point in pixel space that marks the physical origin
  anchor on the vertical axis.

- um_per_pixel:

  Numeric. Physical size of one pixel in µm.

- origin:

  Numeric. Physical position assigned to `reference` in µm. Default `0`.

- units:

  Character. Output units for the calibrated raster extent. One of
  `"um"`, `"mm"`, or `"cm"`. Default `"cm"`.

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with a physically calibrated extent.

## Details

This keeps the current raster values unchanged, but updates the spatial
extent so that downstream plotting and profile extraction work in
physical units.

The calibration is linear: the raster grid is converted from pixels to
physical units using the supplied micrometers-per-pixel ratio. The
horizontal axis starts at the first cell centre, and the vertical axis
is anchored to `reference` at `origin`.

Importantly, at this stage, calibration metag does not carry forvard
into analysis products. Calibrate immediate products, where real world
units are necessary.

## See also

[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_scale.md),
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_dims.md)
for obtaining `um_per_pixel`.
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/reference/hsi_pixels_to_units.md)
for direct conversion of pixel positions.

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_dims.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/reference/hsi_calibration_from_scale.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/reference/hsi_drop_crs.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/reference/hsi_pixels_to_units.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

# Anchor point in pixel space, e.g. the core top in the first column.
reference <- terra::vect(cbind(1, terra::nrow(x)), type = "points")

# 60 µm per pixel, supplied directly.
um <- hsi_calibration_direct(60)

x_cal <- hsi_set_extent(x, reference, um_per_pixel = um, units = "cm")
} # }
```
