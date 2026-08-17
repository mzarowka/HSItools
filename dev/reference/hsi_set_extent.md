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
  with a single point marking the physical origin anchor on the vertical
  axis. Its y value is a spatial coordinate in the raster's own frame,
  increasing upward, as produced by digitising over the raster or by
  [`terra::xyFromCell()`](https://rspatial.github.io/terra/reference/xyCellFrom.html)..
  It is converted internally to a fractional row position, so sub-pixel
  anchors are preserved and a point falling outside `raster` is
  extrapolated with a warning.

- um_per_pixel:

  Numeric. Physical size of one pixel in µm.

- origin:

  Numeric. Physical position assigned to `reference`, in µm regardless
  of `units`. Converted to `units` alongside `um_per_pixel`. Default
  `0`.

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
extent is expressed in cell edges, so the calibrated raster has a
resolution of exactly `um_per_pixel` on both axes. The horizontal axis
starts at zero, and the vertical axis is anchored so that `reference`
sits at `origin`.

Importantly, at this stage, calibration metag does not carry forvard
into analysis products. Calibrate immediate products, where real world
units are necessary.

## See also

[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_dims.md)
for obtaining `um_per_pixel`.
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_pixels_to_units.md)
for direct conversion of pixel positions.

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_dims.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_drop_crs.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_pixels_to_units.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

# Anchor on the vertical axis, e.g. the core top. The y value is a spatial
# coordinate increasing upward, so the centre of the top row sits at
# `terra::nrow(x) - 0.5`.
reference <- terra::vect(cbind(0.5, terra::nrow(x) - 0.5), type = "points")

# 60 µm per pixel, supplied directly.
um <- hsi_calibration_direct(60)

x_cal <- hsi_set_extent(x, reference, um_per_pixel = um, units = "cm")
} # }
```
