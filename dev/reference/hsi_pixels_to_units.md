# Convert pixel coordinates to physical units

Convert pixel coordinates to physical positions using a spatial
calibration. Optionally set an origin pixel (specimen start) so that
output positions are specimen-relative.

## Usage

``` r
hsi_pixels_to_units(
  pixels,
  calibration,
  origin = 0,
  direction = 1,
  units = "um"
)
```

## Arguments

- pixels:

  Numeric. Pixel coordinates to convert.

- calibration:

  Numeric. Spatial calibration in µm/px, as created by any
  `hsi_calibration_*` function.

- origin:

  Numeric. Pixel coordinate that corresponds to position zero (default
  0).

- direction:

  Numeric. Either 1 or -1. Controls the sign of the output (default 1).
  Set to -1 to flip direction, which is typically needed for vertical
  profiles in terra where y-coordinates decrease downward but physical
  positions should increase.

- units:

  Character. Output units. One of "um" (micrometers, default), "mm", or
  "cm".

## Value

Numeric vector. Physical positions in the requested units. Same length
as input `pixels`.

## Details

The conversion formula is:

`position = (pixels - origin) * calibration * direction`

The result is in µm and then converted to the requested output units.

In terra, y-coordinates and row indices run in opposite directions:
y-coordinates decrease downward while row indices increase downward. The
appropriate `direction` depends on what you pass as `pixels`:

- y-coordinates (e.g., from
  [`hsi_extract_profile()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_extract_profile.md)):
  use `direction = -1` for positions increasing along the specimen

- Row indices: use `direction = 1` (default)

## See also

[`hsi_calibration_direct`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_scale`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_calibration_from_dims`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_dims.md)
for creating calibration objects

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_dims()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_dims.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_drop_crs.md),
[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_set_extent.md)

## Examples

``` r
# Create calibration: 60 µm per pixel
calibration <- hsi_calibration_direct(60)

# Row indices are increasing downward, direction = 1 (default)
hsi_pixels_to_units(c(0, 10, 20), calibration)
#> [1]    0  600 1200

# Row indices with origin and specimen starts at row 50
hsi_pixels_to_units(c(50, 60, 70), calibration, origin = 50)
#> [1]    0  600 1200

# Y-coordinates from terra and decreasing downward, use direction = -1
hsi_pixels_to_units(c(850, 840, 830), calibration, origin = 850, direction = -1)
#> [1]    0  600 1200

# Output in mm
hsi_pixels_to_units(c(0, 10, 20), calibration, units = "mm")
#> [1] 0.0 0.6 1.2
```
