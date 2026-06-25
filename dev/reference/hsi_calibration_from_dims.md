# Create spatial calibration from image dimensions and physical distance

Create a spatial calibration from total pixel count and total physical
distance. Useful when scanner metadata provides the scan length and the
image dimensions are known.

## Usage

``` r
hsi_calibration_from_dims(pixels, distance = 1000, units = "um")
```

## Arguments

- pixels:

  Numeric. Total number of pixels along the measured axis. Single
  positive value.

- distance:

  Numeric. Total physical distance corresponding to those pixels. Single
  positive value. Default `1000`.

- units:

  Character. Units of the provided distance. One of `"um"`
  (micrometers), `"mm"`, or `"cm"`. Default `"um"`.

## Value

A named numeric with resolution in µm/px, named `"um_per_px"`.

## See also

Other HSI Calibration:
[`hsi_calibration_direct()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_direct.md),
[`hsi_calibration_from_scale()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calibration_from_scale.md),
[`hsi_drop_crs()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_drop_crs.md),
[`hsi_pixels_to_units()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_pixels_to_units.md),
[`hsi_set_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_set_extent.md)

## Examples

``` r
calibration <- hsi_calibration_from_dims(
  pixels = 2500,
  distance = 150,
  units = "mm")

if (FALSE) { # \dontrun{
raster <- terra::rast("scan.tif")

calibration <- hsi_calibration_from_dims(
  pixels = terra::nrow(raster),
  distance = 150,
  units = "mm"
)
} # }
```
