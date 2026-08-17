# Write HSI reflectance raster as a scaled integer or float GeoTIFF

Write a reflectance raster to a GeoTIFF, optionally scaling float values
to an integer datatype. The scale factor is embedded in GeoTIFF band
metadata so
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
reads back float values transparently — no manual rescaling required.

## Usage

``` r
hsi_write_scaled(
  x,
  filename,
  scale_factor = 10000L,
  overwrite = FALSE,
  datatype = "INT2U",
  ...
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- filename:

  Character. Output file path. Always writes to disk.

- scale_factor:

  Numeric. Scale factor applied before writing. Default `10000` gives 4
  decimal places of precision. Ignored for float datatypes.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

- datatype:

  Character. Output datatype. One of `"INT1U"`, `"INT2U"`, `"INT2S"`,
  `"INT4U"`, `"INT4S"`, `"FLT4S"`, `"FLT8S"`. Default `"INT2U"`. Integer
  types apply `scale_factor` and are range-checked before writing. Float
  types write values as-is with no scaling or range validation. Use
  `"FLT4S"` for sensors with low SNR such as SWIR.

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with values written to `filename`.

## Details

For integer datatypes, values are multiplied by `scale_factor` before
writing and the reciprocal is stored as GDAL scale metadata. An error is
raised if any value falls outside the storable range for the chosen
datatype at the given scale factor, in either direction. The floor
matters as much as the ceiling: unsigned types cannot hold negative
values, and GDAL clamps them on write without reporting how many were
affected. Integer storage reduces file size by approximately 50%
relative to float32 before compression.

For float datatypes (`"FLT4S"`, `"FLT8S"`), `scale_factor` has no effect
and no range validation is performed.

Choose the datatype based on sensor characteristics. VNIR sensors with
high SNR are well suited to `"INT2U"` at the default scale factor.
Sensors with lower SNR, such as SWIR, should use `"FLT4S"` to avoid
quantization degrading meaningful signal.

## See also

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ndi.md),
[`hsi_calc_raba()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_raba.md),
[`hsi_calc_rabd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rabd.md),
[`hsi_calc_ratio()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ratio.md),
[`hsi_calc_rcv()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rcv.md),
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md),
[`hsi_calc_remp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_remp.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmedian.md),
[`hsi_calc_rsd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rsd.md),
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md),
[`hsi_destripe()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_destripe.md),
[`hsi_mask()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_mask.md),
[`hsi_remove_continuum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_remove_continuum.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE.tif")

# Default: scaled uint16 for VNIR
x_scaled <- hsi_write_scaled(
  x,
  filename = "REFLECTANCE_scaled.tif",
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
)

# Float32 for SWIR
x_scaled <- hsi_write_scaled(
  x,
  filename = "REFLECTANCE_swir.tif",
  datatype = "FLT4S"
)

terra::rast("REFLECTANCE_scaled.tif")
} # }
```
