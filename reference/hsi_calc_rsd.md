# Calculate standard deviation of reflectance (Rsd)

Calculate standard deviation of reflectance (Rsd)

## Usage

``` r
hsi_calc_rsd(
  x,
  index_name = NULL,
  na.rm = TRUE,
  filename = "",
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- index_name:

  Character. Name for the output layer. Default `NULL`.

- na.rm:

  Logical. Remove `NA` values. Default `TRUE`.

- filename:

  Character. Output filename. Default `""` keeps result in memory.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with reflectance standard deviation values.

## See also

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_ndi.md),
[`hsi_calc_raba()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_raba.md),
[`hsi_calc_rabd()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rabd.md),
[`hsi_calc_ratio()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_ratio.md),
[`hsi_calc_rcv()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rcv.md),
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_reflectance.md),
[`hsi_calc_remp()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_remp.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rmedian.md),
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_stretch.md),
[`hsi_destripe()`](https://mzarowka.github.io/HSItools/reference/hsi_destripe.md),
[`hsi_remove_continuum()`](https://mzarowka.github.io/HSItools/reference/hsi_remove_continuum.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/reference/hsi_smooth_savgol.md),
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

x_rsd <- hsi_calc_rsd(x)

x_rsd <- hsi_calc_rsd(
  x,
  index_name = "sd_reflectance",
  filename = "output_rsd.tif",
  overwrite = TRUE
)
} # }
```
