# Calculate lambdaREMP

Calculate lambda REMP (Red-Edge Minimum Point), the wavelength at which
the first derivative of reflectance crosses zero within a defined search
range.

## Usage

``` r
hsi_calc_remp(
  x,
  search_range = c(660, 680),
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with the first derivative of reflectance. Compute with
  [`hsi_smooth_savgol(x, m = 1)`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md).

- search_range:

  Numeric vector of length 2. Wavelength range in nm to search for the
  red-edge minimum point. Default `c(660, 680)`.

- index_name:

  Character. Name for the output layer. Default `NULL`.

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
with lambdaREMP values (wavelength in nm).

## Details

Lambda REMP is the inflection point where reflectance transitions from
decreasing to increasing — typically between 660–680 nm — and is
sensitive to chlorophyll-a concentration.

The algorithm:

1.  Subsets the derivative raster to `search_range`.

2.  Identifies zero-crossings using
    [`gsignal::zerocrossing()`](https://rdrr.io/pkg/gsignal/man/zerocrossing.html).

3.  Uses linear interpolation to find the exact wavelength where the
    derivative equals zero.

4.  Falls back to the wavelength nearest zero if no crossing is found.

## References

Ghanbari, H., Zilkey, D.R., Gregory-Eaves, I., Antoniades, D., 2023. A
new index for the rapid generation of chlorophyll time series from
hyperspectral imaging of sediment cores. Limnology and Oceanography:
Methods 21, 703-717.
[doi:10.1002/lom3.10576](https://doi.org/10.1002/lom3.10576)

## See also

[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md)
for computing the derivative input,
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md)
for wavelength range extraction.

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ndi.md),
[`hsi_calc_preview()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_preview.md),
[`hsi_calc_raba()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_raba.md),
[`hsi_calc_rabd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rabd.md),
[`hsi_calc_ratio()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ratio.md),
[`hsi_calc_rcv()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rcv.md),
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmedian.md),
[`hsi_calc_rsd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rsd.md),
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md),
[`hsi_destripe()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_destripe.md),
[`hsi_mask()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_mask.md),
[`hsi_remove_continuum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_remove_continuum.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

x_deriv <- hsi_smooth_savgol(x, m = 1)

x_remp <- hsi_calc_remp(x_deriv)

x_remp <- hsi_calc_remp(x_deriv, search_range = c(665, 690))

x_remp <- hsi_calc_remp(
  x_deriv,
  index_name = "remp",
  filename = "output_remp.tif",
  overwrite = TRUE
)
} # }
```
