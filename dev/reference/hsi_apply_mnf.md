# Apply Minimum Noise Fraction transformation to a SpatRaster

Apply Minimum Noise Fraction transformation to a SpatRaster

## Usage

``` r
hsi_apply_mnf(x, fit, n = NULL, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- fit:

  An object of class `mnf` as returned by
  [`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md).

- n:

  Positive integer. Number of signal-rich MNF components to retain.
  Inspect `fit$values` to choose. Default `NULL`.

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
with `n` MNF component layers.

## Details

MNF components are ordered by decreasing noise fraction: the first
component carries the most noise and the last carries the most signal.
`n` selects the signal-rich tail, so the output layer `MNF_1` always
corresponds to the most signal-rich component regardless of the total
number of components computed. The internal index reversal is hidden
from the user.

`x` is used only as a spatial template (extent, CRS, resolution). Its
band count is irrelevant — only cell count must match `nrow(fit$x)`.

Separating
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md)
from `hsi_apply_mnf()` means the expensive eigen decomposition runs
once; `hsi_apply_mnf()` can be called repeatedly with different values
of `n` without recomputing the transform.

## See also

[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md)

Other HSI Transformations:
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_ndi.md),
[`hsi_calc_preview()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_preview.md),
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
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")
fit <- hsi_calc_mnf(x)
fit$values

x_mnf <- hsi_apply_mnf(x, fit = fit, n = 10L)

x_mnf <- hsi_apply_mnf(
  x,
  fit = fit,
  n = 10L,
  filename = "output_mnf.tif",
  overwrite = TRUE
)
} # }
```
