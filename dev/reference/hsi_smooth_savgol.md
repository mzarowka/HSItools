# Spectral raster smooth with a Savitzky-Golay filter

Smooth hyperspectral data using a Savitzky-Golay filter via
[`gsignal::sgolayfilt()`](https://rdrr.io/pkg/gsignal/man/sgolayfilt.html).
The filter fits successive subsets of adjacent data points with a
low-degree polynomial by the method of linear least squares.

## Usage

``` r
hsi_smooth_savgol(
  x,
  p = 3,
  n = p + 13 - p%%2,
  m = 0,
  ts = 1,
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

- p:

  Integer. Filter polynomial order. Typically 2–4. Default `3`.

- n:

  Positive odd integer. Filter window size. Must be odd and greater than
  `p`. Typically 5–15. Default computed from `p`.

- m:

  Integer. Derivative order. `0` for smoothing, `1` for first
  derivative. Default `0`.

- ts:

  Numeric. Sampling interval for derivative calculations. Default `1`.

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
with Savitzky-Golay filtered values.

## Details

The Savitzky-Golay filter preserves spectral features such as peak
height and width that are typically flattened by other smoothing
methods. The filter fits a polynomial of order `p` through a moving
window of `n` points.

Setting `m = 1` or `m = 2` computes the first or second derivative of
the smoothed spectrum respectively; higher-order derivatives are also
supported by increasing `m`. Note that edge bands equal to roughly half
the window size are unreliable for derivatives — always compute on the
full spectrum before subsetting to a wavelength range of interest.

Pixels with `NA` values will cause the function to fail. For full-raster
processing,
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md)
can distribute the workload across parallel workers. Requires the
[`gsignal`](https://CRAN.R-project.org/package=gsignal) package.

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
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

x_savgol <- hsi_smooth_savgol(x)

x_savgol <- hsi_smooth_savgol(
  x,
  filename = "output_savgol.tif",
  overwrite = TRUE
)
} # }
```
