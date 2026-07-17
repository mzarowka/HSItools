# Compute Minimum Noise Fraction transform

Compute Minimum Noise Fraction transform

## Usage

``` r
hsi_calc_mnf(x, trim = 0L, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- trim:

  Non-negative integer. Number of bands to drop symmetrically from both
  spectral edges before computing the transform. Default `0L` applies no
  trimming.

- ...:

  Additional arguments passed to
  [`spacetime::mnf()`](https://rdrr.io/pkg/spacetime/man/mnf.html),
  notably `Sigma.Noise` for a custom noise covariance matrix and `use`
  for NA handling. Default noise estimate uses `0.5 * cov(diff(x))`
  (MAF).

## Value

An object of class `c("mnf", "prcomp")` with components:

- values:

  Numeric vector of eigenvalues (noise fractions), one per band.

- rotation:

  Numeric matrix of eigenvectors (loadings).

- x:

  Numeric matrix of MNF scores (pixels x bands).

## Details

MNF components are ordered by **decreasing noise fraction**: MNF1
carries the most noise, the final components carry the most signal.
Eigenvalues approximate the noise fraction under the proportional
covariance model and are non-negative, but may exceed 1 with real data.
`1 - eigenvalue` is the lag-1 autocorrelation of that component. Inspect
`$values` to identify the signal-rich tail before passing the result to
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md).

When `x` has been pre-processed with
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
spectral edge bands become nearly collinear due to the polynomial
fitting, which can make the noise covariance matrix singular. Set `trim`
to `ceiling(n / 2)` where `n` is the window size passed to
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md)
to drop the affected bands from both ends before computing the
transform.

Wraps [`spacetime::mnf()`](https://rdrr.io/pkg/spacetime/man/mnf.html)
by Edzer Pebesma, implementing the algorithm of Green et al. (1988) with
noise estimation following Switzer & Green (1984).

Green, A.A., Berman, M., Switzer, P. and Craig, M.D. (1988). A
transformation for ordering multispectral data in terms of image quality
with implications for noise removal. *IEEE Transactions on Geoscience
and Remote Sensing*, 26(1), 65–74.

Switzer, P. and Green, A. (1984). Min/max autocorrelation factors for
multivariate spatial imagery. Technical Report, Department of
Statistics, Stanford University.

## See also

[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md),
[`spacetime::mnf()`](https://rdrr.io/pkg/spacetime/man/mnf.html)

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_difference.md),
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
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")
x_mnf <- hsi_calc_mnf(x)
x_mnf$values

# After hsi_smooth_savgol() with window = 15
x_mnf <- hsi_calc_mnf(x, trim = 8L)
} # }
```
