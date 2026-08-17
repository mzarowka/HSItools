# Remove continuum from hyperspectral data

Normalize spectra by removing the spectral continuum to highlight
absorption features. The continuum is the convex hull connecting local
maxima across the spectrum. Requires the
[`prospectr`](https://CRAN.R-project.org/package=prospectr) package.

## Usage

``` r
hsi_remove_continuum(x, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

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
with continuum-removed values.

## Details

Output values are bounded `[0, 1]` by definition — each band is divided
by its continuum value.

Degenerate spectra for which no valid continuum exists (e.g. cracks,
all-zero or constant spectra, pixels containing `NA`) return `NA` in all
bands instead of aborting the computation. Inspect the `NA` pattern of
the result if coverage looks unexpectedly sparse.

Wavelengths are taken from band names. If band names cannot be converted
to numeric wavelengths, band indices are used instead and a warning is
issued.

Computationally intensive; consider applying to subsets or regions of
interest rather than full-resolution data. For full-raster processing,
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md)
can distribute the workload across parallel workers.

## See also

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
[`hsi_calc_remp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_remp.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rmedian.md),
[`hsi_calc_rsd()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_rsd.md),
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md),
[`hsi_destripe()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_destripe.md),
[`hsi_mask()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_mask.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

x_crem <- hsi_remove_continuum(x)

x_crem <- hsi_remove_continuum(
  x,
  filename = "output_crem.tif",
  overwrite = TRUE
)
} # }
```
