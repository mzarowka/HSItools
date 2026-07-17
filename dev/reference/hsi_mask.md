# Mask hyperspectral raster

Mask hyperspectral raster

## Usage

``` r
hsi_mask(x, mask, inverse = FALSE, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- mask:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with a single mask layer. Must have the same extent and resolution as
  `x`.

- inverse:

  Logical. How `mask` is read. Default `FALSE`.

  `FALSE`

  :   Keep-mask: nonzero cells are kept, `0` cells dropped.

  `TRUE`

  :   Bad-mask: nonzero cells are dropped, `0` cells kept.

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
with masked values.

## Details

Dropped cells become `NA` in every layer of `x`. A single mask layer is
recycled across all layers, so each cell is kept or dropped for its
whole spectrum.

Cells that are `NA` in `mask` are dropped whatever `inverse` is set to.
`NA` marks a cell the mask carries no information about, and an unknown
cell is discarded rather than trusted. This departs from
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html),
which keeps `NA` cells when `inverse = TRUE`.

Mask *creation* is out of scope: what counts as background, defect, or
specimen is domain knowledge that varies by material and study. Masks
are built upstream, whether by thresholding a band or index, by
rasterising digitised polygons with
[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html),
or by any other rule.

Combine several sources in mask space rather than calling this function
repeatedly. Masks are single-layer and cheap to combine, while `x` is
the expensive side. Keep every source on one polarity while combining:
bad-masks combine as a union
([`any()`](https://rdrr.io/r/base/any.html)), keep-masks as an
intersection ([`all()`](https://rdrr.io/r/base/all.html)), and mixing
the two silently turns one into the other. Note that
[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html)
writes `NA`, not `0`, outside its polygons, so pass `background = 0` to
keep the result usable in that algebra.

Masking precedes endmember search and pixel statistics. Background and
tray pixels are spectrally distinct from the specimen, and will be
selected as endmembers or skew band statistics if left in.

## See also

[`hsi_destripe()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_destripe.md),
which likewise requires background pixels excluded.

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
[`hsi_remove_continuum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_remove_continuum.md),
[`hsi_smooth_median()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_median.md),
[`hsi_smooth_savgol()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_smooth_savgol.md),
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

# Keep-mask: nonzero marks the specimen to keep
specimen <- terra::rast("SPECIMEN_testdata.tif")

x_mask <- hsi_mask(x, specimen)

# Bad-mask: nonzero marks defects to drop
defects <- terra::rast("DEFECTS_testdata.tif")

x_mask <- hsi_mask(x, defects, inverse = TRUE)

x_mask <- hsi_mask(
  x,
  defects,
  inverse = TRUE,
  filename = "REFLECTANCE_mask.tif",
  overwrite = TRUE
)
} # }
```
