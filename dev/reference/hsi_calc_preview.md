# Calibrate a three-band preview composite

Calibrate only the three bands needed for a false-colour composite,
instead of converting an entire cube to reflectance and discarding all
but three bands afterwards. Intended for rapid visual inspection and
markup.

## Usage

``` r
hsi_calc_preview(
  x,
  whiteref,
  darkref,
  darkspec = NULL,
  tint = c(1, 1),
  type,
  tol = 25,
  filename = "",
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with raw hyperspectral sample data. Band names must be numeric
  wavelengths in nm.

- whiteref:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with white reference data. Must have the same bands and wavelengths as
  `x`.

- darkref:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with dark reference data from the white reference session. Must have
  the same bands and wavelengths as `x`.

- darkspec:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with dark reference data from the specimen session. Default `NULL`.
  Required for dual-exposure workflows where `tint` values differ. When
  `NULL` and `tint = c(1, 1)`, a single dark reference is sufficient.

- tint:

  Numeric vector of length 2. Integration times for white reference and
  specimen capture, in that order. Default `c(1, 1)` assumes equal
  integration times.

- type:

  Character or numeric. A predefined band combination (`"RGB"`, `"CIR"`,
  `"NIR"`, `"SWIR"`) or a numeric vector of exactly 3 wavelengths in nm.

- tol:

  Numeric. Wavelength tolerance for band matching in nm. Default `25`.

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
with 3 reflectance bands, named by the wavelengths of the matched bands.

## Details

The requested wavelengths are resolved against the band grid of `x`
once, and the resulting band indices are applied to `x`, `whiteref`,
`darkref`, and `darkspec` alike. Resolving each raster independently
would allow rounding differences between band labels to pull the
specimen and its references onto different bands, silently calibrating
one wavelength against another. All inputs must therefore share the same
number of bands.

Calibration is delegated to
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md)
and follows the same three paths, selected by `darkspec` and `tint`; see
its documentation for the formulas. Processing is forced in memory,
since three bands are small by construction.

Bands are matched to the nearest available wavelength, but a match
further than `tol` from the request is an error rather than a silent
substitution — without it, asking for a `"SWIR"` composite of a VNIR
capture would return the nearest edge band three times over. The result
is a reflectance raster, not a stretched one; pipe it through
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md)
for display.

## See also

[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md)
for calibrating a full cube.
[`hsi_calc_stretch()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_stretch.md)
for stretching the result to a displayable range.
[`hsi_plot_raster_rgb()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_raster_rgb.md)
for rendering the composite.

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
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md),
[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("capture/testdata.tif")
whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
darkref <- terra::rast("capture/DARKREF_testdata.tif")

x_preview <- hsi_calc_preview(
  x = x,
  whiteref = whiteref,
  darkref = darkref,
  type = "RGB"
)

# Ready to display
x_preview |>
  hsi_calc_stretch(type = "RGB") |>
  hsi_plot_raster_rgb()

# Custom wavelengths, matched darks, written to disk
darkspec <- terra::rast("specimen/DARKREF_testdata.tif")

x_preview <- hsi_calc_preview(
  x = x,
  whiteref = whiteref,
  darkref = darkref,
  darkspec = darkspec,
  tint = c(3, 9),
  type = c(700, 620, 540),
  filename = "output_preview.tif",
  overwrite = TRUE
)
} # }
```
