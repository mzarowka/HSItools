# Hyperspectral reflectance raster

Convert raw hyperspectral imaging data (digital numbers) to calibrated
reflectance values using white and dark reference measurements. This is
the essential first step in hyperspectral data processing.

## Usage

``` r
hsi_calc_reflectance(
  x,
  whiteref,
  darkref,
  darkspec = NULL,
  tint = c(1, 1),
  in_memory = FALSE,
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

- in_memory:

  Logical. Process entirely in RAM. Default `FALSE`. Set `TRUE` only
  when data fits comfortably in available memory. When `FALSE`, writes
  one temporary file per band.

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
with normalized reflectance values.

## Details

All inputs must share the same spatial resolution, number of bands,
wavelength labels, and compatible spatial extents. When reading `.raw`
ESRI data, load with `terra::rast(x, noflip = TRUE)`.

Three calibration paths are supported:

**Single session** (`darkspec = NULL`, `tint = c(1, 1)`): specimen,
white reference, and dark reference all share the same integration time.
No scaling is needed. This is the simplest workflow and produces correct
reflectance, though signal-to-noise is lower than with a dual-exposure
strategy.

**Matched darks** (`darkspec` provided): a dual-exposure workflow where
the specimen is overexposed relative to the white reference to maximise
signal. Each subtraction uses the dark reference captured at the
matching integration time. This is the recommended approach for
dual-exposure scanning. For example, Lumo Scanner always captures a dark
reference per session, so matched darks should be available for all
standard workflows.

\$\$R(\lambda) = \frac{specimen - dark\_{specimen}}{white -
dark\_{white}} \times \frac{t\_{white}}{t\_{specimen}}\$\$

**Scaled dark** (`darkspec = NULL`, `tint` values differ): fallback for
dual-exposure workflows when only the white-session dark reference is
available. The dark reference is scaled by the integration time ratio
before numerator subtraction. This assumes dark current scales linearly
with integration time. In practice, some detectors (SWIR) have a large
fixed-pattern noise component that does not scale with exposure time.
Scaling overestimates the specimen dark current, producing severely
degraded reflectance — often negative across entire spectra. Use only as
a last resort.

\$\$R(\lambda) = \frac{specimen - dark\_{white} \times
\frac{t\_{specimen}}{t\_{white}}}{white - dark\_{white}} \times
\frac{t\_{white}}{t\_{specimen}}\$\$

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
x <- terra::rast("capture/testdata.tif")
whiteref <- terra::rast("capture/WHITEREF_testdata.tif")
darkref <- terra::rast("capture/DARKREF_testdata.tif")

# Path 1: single session, equal integration times
x_reflectance <- hsi_calc_reflectance(
  x = x,
  whiteref = whiteref,
  darkref = darkref
)

# Path 2a: matched darks (recommended)
darkspec <- terra::rast("specimen/DARKREF_testdata.tif")

x_reflectance <- hsi_calc_reflectance(
  x = x,
  whiteref = whiteref,
  darkref = darkref,
  darkspec = darkspec,
  tint = c(3, 9)
)

# Path 2b: scaled dark (single dark, different integration times)
x_reflectance <- hsi_calc_reflectance(
  x = x,
  whiteref = whiteref,
  darkref = darkref,
  tint = c(3, 9),
  filename = "output_reflectance.tif",
  overwrite = TRUE
)
} # }
```
