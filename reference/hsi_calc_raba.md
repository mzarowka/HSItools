# Calculate Relative Absorption Band Area (RABA)

Calculate Relative Absorption Band Area (RABA), which quantifies the
total absorption across a spectral feature by summing band-by-band RABD
calculations. Implementation follows Butz et al. (2015) formula in HSI
scanning manual.

## Usage

``` r
hsi_calc_raba(
  x,
  continuum_edges,
  index_name = NULL,
  filename = "",
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A terra SpatRaster with hyperspectral data

- continuum_edges:

  Numeric vector of length 2. Wavelength boundaries (in nm) that define
  the continuum for the calculation window

- index_name:

  Character. Name of calculated RABA index. Default NULL

- filename:

  Character. Output filename. Default "" keeps in memory

- overwrite:

  Logical. Overwrite existing file (default: FALSE)

- ...:

  Additional arguments passed to
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

A terra SpatRaster with RABA values

## Details

RABA extends the RABD concept from a single point to the entire
absorption feature. The method calculates RABD at each wavelength
between the continuum edges and sums them. The continuum is calculated
using linear interpolation.

This approach:

- Uses the same continuum concept as
  [`hsi_calc_rabd`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rabd.md)

- Integrates across the entire absorption feature

- Is bandwidth-independent (works with any spectral resolution)

- Provides a measure of total absorption strength

## See also

[`hsi_calc_rabd`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rabd.md)
for single-point absorption depth

Other HSI Transformations:
[`hsi_apply_mnf()`](https://mzarowka.github.io/HSItools/reference/hsi_apply_mnf.md),
[`hsi_calc_difference()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_difference.md),
[`hsi_calc_mnf()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_mnf.md),
[`hsi_calc_ndi()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_ndi.md),
[`hsi_calc_rabd()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rabd.md),
[`hsi_calc_ratio()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_ratio.md),
[`hsi_calc_rcv()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rcv.md),
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_reflectance.md),
[`hsi_calc_remp()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_remp.md),
[`hsi_calc_rmean()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rmean.md),
[`hsi_calc_rmedian()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rmedian.md),
[`hsi_calc_rsd()`](https://mzarowka.github.io/HSItools/reference/hsi_calc_rsd.md),
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
# Load hyperspectral data
x <- terra::rast("REFLECTANCE_testdata.tif")

# Calculate RABA for chlorophyll-a (typical range 650-700 nm)
x_raba <- hsi_calc_raba(
  x,
  continuum_edges = c(650, 700)
)

# Save to file and provide a name
x_raba <- hsi_calc_raba(
  x = reflectance,
  continuum_edges = c(650, 700),
  index_name = "raba_650700",
  filename = "raba_output.tif",
  overwrite = TRUE
)
} # }
```
