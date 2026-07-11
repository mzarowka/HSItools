# Extract averaged spectrum from hyperspectral raster

Aggregate a hyperspectral raster to a single spectrum representing the
mean (or other summary) across all pixels. Useful for examining
representative spectral signatures of regions of interest.

## Usage

``` r
hsi_extract_spectrum(x, fun = "mean")
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data. Band names must be numeric wavelengths in nm.

- fun:

  Character. Aggregation function passed to
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).
  Default `"mean"`. Other
  [`terra`](https://rspatial.github.io/terra/reference/terra-package.html)
  summary functions are also supported.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- wavelength:

  Numeric. Wavelength in nm.

- value:

  Numeric. Aggregated reflectance value.

## Details

Returns one value per wavelength band. To extract a spectrum from a
specific region, crop the raster first with
[`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html).

## See also

[`hsi_extract_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_profile.md)
for extracting values along a spatial axis.

Other HSI Extraction:
[`hsi_extract_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_profile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")

spectrum <- hsi_extract_spectrum(x)

spectrum_roi <- x |>
  terra::crop(my_extent) |>
  hsi_extract_spectrum()

spectrum_median <- hsi_extract_spectrum(x, fun = "median")
} # }
```
