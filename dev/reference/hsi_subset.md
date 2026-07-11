# Subset SpatRaster by wavelength

Subset SpatRaster by wavelength

## Usage

``` r
hsi_subset(x, wavelength, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data. Band names must be numeric wavelengths in nm.

- wavelength:

  Numeric vector. Wavelength(s) to extract in nm. Nearest available band
  is selected for each value.

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
subset to the requested wavelength(s).

## See also

Other Utilities:
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x |> hsi_subset(675)

x |> hsi_subset(c(650, 550, 450))

x |>
  hsi_smooth_savgol(m = 1) |>
  hsi_subset(675)
} # }
```
