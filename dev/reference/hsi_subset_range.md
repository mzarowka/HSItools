# Subset SpatRaster by wavelength range

Subset SpatRaster by wavelength range

## Usage

``` r
hsi_subset_range(x, from, to, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data. Band names must be numeric wavelengths in nm.

- from:

  Numeric. Start wavelength of the range in nm, inclusive.

- to:

  Numeric. End wavelength of the range in nm, inclusive.

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
with all bands within the specified wavelength range.

## See also

Other Utilities:
[`hsi_bind_layers()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_layers.md),
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x |> hsi_subset_range(from = 660, to = 680)

x |>
  hsi_smooth_savgol(m = 1) |>
  hsi_subset_range(from = 680, to = 750)
} # }
```
