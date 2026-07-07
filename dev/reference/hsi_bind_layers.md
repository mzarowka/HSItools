# Bind hyperspectral SpatRasters along the spectral axis

Bind hyperspectral SpatRasters along the spectral axis

## Usage

``` r
hsi_bind_layers(x, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  List of
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  objects with hyperspectral data. All elements must share the same
  spatial resolution and spatial extent (pixel to pixel coverage).

- filename:

  Character. Output filename. Default `""` keeps result in memory.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

- ...:

  Additional arguments passed to
  [`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with all inputs bound along the spectral axis in the order provided.

## See also

Other Utilities:
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)
