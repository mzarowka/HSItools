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
