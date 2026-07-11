# Bind hyperspectral SpatRasters along the y-axis

Bind hyperspectral SpatRasters along the y-axis

## Usage

``` r
hsi_bind_rows(x, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  List of
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  objects with hyperspectral data, in the desired top-to-bottom order.
  All elements must share the same number of layers, layer names, and
  spatial resolution.

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
with all inputs bound along the y-axis in the order provided.

## Details

Concatenates SpatRasters along the y-axis in the order provided. All
rasters are left-aligned to `xmin = 0`. Rasters narrower than the widest
input are padded with `NA` columns on the right via
[`terra::extend()`](https://rspatial.github.io/terra/reference/extend.html),
so the output forms a rectangular block rather than a Tetris-like shape.

Resolution, layer count, and layer names (wavelengths) must be identical
across all inputs — the function aborts if any mismatch is detected.

## See also

Other Utilities:
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sections <- list(section_01, section_02, section_03, section_04)
x_bound <- hsi_bind_rows(sections, filename = "bound.tif", overwrite = TRUE)
} # }
```
