# Merge SpatRasters in a stratigraphic order

Merge SpatRasters in a stratigraphic order

## Usage

``` r
hsi_merge_rasters(x, y, filename = "", overwrite = FALSE, ...)
```

## Arguments

- x:

  a terra SpatRaster. First in the sequence.

- y:

  a terra SpatRaster. Second in the sequence.

- filename:

  Character. Output filename. Default "" keeps in memory

- overwrite:

  Logical

- ...:

  further passed to writeRaster

## Value

a terra SpatRaster. Merged inputs.

## See also

Other Utilities:
[`hsi_bind_layers()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_layers.md),
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md),
[`wavelength_position()`](https://mzarowka.github.io/HSItools/dev/reference/wavelength_position.md)
