# Find position of selected wavelengths

Find band index positions by matching requested wavelengths to the
nearest available band. When multiple requested wavelengths resolve to
the same band index, only the last is retained.

## Usage

``` r
wavelength_position(x, wavelength)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- wavelength:

  Numeric vector. Desired wavelengths in nm.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- wavelength:

  Numeric. Requested wavelength in nm.

- position:

  Integer. Corresponding band index in `x`.

- band_wavelength:

  Numeric. Actual wavelength of the matched band in nm.

## See also

Other Utilities:
[`hsi_bind_rows()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_bind_rows.md),
[`hsi_find_extent()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_find_extent.md),
[`hsi_subset()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset.md),
[`hsi_subset_range()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_subset_range.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 5)
names(r) <- c("400", "500", "600", "700", "800")

wavelength_position(r, c(450, 650))
} # }
```
