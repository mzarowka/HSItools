# Extract value profile along an axis

Extract value profile along an axis

## Usage

``` r
hsi_extract_profile(
  x,
  fun = "mean",
  direction = "vertical",
  na.rm = TRUE,
  y = NULL
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- fun:

  Character. Aggregation function passed to
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).
  Default `"mean"`. Use `"modal"` for categorical data.

- direction:

  Character. Direction of profile extraction. Either `"vertical"`
  (aggregates across columns, profile along rows) or `"horizontal"`
  (aggregates across rows, profile along columns). Default `"vertical"`.

- na.rm:

  Logical. Remove `NA` values. Default `TRUE`.

- y:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with layers `row_um` and `col_um`. When provided, the `position`
  column is expressed in physical units rather than pixel coordinates.
  Default `NULL`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- position:

  Numeric. Position along the profile axis, in pixel coordinates or
  physical units when `y` is supplied or `x` carries unit metadata.

- ...:

  One column per input layer, named after band names.

## Details

Aggregation is performed perpendicular to the profile direction using
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).
Crop `x` with
[`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html)
before calling this function to restrict extraction to a region of
interest.

When `y` is provided, the relevant coordinate layer (`row_um` for
vertical, `col_um` for horizontal) is aggregated with `fun = "mean"`
independently of `fun`, as physical position is a geometric property not
a statistical summary.

Band names conflicting with reserved column names (`"x"`, `"y"`,
`"position"`) are prefixed with `"band_"` and a warning is emitted.

## See also

[`hsi_extract_spectrum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_extract_spectrum.md)
for extracting an averaged spectrum.

Other HSI Extraction:
[`hsi_extract_spectrum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_extract_spectrum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("RABD_index.tif")
um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)

# Pixel-space profile
x_profile <- hsi_extract_profile(x)

# Physical-space profile
ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
x_cal <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
x_profile <- hsi_extract_profile(x_cal)

# Region of interest
x_profile <- x |>
  terra::crop(my_extent) |>
  hsi_extract_profile()

# Horizontal profile
x_profile <- hsi_extract_profile(x_cal, direction = "horizontal")

# Classified raster
x_class <- terra::rast("classified.tif")
x_profile <- hsi_extract_profile(x_class, fun = "modal")
} # }
```
