# Check a raster for saturated pixels

Mark every pixel whose value reaches the sensor's saturation threshold.

## Usage

``` r
hsi_check_saturation(
  x,
  limit,
  collapse = FALSE,
  filename = "",
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data.

- limit:

  Numeric. Saturation threshold in the units of `x`. A pixel is
  saturated when its value is greater than or equal to `limit`.
  Required.

- collapse:

  Logical. Reduce the per-band mask to a single layer marking pixels
  saturated in any band. Default `FALSE`.

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
of `0`/`1` values marking saturated pixels. With `collapse = FALSE` it
has one layer per band of `x`, carrying the band names of `x`; with
`collapse = TRUE` it has a single layer named `"saturated"`.

## Details

`limit` has no default and is never inferred from the data. It is
instrument—specific for the digital number at which the sensor clips and
it must be supplied.

Further:

- **Intended for raw digital numbers.** Saturation is a property of the
  sensor, so the check is meaningful only before calibration. The
  function cannot verify the processing level of `x` and will not stop
  you running it on radiance or reflectance, where the result is
  meaningless.

- **A saturated white reference compromises a whole session.** It
  corrupts the denominator of every reflectance calculation that uses
  it, and the damage is invisible once calibration has been applied.
  Check references and captures right after acquisition, while
  re-scanning is still an option.

- **`NA` propagates rather than defaulting to "not saturated".** Any
  `NA` in a pixel's spectrum makes that pixel `NA` in the collapsed
  mask, even where another band of the same pixel is saturated. The
  collapsed mask therefore never reads as clean on incomplete evidence,
  which is the point — an unknown band cannot be ruled out. Raw digital
  numbers seldom contain `NA` at all; the usual source is a raster that
  has already been cropped or masked, where a background pixel is `NA`
  in every band and collapses to `NA` either way.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("CAPTURE_testdata.tif")

# Per-band mask for a 12-bit sensor
x_saturated <- hsi_check_saturation(x, limit = 4095)

# Per-band counts of saturated pixels
terra::global(x_saturated, "sum", na.rm = TRUE)

# Single-layer mask of pixels saturated in any band, written to disk
x_any <- hsi_check_saturation(
  x,
  limit = 4095,
  collapse = TRUE,
  filename = "saturated.tif",
  overwrite = TRUE
)
} # }
```
