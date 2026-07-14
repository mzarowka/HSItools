# Basic HSItools workflow

## Setup

``` r

library(HSItools)
```

## Reflectance

Typically, you need to calculate reflectance from raw data captured
during the scan. HSItools makes this straightforward using a combination
of {[terra](https://CRAN.R-project.org/package=terra)} and {HSItools}
functions.

First, locate your data. Because no assumptions about directory
structure are made, you can use external white and dark reference files
in this step. If you are reading data from ENVI `.raw` files, it may be
important to use `noflip = TRUE` in your
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
call, otherwise files will be flipped vertically.

Our test dataset is a 9 × 9 pixel, 101-band subset of HSI data (517–772
nm) from the Hyperspectral Imaging Laboratory in Gdańsk. It comes from a
**single-session** scan: specimen, white reference and dark reference
were all captured at the same integration time. The dual-exposure
examples below are therefore shown but not run, as they need a second
dark reference that this dataset does not contain.

``` r

# Read in captured data and references
data <- list(
  x = terra::rast(
    system.file(
      package = "HSItools",
      "testdata/capture/testdata.tif"
    )
  ),
  whiteref = terra::rast(
    system.file(
      package = "HSItools",
      "testdata/capture/WHITEREF_testdata.tif"
    )
  ),
  darkref = terra::rast(
    system.file(
      package = "HSItools",
      "testdata/capture/DARKREF_testdata.tif"
    )
  )
)
```

Reflectance is a fraction representing the proportion of light reflected
by the specimen relative to the white reference. The `tint` argument
controls the integration times for white reference and specimen capture,
in that order. The default `c(1, 1)` assumes both use the same
integration time.

### Three calibration paths

[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md)
supports three calibration paths depending on your scanning workflow.

**Single session** is the simplest case: specimen, white reference, and
dark reference all share the same integration time. No scaling is
needed. This produces correct reflectance, though signal-to-noise is
lower than with a dual-exposure strategy that overexposes the specimen
to maximise signal.

``` r

# Single session: equal integration times (default)
r1 <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref,
  in_memory = TRUE
)
```

**Matched darks** is the recommended approach for dual-exposure
workflows where the specimen is scanned at a higher integration time
than the white reference. Each dark subtraction uses the dark reference
captured at the matching integration time, so two dark references are
needed: `darkref` from the white reference session, and `darkspec` from
the specimen session. Many scanners capture a dark reference once per
session, so both are usually already sitting in your capture folders.

``` r

# One dark reference per session, each at its own integration time
darkref <- terra::rast("white_session/DARKREF.tif") # captured at tint_white
darkspec <- terra::rast("specimen_session/DARKREF.tif") # captured at tint_specimen

# Matched darks: recommended for dual-exposure scanning
r2 <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = darkref,
  darkspec = darkspec,
  tint = c(3, 9),
  in_memory = TRUE
)
```

**Scaled dark** is a fallback for dual-exposure workflows when only the
white-session dark reference is available. The dark reference is scaled
by the integration time ratio before subtraction in the numerator. A
warning is emitted when this path is active.

> **Warning**
>
> Dark current scaling assumes linear scaling with integration time. In
> practice, numerous detectors (especially SWIR) have a large
> fixed-pattern noise component that does not scale with exposure time.
> Scaling overestimates the specimen dark current, producing severely
> degraded SWIR reflectance which is often negative across entire
> spectra. For VNIR the effect is smaller in absolute terms but we found
> that it introduces a systematic spectral tilt that biases indices
> comparing blue and NIR bands. Always prefer matched darks.

``` r

# Scaled dark: fallback, not recommended
r3 <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref,
  tint = c(3, 9),
  in_memory = TRUE
)
```

### Memory and file handling

You can control whether processing happens on disk or in RAM.
Intermediate files created *inside* the function are cleaned up when it
returns. The result itself is different: with the default
`in_memory = FALSE` and no `filename`, the returned raster is still
backed by a temporary file, and that file cannot be deleted on exit
precisely because it *is* the storage behind the object you get back. It
persists until the R session ends, and
[`hsi_calc_reflectance()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_calc_reflectance.md)
warns you when you take this path.

Two ways to avoid it: pass a `filename` to write a permanent file, or
set `in_memory = TRUE` to keep the result entirely in RAM. The latter is
only safe when the data fits comfortably in memory.

``` r

# Calculate with defaults: result backed by a session-lifetime temporary file
r4 <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref
)
#> Warning: `in_memory` is "FALSE" but no `filename` was provided.
#> ℹ Temporary files will not be cleaned up until the R session ends. Files might
#>   persist.

# Calculate in memory and write to a permanent file
r5 <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref,
  in_memory = TRUE,
  filename = tempfile(fileext = ".tif")
)

# Results should be nearly identical, differing only at distant decimal places
terra::all.equal(r1, r4)
#> [1] "Mean relative difference: 2.224413e-08"
```

## Smoothing: spatial

Before working in the spectral dimension, apply a spatial median filter
to remove outlier pixels. The default window size is 3 × 3. As always,
you can write the result to a file.

``` r

r6 <- HSItools::hsi_smooth_median(x = r5)
```

## Smoothing: spectral

With data smoothed across the spatial dimension there should be no `NA`
values in the spectral series. You can now apply a Savitzky-Golay filter
to smooth the reflectance spectrum in each pixel. Several arguments are
available to control the filter parameters. The function uses the
{[gsignal](https://CRAN.R-project.org/package=gsignal)} implementation.

``` r

r7 <- HSItools::hsi_smooth_savgol(x = r6)
```

### Derivatives

Derivatives of reflectance are useful for enhancing subtle spectral
features and are calculated in the same function by changing the
derivative order. A first derivative emphasises the rate of change in
reflectance across wavelengths and can reveal absorption features that
are difficult to detect in the raw spectrum. Set `m = 1` for a first
derivative.

Note that edge effects at the first and last few bands (roughly equal to
half the filter window width) produce unreliable derivative values. It
is best to calculate derivatives on the full spectrum and subset
afterwards.

``` r

r_derivative <- HSItools::hsi_smooth_savgol(
  x = r6,
  m = 1
)
```

## Continuum removal

Smoothed data can be enhanced further by removing the continuum, the
convex hull of the spectrum. This corrects for the broad slope inherent
in most hyperspectral data, isolating individual absorption features and
making them more comparable across pixels and samples. The function uses
the {[prospectr](https://CRAN.R-project.org/package=prospectr)}
implementation.

Continuum removal is computationally intensive. Consider applying it to
subsets or regions of interest rather than full-resolution data.

``` r

r8 <- HSItools::hsi_remove_continuum(x = r7)
```

## Workflow

With {HSItools} and {terra} it is straightforward to pipe operations
into a single workflow. You can write to a permanent file at any step by
passing a `filename` argument.

``` r

r <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref,
  in_memory = TRUE
) |>
  # Smooth spatially
  HSItools::hsi_smooth_median() |>
  # Smooth spectrally and write an intermediate checkpoint
  HSItools::hsi_smooth_savgol(filename = tempfile(fileext = ".tif")) |>
  # Remove continuum
  HSItools::hsi_remove_continuum()
```

## Tiled processing

If your computer has multiple cores you can apply pixel-wise operations
in parallel using
[`hsi_tiled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_tiled.md).
Workers are managed by
{[mirai](https://CRAN.R-project.org/package=mirai)}.

> **Caution**
>
> Tiled processing produces reliable results only for operations that
> work on a single-pixel basis. Spatial operations such as the median
> filter must never be run this way, as tiles do not share border
> information.

``` r

library(mirai)

# Set number of parallel workers.
# Reserve at least one core for the main process.
cores <- 2
mirai::daemons(cores)
```

``` r

r <- HSItools::hsi_calc_reflectance(
  x = data$x,
  whiteref = data$whiteref,
  darkref = data$darkref,
  in_memory = TRUE
) |>
  # Smooth spatially — no tiling
  HSItools::hsi_smooth_median() |>
  # Smooth spectrally — tiling is safe here
  HSItools::hsi_tiled(
    fun = \(tile) HSItools::hsi_smooth_savgol(tile),
    n_tiles = cores,
    filename = tempfile(fileext = ".tif"),
    overwrite = TRUE
  ) |>
  # Remove continuum — tiling is safe here
  HSItools::hsi_tiled(
    fun = \(tile) HSItools::hsi_remove_continuum(tile),
    n_tiles = cores,
    filename = tempfile(fileext = ".tif"),
    overwrite = TRUE
  )

# Shut down workers when done
mirai::daemons(0)
```

## Storage: quantizing

High spatial and spectral resolution cameras, especially VNIR, produce
large reflectance files once data moves from integer DN space into
floating-point numbers. One way to reduce file size is to store a
*quantized* version: values are multiplied by a known scale factor and
stored as integers, then divided back on read. This typically reduces
file size by more than 50%.

{terra} reads quantized files transparently. The tradeoff is slightly
reduced precision of the restored values, which is generally acceptable
for high-SNR sensors.

> **Warning**
>
> Do not quantize low-SNR data such as current-generation SWIR sensors.
> Quantization discards the fine floating-point differences that carry
> meaningful signal in noisy data.

[`hsi_write_scaled()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_scaled.md)
is a write-primary function: it encodes `x` to disk and returns the
original object invisibly so it can sit in a pipeline without printing.
Continue processing from the object you passed in, not from the return
value.

``` r

HSItools::hsi_write_scaled(
  x = r,
  filename = tempfile(fileext = ".tif"),
  overwrite = TRUE
)
```
