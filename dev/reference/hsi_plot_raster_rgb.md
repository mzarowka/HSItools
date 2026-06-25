# Plot a three-layer pseudoRGB SpatRaster

Plot a three-layer pseudoRGB SpatRaster

## Usage

``` r
hsi_plot_raster_rgb(x, stretch = NULL)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data. Must have exactly three layers.

- stretch:

  Character. Contrast stretch applied before rendering. One of `NULL`
  (no stretch), `"lin"` (linear stretch via
  [`terra::stretch()`](https://rspatial.github.io/terra/reference/stretch.html)),
  or `"hist"` (histogram equalization via
  [`terra::stretch()`](https://rspatial.github.io/terra/reference/stretch.html)).
  Default `NULL`.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Extend with `+` to add labels, themes, or colour scales.

## Details

Produces a minimal RGB raster map with a fixed aspect ratio and no axis
expansion. The returned ggplot carries no theme or axis labels — add
these with `+` using standard ggplot2 conventions.

With `stretch = NULL`, cell values are taken as-is on a `[0, 1]` scale
(raw reflectance). Values outside this range require `stretch = "lin"`
or `"hist"`, which rescale each layer to `[0, 255]` before rendering.

When raster unit metadata is present, the y-axis tick labels include the
unit suffix such as `0 cm` or `1.5 cm`. If no unit metadata exists,
ggplot2 default labels are used, showing pixel coordinates. Any
three-band combination can be used — RGB, CIR, SWIR false colour, or any
other composite.

## See also

[`hsi_plot_raster()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_raster.md)
for single-layer plots.
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_profile.md)
for 1-D depth profiles.

Other Plotting:
[`hsi_plot_composite()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_composite.md),
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_profile.md),
[`hsi_plot_raster()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_raster.md),
[`hsi_plot_spectrum()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_plot_spectrum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif") |> terra::subset(1:3)

# Quick pixel-space RGB plot
x_rgb <- hsi_plot_raster_rgb(x)

# With linear stretch
x_rgb <- hsi_plot_raster_rgb(x, stretch = "lin")

# Physical-space plot after calibration
um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
ref <- terra::vect(cbind(1, terra::nrow(x)), type = "points")
x_physical <- hsi_set_extent(x, reference = ref, um_per_pixel = um)
x_rgb <- hsi_plot_raster_rgb(x_physical)

# Add labels and theme with ggplot2
x_rgb +
  ggplot2::labs(x = "Width (cm)", y = "Depth (cm)") +
  ggplot2::theme_minimal()
} # }
```
