# Plot a single-layer SpatRaster

Plot a single-layer SpatRaster

## Usage

``` r
hsi_plot_raster(x)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with hyperspectral data. Must be single-layer.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Extend with `+` to add labels, themes, or colour scales.

## Details

Produces a minimal raster map with a fixed aspect ratio and no axis
expansion. The returned ggplot carries no theme, colour scale, or axis
labels — add these with `+` using standard ggplot2 conventions.

When raster unit metadata is present, the y-axis tick labels include the
unit suffix such as `0 cm` or `1.5 cm`. If no unit metadata exists,
ggplot2 default labels are used, showing pixel coordinates.

## See also

[`hsi_plot_raster_rgb()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster_rgb.md)
for three-layer RGB plots.
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_profile.md)
for 1-D depth profiles.

Other Plotting:
[`hsi_plot_composite()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_composite.md),
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_profile.md),
[`hsi_plot_raster_rgb()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster_rgb.md),
[`hsi_plot_spectrum()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_spectrum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif") |> terra::subset(1)

# Quick pixel-space plot
x_raster <- hsi_plot_raster(x)

# Physical-space plot after calibration
um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
x_physical <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
x_raster <- hsi_plot_spatraster(x_physical)

# Add labels and theme with ggplot2
x_raster +
  ggplot2::labs(x = "Width (cm)", y = "Depth (cm)", fill = "RABD") +
  ggplot2::theme_minimal()
} # }
```
