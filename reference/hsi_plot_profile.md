# Plot a depth profile

Plot a depth profile

## Usage

``` r
hsi_plot_profile(x)
```

## Arguments

- x:

  A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
  columns `position` and exactly one value column, as produced by
  [`hsi_extract_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_profile.md).

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Extend with `+` to add labels, themes, or colour scales.

## Details

Produces a minimal stratigraphic profile plot. Position is mapped to the
x-axis as the independent variable — ensuring that stats like
[`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
work correctly — then
[`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
rotates the plot so that depth runs top-to-bottom visually.
[`ggplot2::scale_x_reverse()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
places shallow positions at the top.

When profile unit metadata is present, tick labels include the unit
suffix such as `0 mm` or `1.5 cm`. If no unit metadata exists, ggplot2
default labels are used, showing pixel coordinates or raw position
values.

The returned ggplot carries no theme or axis labels — add these with `+`
using standard ggplot2 conventions.

## See also

[`hsi_extract_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_profile.md)
to produce the input tibble.
[`hsi_plot_spectrum()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_spectrum.md)
for spectral plots.
[`hsi_plot_raster()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster.md)
for spatial raster maps.

Other Plotting:
[`hsi_plot_composite()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_composite.md),
[`hsi_plot_raster()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster.md),
[`hsi_plot_raster_rgb()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster_rgb.md),
[`hsi_plot_spectrum()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_spectrum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("RABD_testdata.tif") |> terra::subset(1)
x_profile <- hsi_extract_profile(x)

# Quick pixel-space profile
x_plot <- hsi_plot_profile(x_profile)

# Physical-space profile
um <- hsi_calibration_from_dims(scan_length_um = 50000, n_pixels = 1000)
ref <- terra::vect(matrix(c(1001.5, 2007.5), ncol = 2), type = "points")
x_cal <- hsi_calibrate_raster(x, reference = ref, um_per_pixel = um)
x_profile <- hsi_extract_profile(x_cal)
x_plot <- hsi_plot_profile(x_profile)

# Add labels and theme with ggplot2
x_plot +
  ggplot2::labs(x = "Depth (cm)", y = "RABD") +
  ggplot2::theme_minimal()
} # }
```
