# Plot a reflectance spectrum

Plot a reflectance spectrum

## Usage

``` r
hsi_plot_spectrum(x)
```

## Arguments

- x:

  A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
  columns `wavelength` and `value`, as produced by
  [`hsi_extract_spectrum()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_spectrum.md).

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Extend with `+` to add labels, themes, or colour scales.

## Details

Produces a minimal line plot of reflectance against wavelength. The
returned ggplot carries no theme or axis labels — add these with `+`
using standard ggplot2 conventions.

## See also

[`hsi_extract_spectrum()`](https://mzarowka.github.io/HSItools/reference/hsi_extract_spectrum.md)
to produce the input tibble.
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_profile.md)
for 1-D depth profiles.

Other Plotting:
[`hsi_plot_composite()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_composite.md),
[`hsi_plot_profile()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_profile.md),
[`hsi_plot_raster()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster.md),
[`hsi_plot_raster_rgb()`](https://mzarowka.github.io/HSItools/reference/hsi_plot_raster_rgb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- terra::rast("REFLECTANCE_testdata.tif")
x_spectrum <- hsi_extract_spectrum(x)

# Quick plot
x_plot <- hsi_plot_spectrum(x_spectrum)

# Add labels and theme with ggplot2
x_plot +
  ggplot2::labs(x = "Wavelength (nm)", y = "Reflectance") +
  ggplot2::theme_minimal()
} # }
```
