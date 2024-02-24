
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HSItools

<!-- badges: start -->

[![R-CMD-check](https://github.com/mzarowka/HSItools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mzarowka/HSItools/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

HSItools is an R package to process and visualize hyperspectral core
scanning data.

## Installation

You can install the development version of HSItools like so:

``` r
# install.packages("pak")

# pak::pak("mzarowka/HSItools)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(HSItools)

# Run shiny app and store results for further processing
core <- run_core()

# Get reflectance, normalize the capture
reflectance <- core |>
  prepare_core()

# Create RGB preview - to file
rgb_preview <- reflectance |>
  stretch_raster_full(reflectance, write = TRUE)

# Plot RGB preview
rgb_plot <- reflectance |>
  plot_raster_rgb(reflectance)
```

Maurycy Żarczyński is founded by the Polish National Agency for Academic
Exchange (BPN/BEK/2021/1/00133/U/0001).
