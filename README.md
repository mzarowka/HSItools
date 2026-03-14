
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HSItools

<!-- badges: start -->

[![R-CMD-check](https://github.com/mzarowka/HSItools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mzarowka/HSItools/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

HSItools is an R package to process and visualize hyperspectral core
scanning data.

<figure>
<img src="man/figures/package_logo.png" width="150"
alt="HSItools logo" />
<figcaption aria-hidden="true">HSItools logo</figcaption>
</figure>

## Installation

You can install the development version of HSItools like so:

``` r
# install.packages("pak")

# pak::pak("mzarowka/HSItools@dev)
```

## Book

A more extensive tutorial is available at:
<https://mzarowka.quarto.pub/hsitools>

## Example

The basic workflow includes running the shiny app to choose analysis
options and visually interact with the core image. After this,
reflectance is calculated and all subsequent operations use reflectance
or its subsets.

``` r
library(HSItools)

# Basic functionality is beeing rewritten now
```

This work is supported by the National Science Centre, Poland, under
research project „Exploring methods of hyperspectral imaging of lake
sediments: proxy development and calibration” (2023/51/D/ST10/00801),
and previously by the Polish National Agency for Academic Exchange
(BPN/BEK/2021/1/00133).

<img src="man/figures\logo-poziom-en.png"
data-fig-alt="National Science Centre, Poland logo" width="319" />

<img src="man/figures/nawa_logo.png"
data-fig-alt="Polish National Agency for Academic Exchange logo"
width="192" />
