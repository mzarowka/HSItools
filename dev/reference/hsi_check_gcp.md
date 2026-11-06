# Assess affine transformation quality from matched control points

Fit a first-order affine transformation from matched control points
using least squares and return per-point residuals. Use this to identify
poorly digitized points before the expensive warp step.

## Usage

``` r
hsi_check_gcp(x, verbose = FALSE)
```

## Arguments

- x:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
  matched control points from
  [`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md).
  Each row must give the corresponding source and target location in
  `source_x`, `source_y`, `target_x`, and `target_y`.

- verbose:

  Logical. Print control point count and RMSE to console. Default
  `FALSE`.

## Value

A named list containing:

- residuals:

  A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
  all input columns plus `residual_x`, `residual_y`, and
  `residual_total` in target pixels.

- rmse:

  Numeric. Root mean square error in target pixels.

- n_gcps:

  Integer. Number of control points used.

## Details

A control point (GCP) is the same recognizable feature marked in both
images.

The affine model (6 parameters) handles translation, rotation,
independent X/Y scaling, and shear. With N control points, residual
assessment has N - 3 degrees of freedom. Residuals are in target pixel
units. An RMSE above 5 pixels triggers a warning.

## See also

[`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md)
for preparing input,
[`hsi_coregister()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_coregister.md)
for applying the warp.

Other HSI Co-registration:
[`hsi_coregister()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_coregister.md),
[`hsi_match_gcp()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_match_gcp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
matched <- hsi_match_gcp(swir_gcps, vnir_gcps)
x_check <- hsi_check_gcp(matched)

# Inspect the least consistent control points
x_check$residuals |>
  dplyr::arrange(dplyr::desc(residual_total))

# Remove outliers and re-check
cleaned <- matched |> dplyr::filter(!gcp_id %in% c(5, 12))
hsi_check_gcp(cleaned)
} # }
```
