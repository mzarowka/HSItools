# Read hsi_metadata from file

Read hsi_metadata from file

## Usage

``` r
hsi_read_metadata(filename)
```

## Arguments

- filename:

  Character. Path to an `hsi_metadata` YAML sidecar.

## Value

An object of class `hsi_metadata`.

## See also

Other HSI Metadata:
[`hsi_create_metadata()`](https://mzarowka.github.io/HSItools/reference/hsi_create_metadata.md),
[`hsi_write_metadata()`](https://mzarowka.github.io/HSItools/reference/hsi_write_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- hsi_create_metadata(name = "test", wavelengths = 400:900, fwhm = rep(2, 501))
hsi_write_metadata(x, filename = "metadata.yaml")

x_metadata <- hsi_read_metadata("metadata.yaml")
} # }
```
