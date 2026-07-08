# Write hsi_metadata to file

Write hsi_metadata to file

## Usage

``` r
hsi_write_metadata(x, filename, overwrite = FALSE)
```

## Arguments

- x:

  An `hsi_metadata` object to write.

- filename:

  Character. Output filename.

- overwrite:

  Logical. Overwrite existing file. Default `FALSE`.

## Value

An `hsi_metadata` object, returned invisibly.

## See also

Other HSI Metadata:
[`hsi_create_metadata()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_create_metadata.md),
[`hsi_read_metadata()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_read_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- hsi_create_metadata(name = "test", wavelengths = 400:900, fwhm = rep(2, 501))
hsi_write_metadata(x, filename = "metadata.yaml")
x_written <- hsi_write_metadata(x, filename = "metadata.yaml", overwrite = TRUE)
} # }
```
