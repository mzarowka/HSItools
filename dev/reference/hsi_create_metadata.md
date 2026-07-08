# Create structured hyperspectral metadata

Create structured hyperspectral metadata

## Usage

``` r
hsi_create_metadata(
  name,
  sensor_type = NULL,
  manufacturer = NULL,
  session_id = NULL,
  nrow = NULL,
  ncol = NULL,
  nlyr = NULL,
  xres = NULL,
  yres = NULL,
  spectral_resolution_nm = NULL,
  frame_rate_hz = NULL,
  et_target_ms = NULL,
  et_white_ms = NULL,
  target_start_mm = NULL,
  target_stop_mm = NULL,
  spectral_binning = NULL,
  spatial_binning = NULL,
  wavelengths = NULL,
  fwhm = NULL
)
```

## Arguments

- name:

  Character. Capture name. A single non-empty string.

- sensor_type:

  Character. Sensor type. Default `NULL`.

- manufacturer:

  Character. Sensor manufacturer. Default `NULL`.

- session_id:

  Character. Session identifier grouping scans that share a white
  reference. Default `NULL`.

- nrow:

  Positive integer. Number of raster rows. Default `NULL`.

- ncol:

  Positive integer. Number of raster columns. Default `NULL`.

- nlyr:

  Positive integer. Number of raster layers. Default `NULL`.

- xres:

  Positive number. Pixel resolution in the x direction. Default `NULL`.

- yres:

  Positive number. Pixel resolution in the y direction. Default `NULL`.

- spectral_resolution_nm:

  Positive number. Spectral resolution in nm. Default `NULL`.

- frame_rate_hz:

  Positive number. Frame rate in Hz. Default `NULL`.

- et_target_ms:

  Positive number. Target integration time in ms. Default `NULL`.

- et_white_ms:

  Positive number. White reference integration time in ms. Default
  `NULL`.

- target_start_mm:

  Positive number. Motor position at scan start in mm. Default `NULL`.

- target_stop_mm:

  Positive number. Motor position at scan end in mm. Default `NULL`.

- spectral_binning:

  Positive integer. Spectral binning factor. Default `NULL`.

- spatial_binning:

  Positive integer. Spatial binning factor. Default `NULL`.

- wavelengths:

  Positive numeric vector. Band centre wavelengths in nm, one value per
  layer. Default `NULL`.

- fwhm:

  Positive numeric vector. Band full width at half maximum in nm, one
  value per layer. Default `NULL`.

## Value

An object of class `hsi_metadata`: a validated list of capture metadata
fields with an internally stamped `schema_version`.

## See also

Other HSI Metadata:
[`hsi_read_metadata()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_read_metadata.md),
[`hsi_write_metadata()`](https://mzarowka.github.io/HSItools/dev/reference/hsi_write_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal: name only
x_metadata <- hsi_create_metadata(name = "capture_01")

# With sensor identity, dimensions, and spectral axis
x_metadata <- hsi_create_metadata(
  name = "capture_01",
  sensor_type = "VNIR",
  manufacturer = "Specim",
  nlyr = 224,
  wavelengths = seq(400, 1000, length.out = 224)
)
} # }
```
