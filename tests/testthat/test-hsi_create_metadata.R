# Test hsi_create_metadata ----
# hsi_create_metadata() assembles and validates an hsi_metadata list. `name`
# is the only required field; every other field defaults to NULL and is
# validated only when supplied. schema_version is stamped internally as
# "1.0.0". validate_hsi_metadata() and new_hsi_metadata() are internal and
# are exercised only through this exported front door.

## Setup ----
expected_fields <- c(
  "schema_version",
  "name",
  "sensor_type",
  "manufacturer",
  "session_id",
  "nrow",
  "ncol",
  "nlyr",
  "xres",
  "yres",
  "spectral_resolution_nm",
  "frame_rate_hz",
  "et_target_ms",
  "et_white_ms",
  "target_start_mm",
  "target_stop_mm",
  "spectral_binning",
  "spatial_binning",
  "wavelengths",
  "fwhm"
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_create_metadata returns an object of class hsi_metadata", {
  x_metadata <- hsi_create_metadata(name = "capture_01")

  expect_s3_class(x_metadata, "hsi_metadata")
})

# ── Output structure ─────────────────────────────────────────────────────────

test_that("hsi_create_metadata minimal call sets every optional field to NULL", {
  x_metadata <- hsi_create_metadata(name = "capture_01")

  expect_named(x_metadata, expected_fields)

  optional_fields <- setdiff(expected_fields, c("schema_version", "name"))
  expect_true(all(purrr::map_lgl(x_metadata[optional_fields], is.null)))
})

test_that("hsi_create_metadata stamps schema_version as 1.0.0", {
  x_metadata <- hsi_create_metadata(name = "capture_01")

  expect_identical(x_metadata$schema_version, "1.0.0")
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_create_metadata requires a non-empty name", {
  expect_error(hsi_create_metadata())

  expect_error(
    hsi_create_metadata(name = ""),
    class = "rlang_error"
  )
})

test_that("hsi_create_metadata errors when a scalar field is non-scalar", {
  expect_error(
    hsi_create_metadata(name = "capture_01", nrow = c(9, 9)),
    class = "hsitools_error"
  )
})

test_that("hsi_create_metadata errors when a numeric field contains non-positive values", {
  expect_error(
    hsi_create_metadata(
      name = "capture_01",
      nlyr = 3,
      wavelengths = c(450, -500, 550)
    ),
    class = "hsitools_error"
  )
})

test_that("hsi_create_metadata errors when wavelengths or fwhm length mismatches nlyr", {
  expect_error(
    hsi_create_metadata(
      name = "capture_01",
      nlyr = 3,
      wavelengths = c(450, 550)
    ),
    class = "hsitools_error"
  )

  expect_error(
    hsi_create_metadata(
      name = "capture_01",
      nlyr = 3,
      fwhm = c(2, 2)
    ),
    class = "hsitools_error"
  )
})

test_that("hsi_create_metadata error messages match snapshot", {
  expect_snapshot(
    hsi_create_metadata(
      name = "capture_01",
      nlyr = 3,
      wavelengths = c(450, 550)
    ),
    error = TRUE
  )

  expect_snapshot(
    hsi_create_metadata(name = "capture_01", nrow = c(9, 9)),
    error = TRUE
  )
})
