# Test hsi_read_metadata ----
# hsi_read_metadata() reads a YAML sidecar back into an hsi_metadata object.
# It gates on schema_version (must be exactly "1.1.0"), assembles the class
# directly via structure() rather than new_hsi_metadata() (which would
# re-stamp schema_version), then re-runs validate_hsi_metadata() so a
# hand-corrupted sidecar cannot be read back silently.

## Setup ----
valid_metadata <- hsi_create_metadata(
  name = "capture_01",
  nlyr = 3,
  wavelengths = c(450, 550, 650)
)

# ── Round trip ────────────────────────────────────────────────────────────────

test_that("hsi_read_metadata round trip preserves the object exactly", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  x_read <- hsi_read_metadata(temp_file)

  expect_identical(x_read, valid_metadata)
})

test_that("hsi_read_metadata round trip preserves geometry and scan fields", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  metadata_with_geometry <- hsi_create_metadata(
    name = "capture_01",
    nlyr = 3,
    wavelengths = c(450, 550, 650),
    fov_mm = 120,
    camera_position_mm = 45.5,
    stage_position_mm = 10,
    scanning_speed_mm_s = 2.5
  )
  hsi_write_metadata(metadata_with_geometry, filename = temp_file)

  x_read <- hsi_read_metadata(temp_file)

  expect_identical(x_read, metadata_with_geometry)
})

test_that("hsi_read_metadata reads a sidecar written before the geometry/scan fields existed", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  new_fields <- c(
    "fov_mm",
    "camera_position_mm",
    "stage_position_mm",
    "scanning_speed_mm_s"
  )
  lines <- lines[
    !grepl(paste0("^(", paste(new_fields, collapse = "|"), "):"), lines)
  ]
  writeLines(lines, temp_file)

  x_read <- hsi_read_metadata(temp_file)

  expect_true(all(purrr::map_lgl(x_read[new_fields], is.null)))
})

test_that("hsi_read_metadata round trip preserves acquisition and QC fields", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  metadata_with_acquisition <- hsi_create_metadata(
    name = "capture_01",
    nlyr = 3,
    wavelengths = c(450, 550, 650),
    operator = "J. Doe",
    campaign_prefix = "LK24",
    dataset_name = "core_03_scan_01",
    lens = "18.5 mm",
    calibration_pack = "specim_2026_06",
    aspect_ratio = 1.02,
    dropped_frames = 3,
    gcp_count = 6
  )
  hsi_write_metadata(metadata_with_acquisition, filename = temp_file)

  x_read <- hsi_read_metadata(temp_file)

  expect_identical(x_read, metadata_with_acquisition)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_read_metadata errors on a nonexistent file", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  expect_error(
    hsi_read_metadata(temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_read_metadata errors when schema_version is missing", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- lines[!grepl("^schema_version:", lines)]
  writeLines(lines, temp_file)

  expect_error(
    hsi_read_metadata(temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_read_metadata errors when schema_version does not match", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- sub("^schema_version:.*$", "schema_version: 0.9.0", lines)
  writeLines(lines, temp_file)

  expect_error(
    hsi_read_metadata(temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_read_metadata rejects a sidecar written under the previous schema_version", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- sub("^schema_version:.*$", "schema_version: 1.0.0", lines)
  writeLines(lines, temp_file)

  expect_error(
    hsi_read_metadata(temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_read_metadata errors when a hand-edited field fails validation", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- sub("^nrow:.*$", "nrow: -5", lines)
  writeLines(lines, temp_file)

  expect_error(
    hsi_read_metadata(temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_read_metadata treats a missing optional key as NULL", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- lines[!grepl("^sensor_type:", lines)]
  writeLines(lines, temp_file)

  x_read <- hsi_read_metadata(temp_file)

  expect_null(x_read$sensor_type)
})

test_that("hsi_read_metadata error messages match snapshot", {
  nonexistent_file <- withr::local_tempfile(fileext = ".yaml")

  expect_snapshot(
    hsi_read_metadata(nonexistent_file),
    error = TRUE,
    transform = \(lines) {
      gsub(nonexistent_file, "<temp_file>", lines, fixed = TRUE)
    }
  )

  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  lines <- readLines(temp_file)
  lines <- sub("^schema_version:.*$", "schema_version: 0.9.0", lines)
  writeLines(lines, temp_file)

  expect_snapshot(
    hsi_read_metadata(temp_file),
    error = TRUE,
    transform = \(lines) gsub(temp_file, "<temp_file>", lines, fixed = TRUE)
  )
})
