# Test hsi_write_metadata ----
# hsi_write_metadata() writes an hsi_metadata object to a YAML sidecar and
# returns the input invisibly (Shape B). Validation is re-run before writing,
# so a corrupted hsi_metadata object cannot reach disk.

## Setup ----
valid_metadata <- hsi_create_metadata(
  name = "capture_01",
  nlyr = 3,
  wavelengths = c(450, 550, 650)
)

# ── File writing ──────────────────────────────────────────────────────────────

test_that("hsi_write_metadata writes file when filename provided", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  hsi_write_metadata(valid_metadata, filename = temp_file)

  expect_true(file.exists(temp_file))
})

test_that("hsi_write_metadata errors when file exists and overwrite = FALSE", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  expect_error(
    hsi_write_metadata(valid_metadata, filename = temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_write_metadata succeeds with overwrite = TRUE", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  expect_no_error(
    hsi_write_metadata(valid_metadata, filename = temp_file, overwrite = TRUE)
  )
})

# ── Return value ──────────────────────────────────────────────────────────────

test_that("hsi_write_metadata returns input invisibly", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  expect_invisible(hsi_write_metadata(valid_metadata, filename = temp_file))

  result <- hsi_write_metadata(
    valid_metadata,
    filename = temp_file,
    overwrite = TRUE
  )
  expect_identical(result, valid_metadata)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_write_metadata errors on non-hsi_metadata input", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  expect_error(
    hsi_write_metadata(list(name = "capture_01"), filename = temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_write_metadata validates filename and overwrite", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  expect_write_tail_validated(
    hsi_write_metadata,
    list(x = valid_metadata, filename = temp_file)
  )
})

test_that("hsi_write_metadata refuses to write an invalidated object", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")

  broken_metadata <- valid_metadata
  broken_metadata$nrow <- c(9, 9)

  expect_error(
    hsi_write_metadata(broken_metadata, filename = temp_file),
    class = "hsitools_error"
  )
})

test_that("hsi_write_metadata error messages match snapshot", {
  temp_file <- withr::local_tempfile(fileext = ".yaml")
  hsi_write_metadata(valid_metadata, filename = temp_file)

  expect_snapshot(
    hsi_write_metadata(list(name = "capture_01"), filename = temp_file),
    error = TRUE
  )

  expect_snapshot(
    hsi_write_metadata(valid_metadata, filename = temp_file),
    error = TRUE,
    transform = \(lines) gsub(temp_file, "<temp_file>", lines, fixed = TRUE)
  )
})
