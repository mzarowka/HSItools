# Test hsi_write_scaled ----
# Writes a float SpatRaster as uint16 GeoTIFF with embedded scale metadata.
# GDAL scale/offset means terra::rast() reads back as float transparently.
# Always writes to disk — filename is required.
# Precision: 1 / scale_factor (default 1/10000 = 0.0001).

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_write_scaled returns a SpatRaster", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

# ── Always writes to disk ────────────────────────────────────────────────────

test_that("hsi_write_scaled always creates a file on disk", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))

  unlink(temp_file)
})

# ── Output dimensions preserved ──────────────────────────────────────────────

test_that("hsi_write_scaled preserves spatial dimensions on read-back", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  result <- terra::rast(temp_file)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))

  unlink(temp_file)
})

# ── Round-trip fidelity ──────────────────────────────────────────────────────

test_that("hsi_write_scaled round-trip is within 1/scale_factor absolute precision", {
  # Precision guarantee is absolute: each stored integer = 1 / scale_factor
  # Must use max absolute error, not relative tolerance
  temp_file <- tempfile(fileext = ".tif")
  scale_factor <- 10000L

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    scale_factor = scale_factor,
    overwrite = TRUE
  )

  result <- terra::rast(temp_file)

  max_abs_error <- max(
    abs(
      terra::values(result, na.rm = FALSE) -
        terra::values(test_reflectance, na.rm = FALSE)
    ),
    na.rm = TRUE
  )

  expect_lte(max_abs_error, 1 / scale_factor)

  unlink(temp_file)
})

test_that("hsi_write_scaled with larger scale_factor gives higher precision", {
  temp_file_lo <- tempfile(fileext = ".tif")
  temp_file_hi <- tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file_lo,
    scale_factor = 100L,
    overwrite = TRUE
  )

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file_hi,
    scale_factor = 10000L,
    overwrite = TRUE
  )

  result_lo <- terra::rast(temp_file_lo)
  result_hi <- terra::rast(temp_file_hi)

  diff_lo <- mean(abs(
    terra::values(result_lo, na.rm = TRUE) -
      terra::values(test_reflectance, na.rm = TRUE)
  ))

  diff_hi <- mean(abs(
    terra::values(result_hi, na.rm = TRUE) -
      terra::values(test_reflectance, na.rm = TRUE)
  ))

  expect_lt(diff_hi, diff_lo)

  unlink(temp_file_lo)
  unlink(temp_file_hi)
})

# ── Overwrite behaviour ──────────────────────────────────────────────────────

test_that("hsi_write_scaled errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_write_scaled(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── uint16 overflow guard ────────────────────────────────────────────────────

test_that("hsi_write_scaled errors when values exceed uint16 capacity", {
  temp_file <- tempfile(fileext = ".tif")

  # At scale_factor = 10000, max storable value = 65535/10000 = 6.5535
  overflow_raster <- test_reflectance
  terra::values(overflow_raster)[1] <- 10

  expect_error(
    hsi_write_scaled(
      x = overflow_raster,
      filename = temp_file,
      scale_factor = 10000L,
      overwrite = TRUE
    ),
    "uint16"
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_write_scaled errors with non-SpatRaster input", {
  temp_file <- tempfile(fileext = ".tif")

  expect_error(
    hsi_write_scaled(x = "not a raster", filename = temp_file)
  )

  unlink(temp_file)
})

test_that("hsi_write_scaled errors with non-positive scale_factor", {
  temp_file <- tempfile(fileext = ".tif")

  expect_error(
    hsi_write_scaled(
      x = test_reflectance,
      filename = temp_file,
      scale_factor = -1L
    )
  )

  unlink(temp_file)
})

test_that("hsi_write_scaled errors with scale_factor of length > 1", {
  temp_file <- tempfile(fileext = ".tif")

  expect_error(
    hsi_write_scaled(
      x = test_reflectance,
      filename = temp_file,
      scale_factor = c(10000L, 1000L)
    )
  )

  unlink(temp_file)
})
