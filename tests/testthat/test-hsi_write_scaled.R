# Test hsi_write_scaled ----
# Writes a float SpatRaster as a scaled integer or float GeoTIFF with embedded
# scale metadata. GDAL scale/offset means terra::rast() reads back as float
# transparently.
#
# Key contracts:
#   - Always writes to disk — filename is required, the return is invisible.
#   - Precision for integer datatypes is 1 / scale_factor (default 0.0001).
#   - Values outside the storable range abort in BOTH directions. The floor
#     matters as much as the ceiling: unsigned types clamp negatives silently.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_write_scaled returns a SpatRaster", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_write_scaled preserves spatial dimensions on read-back", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  result <- terra::rast(temp_file)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_write_scaled round-trip is within 1/scale_factor absolute precision", {
  # Precision guarantee is absolute: each stored integer = 1 / scale_factor
  # Must use max absolute error, not relative tolerance
  temp_file <- withr::local_tempfile(fileext = ".tif")
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
})

test_that("hsi_write_scaled with larger scale_factor gives higher precision", {
  temp_file_lo <- withr::local_tempfile(fileext = ".tif")
  temp_file_hi <- withr::local_tempfile(fileext = ".tif")

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

  diff_lo <- mean(abs(
    terra::values(terra::rast(temp_file_lo), na.rm = TRUE) -
      terra::values(test_reflectance, na.rm = TRUE)
  ))

  diff_hi <- mean(abs(
    terra::values(terra::rast(temp_file_hi), na.rm = TRUE) -
      terra::values(test_reflectance, na.rm = TRUE)
  ))

  expect_lt(diff_hi, diff_lo)
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_write_scaled always creates a file on disk", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_write_scaled(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
})

test_that("hsi_write_scaled errors when file exists and overwrite = FALSE", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

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
})

# ── Storable range guards ────────────────────────────────────────────────────

test_that("hsi_write_scaled errors when values exceed uint16 capacity", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

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
    "exceeds.*capacity",
    class = "hsitools_error"
  )
})

test_that("hsi_write_scaled errors when values fall below uint16 capacity", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  # INT2U is unsigned, so a negative value is clamped to zero on write with no
  # record of how many cells were affected.
  underflow_raster <- test_reflectance
  terra::values(underflow_raster)[1] <- -0.5

  expect_error(
    hsi_write_scaled(
      x = underflow_raster,
      filename = temp_file,
      scale_factor = 10000L,
      overwrite = TRUE
    ),
    "below.*capacity",
    class = "hsitools_error"
  )
})

test_that("hsi_write_scaled keeps negative values for a signed datatype", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  signed_raster <- test_reflectance
  terra::values(signed_raster)[1] <- -0.5

  hsi_write_scaled(
    x = signed_raster,
    filename = temp_file,
    datatype = "INT2S",
    scale_factor = 10000L,
    overwrite = TRUE
  )

  written_min <- terra::global(
    terra::rast(temp_file),
    fun = "min",
    na.rm = TRUE
  ) |>
    dplyr::pull("min") |>
    min(na.rm = TRUE)

  expect_lt(written_min, 0)
})

test_that("hsi_write_scaled skips range validation for float datatypes", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  # Both extremes would abort under INT2U, but floats have no meaningful
  # ceiling or floor.
  wide_raster <- test_reflectance
  terra::values(wide_raster)[1] <- -50
  terra::values(wide_raster)[2] <- 5000

  expect_no_error(
    hsi_write_scaled(
      x = wide_raster,
      filename = temp_file,
      datatype = "FLT4S",
      overwrite = TRUE
    )
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_write_scaled errors with non-SpatRaster input", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  expect_error(
    hsi_write_scaled(x = "not a raster", filename = temp_file)
  )
})

test_that("hsi_write_scaled errors with an invalid scale_factor", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  expect_error(
    hsi_write_scaled(
      x = test_reflectance,
      filename = temp_file,
      scale_factor = -1L
    )
  )

  expect_error(
    hsi_write_scaled(
      x = test_reflectance,
      filename = temp_file,
      scale_factor = c(10000L, 1000L)
    )
  )
})

test_that("hsi_write_scaled validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_write_scaled,
    list(
      x = test_reflectance,
      filename = withr::local_tempfile(fileext = ".tif")
    )
  )
})
