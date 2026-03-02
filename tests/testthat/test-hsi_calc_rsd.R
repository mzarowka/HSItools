# Test standard deviation of reflectance calculation ----
# Rsd = standard deviation of reflectance across all bands per pixel.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_rsd returns a SpatRaster", {
  result <- hsi_calc_rsd(x = test_reflectance)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_rsd returns a single-band raster", {
  result <- hsi_calc_rsd(x = test_reflectance)

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rsd preserves spatial dimensions", {
  result <- hsi_calc_rsd(x = test_reflectance)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_rsd sets layer name when index_name provided", {
  result <- hsi_calc_rsd(
    x = test_reflectance,
    index_name = "rsd"
  )

  expect_equal(terra::names(result), "rsd")
})

test_that("hsi_calc_rsd has default terra name when index_name is NULL", {
  result <- hsi_calc_rsd(x = test_reflectance)

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rsd returns zero for a spectrally constant raster", {
  # sd of identical values == 0 exactly
  constant_raster <- terra::setValues(
    terra::rast(test_reflectance),
    rep(0.4, terra::ncell(test_reflectance) * terra::nlyr(test_reflectance))
  )

  result <- hsi_calc_rsd(x = constant_raster)
  values <- terra::values(result, na.rm = TRUE)

  expect_equal(
    unname(values),
    matrix(0, nrow = nrow(values), ncol = 1),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_rsd returns non-negative values", {
  # Standard deviation is always >= 0 by definition
  result <- hsi_calc_rsd(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_true(all(values >= 0))
})

test_that("hsi_calc_rsd produces only finite values", {
  result <- hsi_calc_rsd(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rsd writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_rsd(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_rsd errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_rsd(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_rsd(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_rsd errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_rsd(x = "not a raster")
  )
})
