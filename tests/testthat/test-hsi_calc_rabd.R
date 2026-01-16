# Test RABD calculation ----

## Setup ----
# Load test data once for all tests
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

## Test output type ----
test_that("hsi_calc_rabd returns a SpatRaster", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 660:680,
    index_type = "max"
  )

  expect_s4_class(result, "SpatRaster")
})

## Test output dimensions ----
test_that("hsi_calc_rabd returns single-band raster with correct dimensions", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  # Should be single band

  expect_equal(terra::nlyr(result), 1)

  # Should preserve spatial dimensions
  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

## Test index_type variants ----
test_that("hsi_calc_rabd works with index_type = 'max'", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 660:680,
    index_type = "max"
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rabd works with index_type = 'strict'", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rabd works with index_type = 'mid'", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 660:680,
    index_type = "mid"
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 1)
})

## Test index_name argument ----
test_that("hsi_calc_rabd sets layer name when index_name provided", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict",
    index_name = "rabd670"
  )

  expect_equal(names(result), "rabd670")
})

test_that("hsi_calc_rabd has NULL name when index_name not provided", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  # Name should be empty/NULL (terra default behavior)
  expect_true(
    names(result) == "" || is.null(names(result)) || names(result) == "lyr.1"
  )
})

## Test input validation ----
test_that("hsi_calc_rabd errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_rabd(
      x = "not a raster",
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "strict"
    ),
    "must be a.*SpatRaster"
  )
})

test_that("hsi_calc_rabd errors with wrong continuum_edges length", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 660, 730),
      absorption_band = 670,
      index_type = "strict"
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_rabd errors with invalid index_type", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "invalid"
    ),
    "must be one of"
  )
})

test_that("hsi_calc_rabd errors with multiple absorption_band for strict type", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 660:680,
      index_type = "strict"
    ),
    "must be length 1"
  )
})

## Test file writing ----
test_that("hsi_calc_rabd writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict",
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

## Test RABD values are reasonable ----
test_that("hsi_calc_rabd produces finite positive values", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  values <- terra::values(result, na.rm = TRUE)

  # RABD should be positive (continuum / trough, both positive reflectances)
  # Allow for some edge cases but most should be positive
  expect_true(sum(values > 0, na.rm = TRUE) > length(values) * 0.5)

  # Should not have Inf values (division by zero handled)
  expect_false(any(is.infinite(values)))
})
