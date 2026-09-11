# Test median reflectance calculation ----
# Rmedian = median of reflectance across all bands per pixel.

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_rmedian returns a SpatRaster", {
  result <- hsi_calc_rmedian(x = test_reflectance)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_rmedian returns a single-band raster", {
  result <- hsi_calc_rmedian(x = test_reflectance)

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rmedian preserves spatial dimensions", {
  result <- hsi_calc_rmedian(x = test_reflectance)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_rmedian sets layer name when index_name provided", {
  result <- hsi_calc_rmedian(
    x = test_reflectance,
    index_name = "rmedian"
  )

  expect_equal(terra::names(result), "rmedian")
})

test_that("hsi_calc_rmedian has default terra name when index_name is NULL", {
  result <- hsi_calc_rmedian(x = test_reflectance)

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rmedian returns the constant value for a uniform raster", {
  # median of identical values == that value exactly
  constant_raster <- terra::setValues(
    terra::rast(test_reflectance),
    rep(0.4, terra::ncell(test_reflectance) * terra::nlyr(test_reflectance))
  )

  result <- hsi_calc_rmedian(x = constant_raster)
  values <- terra::values(result, na.rm = TRUE)

  expect_equal(
    unname(values),
    matrix(0.4, nrow = nrow(values), ncol = 1),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_rmedian output is bounded by the input min and max", {
  # The median of any set of values must lie within [min, max]
  result <- hsi_calc_rmedian(x = test_reflectance)

  input_min <- min(terra::values(test_reflectance, na.rm = TRUE))
  input_max <- max(terra::values(test_reflectance, na.rm = TRUE))
  result_values <- terra::values(result, na.rm = TRUE)

  expect_true(all(result_values >= input_min))
  expect_true(all(result_values <= input_max))
})

test_that("hsi_calc_rmedian produces only finite values", {
  result <- hsi_calc_rmedian(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rmedian writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_rmedian(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_rmedian errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_rmedian(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_rmedian(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_rmedian validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_rmedian,
    list(x = test_reflectance)
  )
})

test_that("hsi_calc_rmedian errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_rmedian(x = "not a raster")
  )
})
