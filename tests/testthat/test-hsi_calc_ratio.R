# Test band ratio calculation ----
# ratio = band1 / band2

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_ratio returns a SpatRaster", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_ratio returns a single-band raster", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_ratio preserves spatial dimensions", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_ratio sets layer name when index_name provided", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690),
    index_name = "ratio570690"
  )

  expect_equal(terra::names(result), "ratio570690")
})

test_that("hsi_calc_ratio has default terra name when index_name is NULL", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_ratio with reversed bands produces reciprocal values", {
  # ratio(a, b) * ratio(b, a) == 1 for all non-zero reflectances
  result_fwd <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  result_rev <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(690, 570)
  )

  product <- unname(terra::values(result_fwd)) *
    unname(terra::values(result_rev))

  expect_equal(
    product,
    matrix(1, nrow = nrow(product), ncol = 1),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_ratio produces positive values for positive reflectance", {
  # a / b > 0 when both bands are positive
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_true(all(values > 0))
})

test_that("hsi_calc_ratio produces only finite values", {
  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690)
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_calc_ratio errors when both bands resolve to the same position", {
  # wavelength_position deduplicates identical inputs — documents known behaviour
  expect_error(
    hsi_calc_ratio(
      x = test_reflectance,
      bands = c(660, 660)
    )
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_ratio writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_ratio errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_ratio(
    x = test_reflectance,
    bands = c(570, 690),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_ratio(
      x = test_reflectance,
      bands = c(570, 690),
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_ratio validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_ratio,
    list(x = test_reflectance, bands = c(570, 690))
  )
})

test_that("hsi_calc_ratio errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_ratio(
      x = "not a raster",
      bands = c(570, 690)
    )
  )
})

test_that("hsi_calc_ratio errors when bands is not length 2", {
  expect_error(
    hsi_calc_ratio(
      x = test_reflectance,
      bands = c(570, 620, 690)
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_ratio errors when bands is length 1", {
  expect_error(
    hsi_calc_ratio(
      x = test_reflectance,
      bands = 660
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_ratio errors when an unused argument is passed without filename", {
  expect_error(
    hsi_calc_ratio(
      x = test_reflectance,
      bands = c(570, 690),
      bogus_arg = 1
    ),
    "not used",
    class = "hsitools_error"
  )
})
