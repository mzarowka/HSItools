# Test coefficient of variation of reflectance ----
# Rcv = sd(reflectance) / mean(reflectance) per pixel across all bands.

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_rcv returns a SpatRaster", {
  result <- hsi_calc_rcv(x = test_reflectance)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_rcv returns a single-band raster", {
  result <- hsi_calc_rcv(x = test_reflectance)

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rcv preserves spatial dimensions", {
  result <- hsi_calc_rcv(x = test_reflectance)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_rcv sets layer name when index_name provided", {
  result <- hsi_calc_rcv(
    x = test_reflectance,
    index_name = "rcv"
  )

  expect_equal(terra::names(result), "rcv")
})

test_that("hsi_calc_rcv has default terra name when index_name is NULL", {
  result <- hsi_calc_rcv(x = test_reflectance)

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rcv returns zero for a spectrally constant raster", {
  # All bands equal → sd = 0 → CV = 0
  constant_raster <- terra::setValues(
    terra::rast(test_reflectance),
    rep(0.5, terra::ncell(test_reflectance) * terra::nlyr(test_reflectance))
  )

  result <- hsi_calc_rcv(x = constant_raster)
  values <- terra::values(result, na.rm = TRUE)

  expect_equal(
    unname(values),
    matrix(0, nrow = nrow(values), ncol = 1),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_rcv is consistent with hsi_calc_rsd / hsi_calc_rmean", {
  # CV = sd / mean — verify against sibling functions pixel-by-pixel
  result_rcv <- hsi_calc_rcv(x = test_reflectance)
  result_rsd <- hsi_calc_rsd(x = test_reflectance)
  result_mean <- hsi_calc_rmean(x = test_reflectance)

  expected <- unname(terra::values(result_rsd)) /
    unname(terra::values(result_mean))

  expect_equal(
    unname(terra::values(result_rcv)),
    expected,
    tolerance = 1e-6
  )
})

test_that("hsi_calc_rcv returns non-negative values", {
  # sd >= 0 and mean of positive reflectance > 0, so CV >= 0
  result <- hsi_calc_rcv(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_true(all(values >= 0))
})

test_that("hsi_calc_rcv produces only finite values", {
  result <- hsi_calc_rcv(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rcv writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_rcv(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_rcv errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_rcv(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_rcv(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_rcv validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_rcv,
    list(x = test_reflectance)
  )
})

test_that("hsi_calc_rcv errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_rcv(x = "not a raster")
  )
})

test_that("hsi_calc_rcv errors when an unused argument is passed without filename", {
  expect_error(
    hsi_calc_rcv(
      x = test_reflectance,
      bogus_arg = 1
    ),
    "not used",
    class = "hsitools_error"
  )
})
