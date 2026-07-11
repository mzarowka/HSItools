# Test Savitzky-Golay filter ----

## Setup ----
# Load test data once for all tests in this file
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Value snapshot ───────────────────────────────────────────────────────────

test_that("Savitzky-Golay smoothed reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_smooth_savgol(x = test_reflectance)),
    terra::values(terra::rast(
      system.file(
        package = "HSItools",
        "testdata/products/SAVGOL_testdata.tif"
      )
    )),
    tolerance = 1e-6
  )
})

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_smooth_savgol returns a SpatRaster", {
  expect_s4_class(
    hsi_smooth_savgol(x = test_reflectance),
    "SpatRaster"
  )
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_smooth_savgol preserves number of layers", {
  result <- hsi_smooth_savgol(x = test_reflectance)
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_smooth_savgol preserves spatial dimensions", {
  result <- hsi_smooth_savgol(x = test_reflectance)
  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

test_that("hsi_smooth_savgol preserves band names", {
  result <- hsi_smooth_savgol(x = test_reflectance)
  expect_equal(terra::names(result), terra::names(test_reflectance))
})

# ── Parameter variants ───────────────────────────────────────────────────────

test_that("hsi_smooth_savgol works with custom polynomial order p = 2", {
  result <- hsi_smooth_savgol(x = test_reflectance, p = 2, n = 11)
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_smooth_savgol works with custom window size n = 9", {
  result <- hsi_smooth_savgol(x = test_reflectance, p = 3, n = 9)
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_smooth_savgol with m = 1 returns first derivative (differs from m = 0)", {
  result_smooth <- hsi_smooth_savgol(x = test_reflectance, m = 0)
  result_deriv1 <- hsi_smooth_savgol(x = test_reflectance, m = 1)

  expect_s4_class(result_deriv1, "SpatRaster")

  # First derivative must differ from smoothed signal
  expect_false(
    isTRUE(all.equal(
      terra::values(result_smooth),
      terra::values(result_deriv1)
    ))
  )
})

test_that("hsi_smooth_savgol with m = 2 returns second derivative (differs from m = 1)", {
  result_deriv1 <- hsi_smooth_savgol(x = test_reflectance, m = 1)
  result_deriv2 <- hsi_smooth_savgol(x = test_reflectance, m = 2)

  expect_s4_class(result_deriv2, "SpatRaster")

  # Second derivative must differ from first derivative
  expect_false(
    isTRUE(all.equal(
      terra::values(result_deriv1),
      terra::values(result_deriv2)
    ))
  )
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_smooth_savgol produces only finite values", {
  result <- hsi_smooth_savgol(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_smooth_savgol (m = 0) output stays in plausible reflectance range", {
  result <- hsi_smooth_savgol(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  # Savitzky-Golay smoothing should not wildly extrapolate a 0-1 reflectance signal
  expect_true(all(values > -0.1))
  expect_true(all(values < 1.1))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_smooth_savgol writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_smooth_savgol(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_smooth_savgol errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_smooth_savgol(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_smooth_savgol(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_smooth_savgol errors with non-SpatRaster input", {
  expect_error(
    hsi_smooth_savgol(x = "not a raster"),
    class = "hsitools_error"
  )
})

test_that("hsi_smooth_savgol errors when p >= n", {
  expect_error(
    hsi_smooth_savgol(x = test_reflectance, p = 11, n = 11),
    "must be less than",
    class = "hsitools_error"
  )
})

test_that("hsi_smooth_savgol errors when n is even", {
  expect_error(
    hsi_smooth_savgol(x = test_reflectance, p = 3, n = 10),
    class = "hsitools_error"
  )
})

test_that("hsi_smooth_savgol errors when filter length exceeds number of bands", {
  # Use a filter window larger than the raster has bands
  expect_error(
    hsi_smooth_savgol(
      x = test_reflectance,
      p = 3,
      n = terra::nlyr(test_reflectance) + 2
    ),
    class = "hsitools_error"
  )
})
