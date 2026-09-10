# Test spatial median smoothing ----
# hsi_smooth_median applies terra::focal with fun = "median" across space.
# Band names are preserved. Window must be odd.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_smooth_median returns a SpatRaster", {
  result <- hsi_smooth_median(x = test_reflectance)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_smooth_median preserves number of bands", {
  result <- hsi_smooth_median(x = test_reflectance)

  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_smooth_median preserves spatial dimensions", {
  result <- hsi_smooth_median(x = test_reflectance)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── Band names ───────────────────────────────────────────────────────────────

test_that("hsi_smooth_median preserves band names", {
  result <- hsi_smooth_median(x = test_reflectance)

  expect_equal(terra::names(result), terra::names(test_reflectance))
})

# ── window argument ──────────────────────────────────────────────────────────

test_that("hsi_smooth_median works with window = 3 (default)", {
  expect_no_error(
    hsi_smooth_median(x = test_reflectance, window = 3)
  )
})

test_that("hsi_smooth_median works with window = 5", {
  expect_no_error(
    hsi_smooth_median(x = test_reflectance, window = 5)
  )
})

test_that("hsi_smooth_median with window = 5 differs from window = 3", {
  result_3 <- hsi_smooth_median(x = test_reflectance, window = 3)
  result_5 <- hsi_smooth_median(x = test_reflectance, window = 5)

  expect_false(
    isTRUE(all.equal(terra::values(result_3), terra::values(result_5)))
  )
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_smooth_median matches MEDIAN_testdata fixture", {
  expected <- terra::rast(
    system.file(
      package = "HSItools",
      "testdata/products/MEDIAN_testdata.tif"
    )
  )

  result <- hsi_smooth_median(x = test_reflectance)

  expect_equal(
    terra::values(result),
    terra::values(expected),
    tolerance = 1e-6
  )
})

test_that("hsi_smooth_median output is bounded by the input min and max", {
  # Median of any neighbourhood must lie within the global value range
  result <- hsi_smooth_median(x = test_reflectance)

  input_min <- min(terra::values(test_reflectance, na.rm = TRUE))
  input_max <- max(terra::values(test_reflectance, na.rm = TRUE))
  result_values <- terra::values(result, na.rm = TRUE)

  expect_true(all(result_values >= input_min))
  expect_true(all(result_values <= input_max))
})

test_that("hsi_smooth_median produces only finite values", {
  result <- hsi_smooth_median(x = test_reflectance)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_smooth_median leaves blanked pixels blank", {
  # Whole-pixel blanks are how saturation masking reaches this function, and
  # terra's default na.policy = "all" computes a value for NA cells too: a
  # blank comes back as the median of its neighbours, a fabricated spectrum
  # where the mask said there is no measurement. Neighbouring pixels must
  # still smooth normally, so both halves are asserted.
  blanked <- terra::cellFromRowCol(test_reflectance, 5, 5)
  neighbour <- terra::cellFromRowCol(test_reflectance, 4, 5)

  values <- terra::values(test_reflectance)
  values[blanked, ] <- NA
  x <- terra::setValues(test_reflectance, values)

  result <- terra::values(hsi_smooth_median(x = x))

  expect_true(all(is.na(result[blanked, ])))
  expect_false(anyNA(result[neighbour, ]))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_smooth_median writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_smooth_median(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_smooth_median errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_smooth_median(
    x = test_reflectance,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_smooth_median(
      x = test_reflectance,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_smooth_median validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_smooth_median,
    list(x = test_reflectance)
  )
})

test_that("hsi_smooth_median errors with non-SpatRaster input", {
  expect_error(
    hsi_smooth_median(x = "not a raster")
  )
})

test_that("hsi_smooth_median errors with even window size", {
  expect_error(
    hsi_smooth_median(x = test_reflectance, window = 4)
  )
})

test_that("hsi_smooth_median errors with a non-finite window size", {
  # Regression: NA, NaN and Inf all made the modulo comparison NA and crashed
  # the `if ()` with a bare simpleError instead of aborting.
  purrr::walk(c(NaN, NA_real_, Inf), \(bad) {
    expect_error(
      hsi_smooth_median(x = test_reflectance, window = bad),
      "odd number",
      class = "hsitools_error"
    )
  })
})

test_that("hsi_smooth_median errors with window = 1", {
  # terra::focal rejects a 1x1 window as not meaningful
  expect_error(
    hsi_smooth_median(x = test_reflectance, window = 1)
  )
})
