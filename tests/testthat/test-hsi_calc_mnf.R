# Test hsi_calc_mnf ----
# Computes the Minimum Noise Fraction transform on a SpatRaster.
# Key contracts: returns a valid mnf/prcomp object with correct slots,
# dimensions consistent with input, eigenvalues in [0, 1] and decreasing.
# Note: standard 9x9x101 fixture has fewer pixels than bands and cannot
# be used directly — all tests use an 8-band subset (81 pixels > 8 bands).

## Setup ----
skip_if_not_installed("spacetime")

# Subset to 8 bands so ncell (81) > nlyr (8)
test_8band <- terra::subset(test_reflectance, 1:8)

# ── Output structure ──────────────────────────────────────────────────────────

test_that("hsi_calc_mnf returns an object of class mnf and prcomp", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_s3_class(result, "mnf")
  expect_s3_class(result, "prcomp")
})

test_that("hsi_calc_mnf result contains required slots", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_true(all(c("values", "rotation", "x") %in% names(result)))
})

# ── Output dimensions ─────────────────────────────────────────────────────────

test_that("hsi_calc_mnf values has length equal to nlyr of input", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_length(result$values, terra::nlyr(test_8band))
})

test_that("hsi_calc_mnf rotation has dimensions nlyr x nlyr", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_equal(
    dim(result$rotation),
    c(terra::nlyr(test_8band), terra::nlyr(test_8band))
  )
})

test_that("hsi_calc_mnf score matrix x has dimensions ncell x nlyr", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_equal(
    dim(result$x),
    c(terra::ncell(test_8band), terra::nlyr(test_8band))
  )
})

# ── Value sanity ──────────────────────────────────────────────────────────────

test_that("hsi_calc_mnf eigenvalues non-negative and finite", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_false(any(is.nan(result$values)))
  expect_false(any(is.infinite(result$values)))
})

test_that("hsi_calc_mnf eigenvalues are in decreasing order", {
  result <- hsi_calc_mnf(x = test_8band)

  expect_true(all(diff(result$values) <= 0))
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_calc_mnf errors with non-SpatRaster input", {
  expect_error(hsi_calc_mnf(x = "not a raster"))
})

test_that("hsi_calc_mnf errors when ncell <= nlyr", {
  # Standard 9x9x101 fixture has 81 cells and 101 bands — fewer pixels than bands
  expect_error(
    hsi_calc_mnf(x = test_reflectance),
    "not enough pixels",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_mnf errors when trim is not length 1", {
  expect_error(hsi_calc_mnf(x = test_8band, trim = c(2L, 3L)))
})

test_that("hsi_calc_mnf errors when valid pixels are fewer than bands", {
  # A masked raster passes the cell count check (81 cells > 8 bands) while
  # holding only 5 pixels that carry data.
  sparse_values <- terra::values(test_8band)
  sparse_values[6:terra::ncell(test_8band), ] <- NA

  sparse <- terra::setValues(test_8band, sparse_values)

  expect_error(
    hsi_calc_mnf(x = sparse),
    "not enough valid pixels",
    class = "hsitools_error"
  )
})

# ── Trim behaviour ────────────────────────────────────────────────────────────

test_that("hsi_calc_mnf with trim reduces output dimensions accordingly", {
  trim <- 2L
  result <- hsi_calc_mnf(x = test_8band, trim = trim)
  expected_bands <- terra::nlyr(test_8band) - (trim * 2)

  expect_length(result$values, expected_bands)
  expect_equal(dim(result$rotation), c(expected_bands, expected_bands))
  expect_equal(dim(result$x), c(terra::ncell(test_8band), expected_bands))
})

test_that("hsi_calc_mnf errors when trim removes all bands", {
  # trim = 4L on 8-band raster leaves 0 bands
  expect_error(
    hsi_calc_mnf(x = test_8band, trim = 4L),
    "no layers left",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_mnf cell/band guard uses post-trim band count", {
  # test_reflectance has 81 cells and 101 bands — fails without trim
  # trimming to 80 bands (trim = 11L) still fails: 81 cells <= 79 bands
  # trimming to 10 bands (trim = 45L) would pass but trim = 45 leaves 11 bands
  # trim = 45L: 101 - 90 = 11 bands, 81 cells > 11 bands — should pass
  expect_no_error(
    hsi_calc_mnf(x = test_reflectance, trim = 45L)
  )
})
