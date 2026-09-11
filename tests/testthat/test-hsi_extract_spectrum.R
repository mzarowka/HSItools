# Test hsi_extract_spectrum ----
# Aggregates all pixels in a hyperspectral SpatRaster to a single spectrum,
# returning a two-column tibble with one row per band.
# Key contracts:
#   - Output is always a tibble with exactly `wavelength` and `value` columns
#   - One row per band (nlyr rows)
#   - `wavelength` column is numeric and matches input band names
#   - `fun` argument affects output values
#   - Errors when band names are not parseable as numeric wavelengths

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_extract_spectrum returns a tibble", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_s3_class(result, "tbl_df")
})

# ── Output structure ─────────────────────────────────────────────────────────

test_that("hsi_extract_spectrum output has exactly two columns", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_equal(ncol(result), 2L)
})

test_that("hsi_extract_spectrum output columns are named wavelength and value", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_equal(names(result), c("wavelength", "value"))
})

test_that("hsi_extract_spectrum output has one row per band", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_equal(nrow(result), terra::nlyr(test_reflectance))
})

# ── Column contracts ─────────────────────────────────────────────────────────

test_that("hsi_extract_spectrum wavelength column is numeric", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_true(is.numeric(result$wavelength))
})

test_that("hsi_extract_spectrum wavelength values match input band names", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expected_wavelengths <- as.numeric(terra::names(test_reflectance))

  expect_equal(result$wavelength, expected_wavelengths)
})

test_that("hsi_extract_spectrum value column is numeric", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_true(is.numeric(result$value))
})

test_that("hsi_extract_spectrum value column contains only finite values", {
  result <- hsi_extract_spectrum(x = test_reflectance)

  expect_false(any(is.infinite(result$value)))
  expect_false(any(is.nan(result$value)))
})

# ── fun argument ─────────────────────────────────────────────────────────────

test_that("hsi_extract_spectrum fun = 'median' produces different values than fun = 'mean'", {
  result_mean <- hsi_extract_spectrum(x = test_reflectance, fun = "mean")
  result_median <- hsi_extract_spectrum(x = test_reflectance, fun = "median")

  expect_false(identical(result_mean$value, result_median$value))
})

test_that("hsi_extract_spectrum fun = 'median' returns same structure as fun = 'mean'", {
  result <- hsi_extract_spectrum(x = test_reflectance, fun = "median")

  expect_equal(names(result), c("wavelength", "value"))
  expect_equal(nrow(result), terra::nlyr(test_reflectance))
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_extract_spectrum errors with non-SpatRaster input", {
  expect_error(
    hsi_extract_spectrum(x = "not a raster")
  )
})

test_that("hsi_extract_spectrum errors when band names are not numeric wavelengths", {
  non_numeric <- test_reflectance
  names(non_numeric) <- paste0("band_", seq_len(terra::nlyr(test_reflectance)))

  expect_error(
    hsi_extract_spectrum(x = non_numeric),
    "numeric wavelengths",
    class = "hsitools_error"
  )
})
