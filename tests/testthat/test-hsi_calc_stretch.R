# Test stretch calculation ----
# hsi_calc_stretch selects 3 bands and applies terra::stretch, producing
# a 3-band SpatRaster with values in [0, 1].
#
# Note: Test data spans ~517–772 nm. All predefined presets (RGB, NIR, CIR,
# SWIR) require bands outside this range and are therefore tested as error
# paths. Happy-path tests use custom wavelengths within the available range.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Three wavelengths confirmed within the test data range (~517-772 nm)
custom_bands <- c(700, 620, 540)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_stretch returns a SpatRaster", {
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_stretch always returns exactly 3 bands", {
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands
  )

  expect_equal(terra::nlyr(result), 3)
})

test_that("hsi_calc_stretch preserves spatial dimensions", {
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── type variants ────────────────────────────────────────────────────────────

test_that("hsi_calc_stretch works with custom numeric wavelengths", {
  expect_no_error(
    hsi_calc_stretch(x = test_reflectance, type = custom_bands)
  )
})

# Predefined presets all require bands outside the ~517-772 nm test data range
test_that("hsi_calc_stretch errors when preset bands fall outside available range", {
  expect_error(hsi_calc_stretch(x = test_reflectance, type = "RGB"))
  expect_error(hsi_calc_stretch(x = test_reflectance, type = "NIR"))
  expect_error(hsi_calc_stretch(x = test_reflectance, type = "CIR"))
  expect_error(hsi_calc_stretch(x = test_reflectance, type = "SWIR"))
})

# ── histeq argument ──────────────────────────────────────────────────────────

test_that("hsi_calc_stretch works with histeq = TRUE", {
  expect_no_error(
    hsi_calc_stretch(x = test_reflectance, type = custom_bands, histeq = TRUE)
  )
})

test_that("hsi_calc_stretch works with histeq = FALSE", {
  expect_no_error(
    hsi_calc_stretch(x = test_reflectance, type = custom_bands, histeq = FALSE)
  )
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_stretch output values are in [0, 255] after linear stretch", {
  # terra::stretch default range is [0, 255]
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_true(all(values >= 0))
  expect_true(all(values <= 255))
})

test_that("hsi_calc_stretch with histeq = TRUE output values are in [0, 255]", {
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands,
    histeq = TRUE
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_true(all(values >= 0))
  expect_true(all(values <= 255))
})

test_that("hsi_calc_stretch produces only finite values", {
  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_stretch writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_stretch errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_stretch(
    x = test_reflectance,
    type = custom_bands,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_stretch(
      x = test_reflectance,
      type = custom_bands,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_stretch validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_stretch,
    list(x = test_reflectance, type = custom_bands)
  )
})

test_that("hsi_calc_stretch errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_stretch(x = "not a raster", type = custom_bands)
  )
})

test_that("hsi_calc_stretch errors with unknown string type", {
  expect_error(
    hsi_calc_stretch(x = test_reflectance, type = "INVALID"),
    "Unknown band type",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_stretch errors when custom type has fewer than 3 wavelengths", {
  expect_error(
    hsi_calc_stretch(x = test_reflectance, type = c(650, 550)),
    "exactly 3",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_stretch errors when custom type has more than 3 wavelengths", {
  expect_error(
    hsi_calc_stretch(x = test_reflectance, type = c(700, 660, 620, 540)),
    "exactly 3",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_stretch errors when an unused argument is passed without filename", {
  expect_error(
    hsi_calc_stretch(
      x = test_reflectance,
      type = c(700, 620, 540),
      bogus_arg = 1
    ),
    "not used",
    class = "hsitools_error"
  )
})
