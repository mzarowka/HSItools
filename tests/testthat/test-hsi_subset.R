# Test hsi_subset ----
# Extracts bands by wavelength value using nearest-band matching.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_subset returns a SpatRaster", {
  result <- hsi_subset(x = test_reflectance, wavelength = 670)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_subset returns 1 band for a single wavelength", {
  result <- hsi_subset(x = test_reflectance, wavelength = 670)

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_subset returns n bands for n wavelengths", {
  result <- hsi_subset(x = test_reflectance, wavelength = c(600, 650, 700))

  expect_equal(terra::nlyr(result), 3)
})

test_that("hsi_subset preserves spatial dimensions", {
  result <- hsi_subset(x = test_reflectance, wavelength = 670)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── Band name correctness ────────────────────────────────────────────────────

test_that("hsi_subset output band name is a wavelength present in the input", {
  result <- hsi_subset(x = test_reflectance, wavelength = 670)

  input_wavelengths <- as.numeric(terra::names(test_reflectance))
  result_wavelength <- as.numeric(terra::names(result))

  expect_true(result_wavelength %in% input_wavelengths)
})

test_that("hsi_subset with multiple wavelengths returns bands present in input", {
  requested <- c(600, 650, 700)
  result <- hsi_subset(x = test_reflectance, wavelength = requested)

  input_wavelengths <- as.numeric(terra::names(test_reflectance))
  result_wavelengths <- as.numeric(terra::names(result))

  expect_true(all(result_wavelengths %in% input_wavelengths))
})

test_that("hsi_subset selects nearest band when exact wavelength not present", {
  # Request a wavelength between two bands — should return one band
  result <- hsi_subset(x = test_reflectance, wavelength = 667)

  expect_equal(terra::nlyr(result), 1)

  # The returned band name must be one of the actual input bands
  input_wavelengths <- as.numeric(terra::names(test_reflectance))
  result_wavelength <- as.numeric(terra::names(result))

  expect_true(result_wavelength %in% input_wavelengths)
})

test_that("hsi_subset result is a proper subset of the input raster", {
  result <- hsi_subset(x = test_reflectance, wavelength = 670)
  result_name <- terra::names(result)

  # The selected band must exist verbatim in the input
  expect_true(result_name %in% terra::names(test_reflectance))

  # Cell values must match the same band in the original
  original_band <- terra::subset(test_reflectance, result_name)

  expect_equal(
    unname(terra::values(result)),
    unname(terra::values(original_band)),
    tolerance = 1e-6
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_subset writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_subset(
    x = test_reflectance,
    wavelength = 670,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_subset errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_subset(
    x = test_reflectance,
    wavelength = 670,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_subset(
      x = test_reflectance,
      wavelength = 670,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_subset errors with non-SpatRaster input", {
  expect_error(
    hsi_subset(x = "not a raster", wavelength = 670)
  )
})

test_that("hsi_subset errors with empty wavelength vector", {
  expect_error(
    hsi_subset(x = test_reflectance, wavelength = numeric(0))
  )
})

test_that("hsi_subset errors with non-numeric wavelength", {
  expect_error(
    hsi_subset(x = test_reflectance, wavelength = "670")
  )
})
