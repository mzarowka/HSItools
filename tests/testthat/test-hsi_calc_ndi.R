# Test normalized difference index calculation ----
# NDI = (band1 - band2) / (band1 + band2)
# For non-negative reflectance this is mathematically bounded to [-1, 1].

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_ndi returns a SpatRaster", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_ndi returns a single-band raster", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_ndi preserves spatial dimensions", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_ndi sets layer name when index_name provided", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690),
    index_name = "ndi570690"
  )

  expect_equal(terra::names(result), "ndi570690")
})

test_that("hsi_calc_ndi has default terra name when index_name is NULL", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_ndi output is bounded to [-1, 1] for non-negative reflectance", {
  # NDI = (a - b) / (a + b); for a, b >= 0 this is a mathematical guarantee
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_true(all(values >= -1))
  expect_true(all(values <= 1))
})

test_that("hsi_calc_ndi with reversed bands negates the result", {
  # NDI(a, b) = (a - b) / (a + b) = -((b - a) / (b + a)) = -NDI(b, a)
  result_fwd <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  result_rev <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(690, 570)
  )

  expect_equal(
    unname(terra::values(result_fwd)),
    -unname(terra::values(result_rev)),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_ndi produces only finite values", {
  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690)
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_calc_ndi errors when both bands resolve to the same position", {
  # wavelength_position deduplicates identical inputs — documents known behaviour
  expect_error(
    hsi_calc_ndi(
      x = test_reflectance,
      bands = c(660, 660)
    )
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_ndi writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_ndi errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_ndi(
    x = test_reflectance,
    bands = c(570, 690),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_ndi(
      x = test_reflectance,
      bands = c(570, 690),
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_ndi errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_ndi(
      x = "not a raster",
      bands = c(570, 690)
    )
  )
})

test_that("hsi_calc_ndi errors when bands is not length 2", {
  expect_error(
    hsi_calc_ndi(
      x = test_reflectance,
      bands = c(570, 620, 690)
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_ndi errors when bands is length 1", {
  expect_error(
    hsi_calc_ndi(
      x = test_reflectance,
      bands = 660
    ),
    "must be length 2"
  )
})
