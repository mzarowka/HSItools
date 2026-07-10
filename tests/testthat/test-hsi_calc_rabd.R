# Test RABD calculation ----
# RABD = continuum / trough reflectance — values > 1 indicate absorption.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_rabd returns a SpatRaster", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 660:680,
    index_type = "max"
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_rabd returns a single-band raster", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_rabd preserves spatial dimensions", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_type variants ──────────────────────────────────────────────────────

test_that("hsi_calc_rabd works with index_type = 'max'", {
  expect_no_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 660:680,
      index_type = "max"
    )
  )
})

test_that("hsi_calc_rabd works with index_type = 'strict'", {
  expect_no_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "strict"
    )
  )
})

test_that("hsi_calc_rabd works with index_type = 'mid'", {
  expect_no_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 660:680,
      index_type = "mid"
    )
  )
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_rabd sets layer name when index_name provided", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict",
    index_name = "rabd670"
  )

  expect_equal(terra::names(result), "rabd670")
})

test_that("hsi_calc_rabd has default terra name when index_name is NULL", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rabd produces positive values for absorption features", {
  # RABD = continuum / trough; both are positive reflectances, so result > 0
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_true(sum(values > 0) > length(values) * 0.5)
})

test_that("hsi_calc_rabd produces only finite values", {
  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict"
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_rabd writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict",
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_rabd errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_rabd(
    x = test_reflectance,
    continuum_edges = c(590, 730),
    absorption_band = 670,
    index_type = "strict",
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "strict",
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_rabd errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_rabd(
      x = "not a raster",
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "strict"
    )
  )
})

test_that("hsi_calc_rabd errors when continuum_edges is not length 2", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 660, 730),
      absorption_band = 670,
      index_type = "strict"
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_rabd errors with invalid index_type", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 670,
      index_type = "invalid"
    ),
    "must be one of",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_rabd errors with multiple absorption_band for index_type = 'strict'", {
  expect_error(
    hsi_calc_rabd(
      x = test_reflectance,
      continuum_edges = c(590, 730),
      absorption_band = 660:680,
      index_type = "strict"
    ),
    "must be length 1"
  )
})
