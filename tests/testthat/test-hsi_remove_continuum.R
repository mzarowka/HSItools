# Test continuum removal ----
# hsi_remove_continuum applies prospectr::continuumRemoval per pixel.
# Output = spectrum / convex hull, so values are always in [0, 1].
# Band names are preserved. Input must have at least 3 bands.

## Setup ----
test_savgol <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/SAVGOL_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_remove_continuum returns a SpatRaster", {
  result <- hsi_remove_continuum(x = test_savgol)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_remove_continuum preserves number of bands", {
  result <- hsi_remove_continuum(x = test_savgol)

  expect_equal(terra::nlyr(result), terra::nlyr(test_savgol))
})

test_that("hsi_remove_continuum preserves spatial dimensions", {
  result <- hsi_remove_continuum(x = test_savgol)

  expect_equal(terra::nrow(result), terra::nrow(test_savgol))
  expect_equal(terra::ncol(result), terra::ncol(test_savgol))
})

# ── Band names ───────────────────────────────────────────────────────────────

test_that("hsi_remove_continuum preserves band names", {
  result <- hsi_remove_continuum(x = test_savgol)

  expect_equal(terra::names(result), terra::names(test_savgol))
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_remove_continuum matches CONREM_testdata fixture", {
  expected <- terra::rast(
    system.file(
      package = "HSItools",
      "testdata/products/CONREM_testdata.tif"
    )
  )

  result <- hsi_remove_continuum(x = test_savgol)

  expect_equal(
    terra::values(result),
    terra::values(expected),
    tolerance = 1e-6
  )
})

test_that("hsi_remove_continuum output values are in [0, 1]", {
  # spectrum / convex_hull is bounded [0, 1] by definition
  result <- hsi_remove_continuum(x = test_savgol)
  values <- terra::values(result, na.rm = TRUE)

  expect_true(all(values >= 0))
  expect_true(all(values <= 1))
})

test_that("hsi_remove_continuum produces only finite values", {
  result <- hsi_remove_continuum(x = test_savgol)
  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_remove_continuum gives identical values on parallel workers", {
  result_serial <- hsi_remove_continuum(x = test_savgol, cores = 1)
  result_parallel <- hsi_remove_continuum(x = test_savgol, cores = 2)

  expect_equal(
    terra::values(result_parallel),
    terra::values(result_serial)
  )
  expect_equal(
    terra::names(result_parallel),
    terra::names(result_serial)
  )
})

test_that("hsi_remove_continuum maps NA spectra to NA on parallel workers", {
  # Derive a per-test copy; the top-level fixture is read-only
  values_na <- terra::values(test_savgol)
  na_cells <- c(1, 5, 40)
  values_na[na_cells, 3] <- NA
  test_na <- terra::setValues(test_savgol, values_na)

  result_serial <- hsi_remove_continuum(x = test_na, cores = 1)
  result_parallel <- hsi_remove_continuum(x = test_na, cores = 2)

  # A spectrum containing NA returns NA in every band
  expect_true(all(is.na(terra::values(result_parallel)[na_cells, ])))

  # Unaffected pixels match the serial result
  expect_equal(
    terra::values(result_parallel)[-na_cells, ],
    terra::values(result_serial)[-na_cells, ]
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_remove_continuum writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_remove_continuum(
    x = test_savgol,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_remove_continuum errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_remove_continuum(
    x = test_savgol,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_remove_continuum(
      x = test_savgol,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_remove_continuum validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_remove_continuum,
    list(x = test_savgol)
  )
})

test_that("hsi_remove_continuum errors with non-SpatRaster input", {
  expect_error(
    hsi_remove_continuum(x = "not a raster")
  )
})

test_that("hsi_remove_continuum errors when cores is not a positive number", {
  expect_error(
    hsi_remove_continuum(x = test_savgol, cores = -1),
    "must contain only positive values",
    class = "hsitools_error"
  )
})

test_that("hsi_remove_continuum errors when input has fewer than 3 bands", {
  two_band <- terra::subset(test_savgol, 1:2)

  expect_error(
    hsi_remove_continuum(x = two_band),
    "at least 3 bands",
    class = "hsitools_error"
  )
})
