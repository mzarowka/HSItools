# Test lambdaREMP calculation ----
# lambdaREMP is the wavelength (nm) of the Red-Edge Minimum Point — the
# zero-crossing of the first derivative of reflectance within a search range.
# Output values are wavelengths, so they must lie within search_range.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Derivative is computed once and reused across tests
test_deriv <- hsi_smooth_savgol(x = test_reflectance, m = 1)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_remp returns a SpatRaster", {
  result <- hsi_calc_remp(x = test_deriv)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_remp returns a single-band raster", {
  result <- hsi_calc_remp(x = test_deriv)

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_remp preserves spatial dimensions", {
  result <- hsi_calc_remp(x = test_deriv)

  expect_equal(terra::nrow(result), terra::nrow(test_deriv))
  expect_equal(terra::ncol(result), terra::ncol(test_deriv))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_remp sets layer name when index_name provided", {
  result <- hsi_calc_remp(
    x = test_deriv,
    index_name = "remp"
  )

  expect_equal(terra::names(result), "remp")
})

test_that("hsi_calc_remp has default terra name when index_name is NULL", {
  result <- hsi_calc_remp(x = test_deriv)

  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_remp output values lie within search_range", {
  # The function only searches within search_range — output must be bounded by it
  search_range <- c(660, 680)

  result <- hsi_calc_remp(
    x = test_deriv,
    search_range = search_range
  )

  values <- terra::values(result, na.rm = TRUE)

  expect_true(all(values >= search_range[1]))
  expect_true(all(values <= search_range[2]))
})

test_that("hsi_calc_remp output values lie within a custom search_range", {
  search_range <- c(665, 690)

  result <- hsi_calc_remp(
    x = test_deriv,
    search_range = search_range
  )

  values <- terra::values(result, na.rm = TRUE)

  expect_true(all(values >= search_range[1]))
  expect_true(all(values <= search_range[2]))
})

test_that("hsi_calc_remp produces only finite values", {
  result <- hsi_calc_remp(x = test_deriv)

  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_calc_remp gives identical values on parallel workers", {
  result_serial <- hsi_calc_remp(x = test_deriv, cores = 1)
  result_parallel <- hsi_calc_remp(x = test_deriv, cores = 2)

  expect_equal(
    terra::values(result_parallel),
    terra::values(result_serial)
  )
  expect_equal(
    terra::names(result_parallel),
    terra::names(result_serial)
  )
})

test_that("hsi_calc_remp maps NA spectra to NA on parallel workers", {
  # The NA must land inside the search range, which is all the pixel function
  # ever sees
  band_wavelengths <- as.numeric(terra::names(test_deriv))
  in_range <- which(band_wavelengths >= 660 & band_wavelengths <= 680)
  na_band <- in_range[ceiling(length(in_range) / 2)]

  # Derive a per-test copy; the top-level fixture is read-only
  values_na <- terra::values(test_deriv)
  na_cells <- c(1, 5, 40)
  values_na[na_cells, na_band] <- NA
  test_na <- terra::setValues(test_deriv, values_na)

  result_serial <- hsi_calc_remp(x = test_na, cores = 1)
  result_parallel <- hsi_calc_remp(x = test_na, cores = 2)

  # A spectrum containing NA returns NA
  expect_true(all(is.na(terra::values(result_parallel)[na_cells, ])))

  # Unaffected pixels match the serial result
  expect_equal(
    terra::values(result_parallel)[-na_cells, ],
    terra::values(result_serial)[-na_cells, ]
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_remp writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_remp(
    x = test_deriv,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_remp errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_remp(
    x = test_deriv,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_remp(
      x = test_deriv,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_remp validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_remp,
    list(x = test_reflectance, search_range = c(660, 680))
  )
})

test_that("hsi_calc_remp errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_remp(x = "not a raster")
  )
})

test_that("hsi_calc_remp errors when search_range is not length 2", {
  expect_error(
    hsi_calc_remp(
      x = test_deriv,
      search_range = c(660, 670, 680)
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_remp errors when cores is not a positive number", {
  expect_error(
    hsi_calc_remp(x = test_deriv, cores = -1),
    "must contain only positive values",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_remp errors when search_range covers fewer than 2 bands", {
  # A range so narrow it contains only one band triggers the internal check
  expect_error(
    hsi_calc_remp(
      x = test_deriv,
      search_range = c(660, 660)
    )
  )
})
