# Test relative absorption band area calculation ----
# hsi_calc_raba() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_raba gives identical values on parallel workers", {
  result_serial <- hsi_calc_raba(
    x = test_reflectance,
    continuum_edges = c(650, 700),
    cores = 1
  )
  result_parallel <- hsi_calc_raba(
    x = test_reflectance,
    continuum_edges = c(650, 700),
    cores = 2
  )

  expect_equal(
    terra::values(result_parallel),
    terra::values(result_serial)
  )
  expect_equal(
    terra::names(result_parallel),
    terra::names(result_serial)
  )
})

test_that("hsi_calc_raba maps NA spectra to NA on parallel workers", {
  # The NA must land inside the continuum window, which is all the pixel
  # function ever sees
  band_wavelengths <- as.numeric(terra::names(test_reflectance))
  in_range <- which(band_wavelengths >= 650 & band_wavelengths <= 700)
  na_band <- in_range[ceiling(length(in_range) / 2)]

  # Derive a per-test copy; the top-level fixture is read-only
  values_na <- terra::values(test_reflectance)
  na_cells <- c(1, 5, 40)
  values_na[na_cells, na_band] <- NA
  test_na <- terra::setValues(test_reflectance, values_na)

  result_serial <- hsi_calc_raba(
    x = test_na,
    continuum_edges = c(650, 700),
    cores = 1
  )
  result_parallel <- hsi_calc_raba(
    x = test_na,
    continuum_edges = c(650, 700),
    cores = 2
  )

  # A spectrum containing NA returns NA
  expect_true(all(is.na(terra::values(result_parallel)[na_cells, ])))

  # Unaffected pixels match the serial result
  expect_equal(
    terra::values(result_parallel)[-na_cells, ],
    terra::values(result_serial)[-na_cells, ]
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_raba validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_raba,
    list(x = test_reflectance, continuum_edges = c(650, 700))
  )
})

test_that("hsi_calc_raba errors when cores is not a positive number", {
  expect_error(
    hsi_calc_raba(
      x = test_reflectance,
      continuum_edges = c(650, 700),
      cores = -1
    ),
    "must contain only positive values",
    class = "hsitools_error"
  )
})
