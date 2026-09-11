# Test band difference calculation ----

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_difference returns a SpatRaster", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_difference returns a single-band raster", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  expect_equal(terra::nlyr(result), 1)
})

test_that("hsi_calc_difference preserves spatial dimensions", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── index_name argument ──────────────────────────────────────────────────────

test_that("hsi_calc_difference sets layer name when index_name provided", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680),
    index_name = "diff620680"
  )

  expect_equal(terra::names(result), "diff620680")
})

test_that("hsi_calc_difference has default terra name when index_name is NULL", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  # terra assigns a default name; we just check it is a single character string
  expect_length(terra::names(result), 1)
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_difference errors when both bands resolve to the same position", {
  # wavelength_position deduplicates identical inputs, leaving band_positions[2]
  # as NA — terra then errors on subset. This documents the known behaviour.
  expect_error(
    hsi_calc_difference(
      x = test_reflectance,
      bands = c(660, 660)
    )
  )
})

test_that("hsi_calc_difference with reversed bands negates the result", {
  result_fwd <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  result_rev <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(680, 620)
  )

  # unname() strips band-name dimnames that differ between the two calls
  expect_equal(
    unname(terra::values(result_fwd)),
    -unname(terra::values(result_rev)),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_difference produces only finite values", {
  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680)
  )

  values <- terra::values(result, na.rm = TRUE)
  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_difference writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_difference errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_calc_difference(
    x = test_reflectance,
    bands = c(620, 680),
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_difference(
      x = test_reflectance,
      bands = c(620, 680),
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_difference validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_difference,
    list(x = test_reflectance, bands = c(570, 690))
  )
})

test_that("hsi_calc_difference errors with non-SpatRaster input", {
  expect_error(
    hsi_calc_difference(
      x = "not a raster",
      bands = c(620, 680)
    )
  )
})

test_that("hsi_calc_difference errors when bands is not length 2", {
  expect_error(
    hsi_calc_difference(
      x = test_reflectance,
      bands = c(620, 660, 680)
    ),
    "must be length 2"
  )
})

test_that("hsi_calc_difference errors when bands is length 1", {
  expect_error(
    hsi_calc_difference(
      x = test_reflectance,
      bands = 660
    ),
    "must be length 2"
  )
})
