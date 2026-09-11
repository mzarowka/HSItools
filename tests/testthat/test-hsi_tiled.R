# Test hsi_tiled ----
# hsi_tiled requires mirai (daemons), withr (local_tempdir), and carrier
# (purrr::in_parallel) — all Suggests. Every test skips gracefully when
# any of these are absent, which is required for CRAN compliance.

## Skip helper ----
skip_if_tiled_unavailable <- function() {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("carrier")
}

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_tiled returns a SpatRaster", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4,
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_tiled preserves spatial dimensions", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4,
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

test_that("hsi_tiled works with 2D tile specification", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = c(2, 2),
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_tiled produces same values as sequential execution", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  sequential <- hsi_calc_rmean(x = test_reflectance)

  tiled <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4,
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_equal(
    terra::values(tiled),
    terra::values(sequential),
    tolerance = 1e-6
  )
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_tiled writes to file when filename provided", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4,
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_true(file.exists(temp_file))
})

test_that("hsi_tiled errors when file exists and overwrite = FALSE", {
  skip_if_tiled_unavailable()

  temp_file <- withr::local_tempfile(fileext = ".tif")

  with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4,
      filename = temp_file,
      overwrite = TRUE
    )
  })

  expect_error(
    with(mirai::daemons(2), {
      hsi_tiled(
        fun = \(tile) HSItools::hsi_calc_rmean(tile),
        x = test_reflectance,
        n_tiles = 4,
        filename = temp_file,
        overwrite = FALSE
      )
    })
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_tiled validates filename and overwrite", {
  skip_if_tiled_unavailable()

  expect_write_tail_validated(
    hsi_tiled,
    list(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4
    )
  )
})

test_that("hsi_tiled errors with non-SpatRaster input", {
  skip_if_tiled_unavailable()

  expect_error(
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = "not a raster",
      n_tiles = 4
    )
  )
})

test_that("hsi_tiled errors when n_tiles has length > 2", {
  skip_if_tiled_unavailable()

  expect_error(
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = c(2, 2, 2)
    )
  )
})
