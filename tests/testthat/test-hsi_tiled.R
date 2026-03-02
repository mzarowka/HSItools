# Test hsi_tiled ----

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

## Test output type ----
test_that("hsi_tiled returns a SpatRaster", {
  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4
    )
  })

  expect_s4_class(result, "SpatRaster")
})

## Test output dimensions ----
test_that("hsi_tiled preserves spatial dimensions", {
  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4
    )
  })

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

## Test values match sequential ----
test_that("hsi_tiled produces same values as sequential", {
  sequential <- hsi_calc_rmean(x = test_reflectance)

  tiled <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = 4
    )
  })

  expect_equal(
    terra::values(tiled),
    terra::values(sequential),
    tolerance = 1e-6
  )
})

## Test 2D tile grid ----
test_that("hsi_tiled works with 2D tile specification", {
  result <- with(mirai::daemons(2), {
    hsi_tiled(
      fun = \(tile) HSItools::hsi_calc_rmean(tile),
      x = test_reflectance,
      n_tiles = c(2, 2)
    )
  })

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

## Test file writing ----
test_that("hsi_tiled writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

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
  unlink(temp_file)
})
