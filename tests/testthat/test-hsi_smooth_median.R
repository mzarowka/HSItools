# Test median ----
## Test values produced ----
test_that("Spatial median smoothing is calculated as expected", {
  expect_equal(
    terra::values(hsi_smooth_median(
      x = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/products/REFLECTANCE_testdata.tif"
        )
      )
    )),
    terra::values(terra::rast(
      system.file(
        package = "HSItools",
        "testdata/products/MEDIAN_testdata.tif"
      )
    )),
    tolerance = 1e-6
  )
})

## Test type ----
test_that("Median smoothed data is a SpatRaster", {
  expect_s4_class(
    hsi_remove_continuum(
      x = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/products/MEDIAN_testdata.tif"
        )
      )
    ),
    class = "SpatRaster"
  )
})
