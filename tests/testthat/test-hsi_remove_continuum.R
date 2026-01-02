# Test continuum removal ----
## Test values produced ----
test_that("Continuum removal reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_remove_continuum(
      x = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/products/SAVGOL_testdata.tif"
        )
      )
    )),
    terra::values(terra::rast(
      system.file(
        package = "HSItools",
        "testdata/products/CONREM_testdata.tif"
      )
    )),
    tolerance = 1e-6
  )
})

## Test type ----
test_that("Calculated continuum removed data is a SpatRaster", {
  expect_s4_class(
    hsi_remove_continuum(
      x = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/products/SAVGOL_testdata.tif"
        )
      )
    ),
    class = "SpatRaster"
  )
})