# Test reflectance calculation and values produced
# In a way it also test for a number of layers in a SpatRaster
test_that("reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_reflectance(
      sample = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/capture/testdata.tif"
        )
      ),
      whiteref = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/capture/WHITEREF_testdata.tif"
        )
      ),
      darkref = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/capture/DARKREF_testdata.tif"
        )
      )
    )),
    terra::values(terra::rast(
      system.file(
        package = "HSItools",
        "testdata/products/REFLECTANCE_testdata.tif"
      )
    ))
  )
})

# Savitzky-Golay filter check - gsignal version
# Some tolerance is needed
test_that("Savitzky-Golay smoothed reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_smooth_savgol(
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
        "testdata/products/SAVGOL_testdata.tif"
      )
    )),
    tolerance = 1e-6
  )
})
