# Test reflectance calculation and values produced
# In a way it also test for a number of layers in a SpatRaster
test_that("reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_reflectance(
      sample = terra::rast(
        fs::path_package(
          package = "HSItools",
          "inst/testdata/capture/testdata.tif"
        )
      ),
      whiteref = terra::rast(
        fs::path_package(
          package = "HSItools",
          "inst/testdata/capture/WHITEREF_testdata.tif"
        )
      ),
      darkref = terra::rast(
        fs::path_package(
          package = "HSItools",
          "inst/testdata/capture/DARKREF_testdata.tif"
        )
      )
    )),
    terra::values(terra::rast(
      fs::path_package(
        package = "HSItools",
        "inst/testdata/products/REFLECTANCE_testdata.tif"
      )
    ))
  )
})
