# Test reflectance calculation
test_that("reflectance is calculated as expected", {
  expect_equal(
    hsi_reflectance(
      sample = terra::rast(
        fs::path_package("HSItools", "inst/testdata/capture/testdata.tif")
      ),
      whiteref = terra::rast(
        fs::path_package(
          "HSItools",
          "inst/testdata/capture/WHITEREF_testdata.tif"
        )
      ),
      darkref = terra::rast(
        fs::path_package(
          "HSItools",
          "inst/testdata/capture/DARKREF_testdata.tif"
        )
      )
    ),
    terra::rast(
      fs::path_package(
        "HSItools",
        "inst/testdata/products/REFLECTANCE_testdata.tif"
      )
    )
  )
})
