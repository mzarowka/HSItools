# Test reflectance calculation ----
## To file ----
### Test values produced ----
test_that("Reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_calc_reflectance(
      x = terra::rast(
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
    )),
    tolerance = 1e-6
  )
})

### Test type ----
test_that("Calculated reflectance is a SpatRaster", {
  expect_s4_class(
    hsi_calc_reflectance(
      x = terra::rast(
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
    ),
    class = "SpatRaster"
  )
})

## In memory ----
### Test values produced ----
test_that("Reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_calc_reflectance(
      x = terra::rast(
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
      ),
      in_memory = TRUE
    )),
    terra::values(terra::rast(
      system.file(
        package = "HSItools",
        "testdata/products/REFLECTANCE_testdata.tif"
      )
    )),
    tolerance = 1e-6
  )
})

### Test type ----
test_that("Calculated reflectance is a SpatRaster", {
  expect_s4_class(
    hsi_calc_reflectance(
      x = terra::rast(
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
      ),
      in_memory = TRUE
    ),
    class = "SpatRaster"
  )
})
