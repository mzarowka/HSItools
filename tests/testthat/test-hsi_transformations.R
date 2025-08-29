# Test reflectance calculation ----
# Test values produced
# In a way it also test for a number of layers in a SpatRaster
test_that("Reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_reflectance(
      hsi_data = terra::rast(
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

# Test type
test_that("Calculated reflectance is a SpatRaster", {
  expect_s4_class(
    hsi_reflectance(
      hsi_data = terra::rast(
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

# Test Savitzky-Golay filter ----
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

# Test type
test_that("Calculated Savitzky-Golay data is a SpatRaster", {
  expect_s4_class(
    hsi_smooth_savgol(
      x = terra::rast(
        system.file(
          package = "HSItools",
          "testdata/products/REFLECTANCE_testdata.tif"
        )
      )
    ),
    class = "SpatRaster"
  )
})

# Test continuum removal ----
# Some tolerance is needed
test_that("Continuum removal reflectance is calculated as expected", {
  expect_equal(
    terra::values(hsi_continuum(
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

# Test type
test_that("Calculated continuum removed data is a SpatRaster", {
  expect_s4_class(
    hsi_continuum(
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

# Test median ----
# Some tolerance is needed
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

# Test type
test_that("Median smoothed data is a SpatRaster", {
  expect_s4_class(
    hsi_continuum(
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

# Test stretch ----

# Test RABD ----

# Test rmean ----

# Test ratio ----

# Test difference ----

# Test RABA ----

# Test REMP ----

# Test derivative ----

# Test NDI ----
