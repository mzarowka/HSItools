# Fixtures for test-hsi_coregister.R, auto-loaded by testthat and
# devtools::load_all(). Read-only: never mutate one inside a test.

# Target grid: empty raster with the geometry of the reflectance fixture
test_target <- terra::rast(
  terra::rast(
    system.file(
      package = "HSItools",
      "testdata/products/REFLECTANCE_testdata.tif"
    )
  ),
  nlyrs = 1
)

# Non-collinear GCPs at the four corners, an identity transform
test_gcp <- tibble::tibble(
  gcp_id = 1:4,
  source_x = c(
    terra::xmin(test_target),
    terra::xmax(test_target),
    terra::xmin(test_target),
    terra::xmax(test_target)
  ),
  source_y = c(
    terra::ymax(test_target),
    terra::ymax(test_target),
    terra::ymin(test_target),
    terra::ymin(test_target)
  ),
  target_x = c(
    terra::xmin(test_target),
    terra::xmax(test_target),
    terra::xmin(test_target),
    terra::xmax(test_target)
  ),
  target_y = c(
    terra::ymax(test_target),
    terra::ymax(test_target),
    terra::ymin(test_target),
    terra::ymin(test_target)
  )
)
