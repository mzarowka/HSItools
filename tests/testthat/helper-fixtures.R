# Shared test fixtures, auto-loaded by testthat and devtools::load_all().
# Read-only: never mutate one inside a test; derive per-test copies instead.

test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)
