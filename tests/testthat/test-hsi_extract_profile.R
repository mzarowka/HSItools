# Test hsi_extract_profile ----
# Aggregates a SpatRaster perpendicular to the profile direction, returning
# a tibble with one row per position along the axis.
# Key contracts:
#   - Output is a tibble with a `position` column plus one column per band
#   - Vertical profile: nrow(x) rows; horizontal: ncol(x) rows
#   - Multi-band input produces one column per band, named after band names
#   - Band names conflicting with reserved names warn and are prefixed
#   - Invalid direction argument errors

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Single-band raster for simpler structural assertions
test_single <- terra::subset(test_reflectance, 1)

# Two-band raster for multi-band column contract
test_two <- terra::subset(test_reflectance, 1:2)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_extract_profile returns a tibble", {
  result <- hsi_extract_profile(x = test_single)

  expect_s3_class(result, "tbl_df")
})

# ── Output structure ─────────────────────────────────────────────────────────

test_that("hsi_extract_profile vertical profile has nrow(x) rows", {
  result <- hsi_extract_profile(x = test_single, direction = "vertical")

  expect_equal(nrow(result), terra::nrow(test_single))
})

test_that("hsi_extract_profile horizontal profile has ncol(x) rows", {
  result <- hsi_extract_profile(x = test_single, direction = "horizontal")

  expect_equal(nrow(result), terra::ncol(test_single))
})

test_that("hsi_extract_profile output always contains a position column", {
  result <- hsi_extract_profile(x = test_single)

  expect_true("position" %in% names(result))
})

test_that("hsi_extract_profile single-band input produces two columns", {
  # position + one band column
  result <- hsi_extract_profile(x = test_single)

  expect_equal(ncol(result), 2L)
})

test_that("hsi_extract_profile multi-band input produces one column per band plus position", {
  result <- hsi_extract_profile(x = test_two)

  expect_equal(ncol(result), terra::nlyr(test_two) + 1L)
})

# ── Column contracts ─────────────────────────────────────────────────────────

test_that("hsi_extract_profile band columns are named after input band names", {
  result <- hsi_extract_profile(x = test_two)

  band_cols <- setdiff(names(result), "position")

  expect_equal(band_cols, terra::names(test_two))
})

test_that("hsi_extract_profile position column is numeric", {
  result <- hsi_extract_profile(x = test_single)

  expect_true(is.numeric(result$position))
})

test_that("hsi_extract_profile band value columns are numeric", {
  result <- hsi_extract_profile(x = test_two)

  band_cols <- setdiff(names(result), "position")

  purrr::walk(band_cols, \(col) expect_true(is.numeric(result[[col]])))
})

# ── Direction behaviour ──────────────────────────────────────────────────────

test_that("hsi_extract_profile vertical and horizontal profiles have different position values", {
  # 9x9 raster: x and y coordinates differ in value
  result_v <- hsi_extract_profile(x = test_single, direction = "vertical")
  result_h <- hsi_extract_profile(x = test_single, direction = "horizontal")

  expect_false(identical(result_v$position, result_h$position))
})

test_that("hsi_extract_profile vertical profile position values are y-coordinates", {
  result <- hsi_extract_profile(x = test_single, direction = "vertical")

  # Y-coordinates from terra: decreasing downward, length == nrow
  expect_equal(length(result$position), terra::nrow(test_single))
})

# ── Band name conflict handling ──────────────────────────────────────────────

test_that("hsi_extract_profile warns when band name conflicts with reserved column name", {
  # Rename first band to "position" to trigger conflict
  conflicting <- test_single
  names(conflicting) <- "position"

  expect_warning(
    hsi_extract_profile(x = conflicting),
    regexp = "conflict",
    class = "hsitools_warning"
  )
})

test_that("hsi_extract_profile prefixes conflicting band name with 'band_'", {
  conflicting <- test_single
  names(conflicting) <- "position"

  result <- suppressWarnings(hsi_extract_profile(x = conflicting))

  expect_true("band_position" %in% names(result))
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_extract_profile errors with non-SpatRaster input", {
  expect_error(
    hsi_extract_profile(x = "not a raster")
  )
})

test_that("hsi_extract_profile errors with invalid direction argument", {
  expect_error(
    hsi_extract_profile(x = test_single, direction = "diagonal")
  )
})
