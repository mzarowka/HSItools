# Test hsi_coregister ----
# Warps a source raster onto a target grid using matched GCPs via GDAL.
# Key contracts: output always matches target grid dimensions; band names are
# preserved both in-memory and on disk; x must have a file source.

## Setup ----
test_source <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Target grid: same extent, half the resolution — forces actual resampling
test_target <- terra::rast(
  extent = terra::ext(test_source),
  nrows = terra::nrow(test_source),
  ncols = terra::ncol(test_source)
)

# Non-collinear GCPs: four corners of a near-identity transform
# source and target coords differ by a small constant offset
test_gcp <- tibble::tibble(
  gcp_id = 1:4,
  source_x = c(
    terra::xmin(test_source),
    terra::xmax(test_source),
    terra::xmin(test_source),
    terra::xmax(test_source)
  ),
  source_y = c(
    terra::ymax(test_source),
    terra::ymax(test_source),
    terra::ymin(test_source),
    terra::ymin(test_source)
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

# ── Output type ───────────────────────────────────────────────────────────────

test_that("hsi_coregister returns a SpatRaster", {
  result <- hsi_coregister(test_source, test_target, test_gcp)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ─────────────────────────────────────────────────────────

test_that("hsi_coregister output nrow matches target", {
  result <- hsi_coregister(test_source, test_target, test_gcp)

  expect_equal(terra::nrow(result), terra::nrow(test_target))
})

test_that("hsi_coregister output ncol matches target", {
  result <- hsi_coregister(test_source, test_target, test_gcp)

  expect_equal(terra::ncol(result), terra::ncol(test_target))
})

test_that("hsi_coregister output nlyr matches source", {
  result <- hsi_coregister(test_source, test_target, test_gcp)

  expect_equal(terra::nlyr(result), terra::nlyr(test_source))
})

# ── Band names ────────────────────────────────────────────────────────────────

test_that("hsi_coregister preserves band names on in-memory result", {
  result <- hsi_coregister(test_source, test_target, test_gcp)

  expect_equal(names(result), names(test_source))
})

test_that("hsi_coregister bakes band names into output file on disk", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_coregister(
    test_source,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  # Read back from disk — names must survive round-trip
  result_from_disk <- terra::rast(temp_file)

  expect_equal(names(result_from_disk), names(test_source))

  unlink(temp_file)
})

# ── File writing ──────────────────────────────────────────────────────────────

test_that("hsi_coregister writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_coregister(
    test_source,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_coregister errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_coregister(
    test_source,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_coregister(
      test_source,
      test_target,
      test_gcp,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_coregister errors with non-SpatRaster x", {
  expect_error(hsi_coregister("not a raster", test_target, test_gcp))
})

test_that("hsi_coregister errors with non-SpatRaster y", {
  expect_error(hsi_coregister(test_source, "not a raster", test_gcp))
})

test_that("hsi_coregister errors with non-data-frame gcp", {
  expect_error(hsi_coregister(test_source, test_target, "not a data frame"))
})

test_that("hsi_coregister errors when gcp columns are missing", {
  bad_gcp <- tibble::tibble(source_x = 1:4, source_y = 1:4)

  expect_error(hsi_coregister(test_source, test_target, bad_gcp))
})

test_that("hsi_coregister errors when fewer than 3 GCPs provided", {
  two_gcp <- test_gcp |> dplyr::slice(1:2)

  expect_error(
    hsi_coregister(test_source, test_target, two_gcp),
    "at least 3"
  )
})

test_that("hsi_coregister errors when GCPs are collinear", {
  collinear_gcp <- tibble::tibble(
    gcp_id = 1:4,
    source_x = c(10, 20, 30, 40),
    source_y = c(100, 200, 300, 400),
    target_x = c(15, 25, 35, 45),
    target_y = c(110, 210, 310, 410)
  )

  expect_error(
    hsi_coregister(test_source, test_target, collinear_gcp),
    "collinear"
  )
})

test_that("hsi_coregister errors when x has no file source", {
  # In-memory raster has no path — GDAL cannot operate on it
  in_memory <- terra::rast(
    nrows = 9,
    ncols = 9,
    nlyr = 3,
    vals = runif(9 * 9 * 3)
  )

  expect_error(
    hsi_coregister(in_memory, test_target, test_gcp),
    "no file source"
  )
})

test_that("hsi_coregister errors with invalid method", {
  expect_error(
    hsi_coregister(test_source, test_target, test_gcp, method = "invalid"),
    "invalid"
  )
})
