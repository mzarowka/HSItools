# Test hsi_coregister ----
# Warps a source raster onto a target grid using matched GCPs via GDAL.
# Key contracts: output always matches target grid dimensions; band names are
# preserved both in-memory and on disk; x must have a file source.

# ── Output type ───────────────────────────────────────────────────────────────

test_that("hsi_coregister returns a SpatRaster", {
  result <- hsi_coregister(test_reflectance, test_target, test_gcp)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ─────────────────────────────────────────────────────────

test_that("hsi_coregister output matches target grid and source bands", {
  result <- hsi_coregister(test_reflectance, test_target, test_gcp)

  expect_equal(terra::nrow(result), terra::nrow(test_target))
  expect_equal(terra::ncol(result), terra::ncol(test_target))
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

# ── Band names ────────────────────────────────────────────────────────────────

test_that("hsi_coregister preserves band names on in-memory result", {
  result <- hsi_coregister(test_reflectance, test_target, test_gcp)

  expect_equal(names(result), names(test_reflectance))
})

test_that("hsi_coregister bakes band names into output file on disk", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_coregister(
    test_reflectance,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  # Read back from disk — names must survive round-trip
  result_from_disk <- terra::rast(temp_file)

  expect_equal(names(result_from_disk), names(test_reflectance))
})

test_that("hsi_coregister writes names(x) to disk, not the names in x's file", {
  # Layers renamed in memory must reach disk, not the names in x's file
  temp_file <- withr::local_tempfile(fileext = ".tif")

  renamed <- terra::subset(
    test_reflectance,
    seq_len(terra::nlyr(test_reflectance))
  )
  names(renamed) <- paste0("renamed_", seq_len(terra::nlyr(renamed)))

  hsi_coregister(
    renamed,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_equal(names(terra::rast(temp_file)), names(renamed))
})

test_that("hsi_coregister returns a raster whose values are readable", {
  # Returned raster must stay readable after the call, with and without filename
  temp_file <- withr::local_tempfile(fileext = ".tif")

  written <- hsi_coregister(
    test_reflectance,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  in_memory <- hsi_coregister(test_reflectance, test_target, test_gcp)

  expect_true(file.exists(terra::sources(written)))
  expect_true(file.exists(terra::sources(in_memory)))
  expect_no_error(terra::values(written))
  expect_no_error(terra::values(in_memory))
})

# ── File writing ──────────────────────────────────────────────────────────────

test_that("hsi_coregister writes to file when filename provided", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- hsi_coregister(
    test_reflectance,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")
})

test_that("hsi_coregister errors when file exists and overwrite = FALSE", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_coregister(
    test_reflectance,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_coregister(
      test_reflectance,
      test_target,
      test_gcp,
      filename = temp_file,
      overwrite = FALSE
    ),
    "already exists",
    class = "hsitools_error"
  )
})

test_that("hsi_coregister overwrite = TRUE replaces an existing file", {
  # A 3-band file must fully replace the existing 101-band output
  temp_file <- withr::local_tempfile(fileext = ".tif")

  three_bands <- terra::writeRaster(
    terra::subset(test_reflectance, 1:3),
    withr::local_tempfile(fileext = ".tif")
  )

  hsi_coregister(
    test_reflectance,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  hsi_coregister(
    three_bands,
    test_target,
    test_gcp,
    filename = temp_file,
    overwrite = TRUE
  )

  result_from_disk <- terra::rast(temp_file)

  expect_equal(terra::nlyr(result_from_disk), 3)
  expect_equal(names(result_from_disk), names(three_bands))
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_coregister validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_coregister,
    list(x = test_reflectance, y = test_target, gcp = test_gcp)
  )
})

test_that("hsi_coregister errors with non-SpatRaster x", {
  expect_error(hsi_coregister("not a raster", test_target, test_gcp))
})

test_that("hsi_coregister errors with non-SpatRaster y", {
  expect_error(hsi_coregister(test_reflectance, "not a raster", test_gcp))
})

test_that("hsi_coregister errors with non-data-frame gcp", {
  expect_error(hsi_coregister(
    test_reflectance,
    test_target,
    "not a data frame"
  ))
})

test_that("hsi_coregister errors when gcp columns are missing", {
  bad_gcp <- tibble::tibble(source_x = 1:4, source_y = 1:4)

  expect_error(hsi_coregister(test_reflectance, test_target, bad_gcp))
})

test_that("hsi_coregister errors when fewer than 3 GCPs provided", {
  two_gcp <- test_gcp |> dplyr::slice(1:2)

  expect_error(
    hsi_coregister(test_reflectance, test_target, two_gcp),
    "at least 3",
    class = "hsitools_error"
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
    hsi_coregister(test_reflectance, test_target, collinear_gcp),
    "collinear",
    class = "hsitools_error"
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
    "no file source",
    class = "hsitools_error"
  )
})

test_that("hsi_coregister errors with invalid method", {
  expect_error(
    hsi_coregister(test_reflectance, test_target, test_gcp, method = "invalid"),
    "invalid",
    class = "hsitools_error"
  )
})
