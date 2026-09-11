# Test hsi_calc_reflectance ----
# Calibrates a raw hyperspectral capture (x) against white and dark references
# to reflectance. Contracts under test:
#   - Output is a SpatRaster carrying the dimensions and band names of x.
#   - The three calibration paths behave as specified: matched darks at equal
#     tint reproduce the single-session result; a differing tint without
#     darkspec scales the dark, changes the values, and warns.
#   - Output is finite; single-session values match the reference fixture.
#   - Writes to file when filename is given; guards on overwrite.
#   - Rejects non-SpatRaster inputs, band-count mismatches, invalid tint, and
#     non-numeric band names.

## Setup ----
test_x <- terra::rast(
  system.file("testdata/capture/testdata.tif", package = "HSItools")
)

test_whiteref <- terra::rast(
  system.file("testdata/capture/WHITEREF_testdata.tif", package = "HSItools")
)

test_darkref <- terra::rast(
  system.file("testdata/capture/DARKREF_testdata.tif", package = "HSItools")
)

# Output type ----

test_that("hsi_calc_reflectance returns a SpatRaster", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_s4_class(result, "SpatRaster")
})

# Output dimensions ----

test_that("hsi_calc_reflectance preserves the dimensions of x", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(terra::nlyr(result), terra::nlyr(test_x))
  expect_equal(terra::nrow(result), terra::nrow(test_x))
  expect_equal(terra::ncol(result), terra::ncol(test_x))
})

# Band names ----

test_that("hsi_calc_reflectance preserves the band names of x", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(names(result), names(test_x))
})

# Value sanity ----

test_that("hsi_calc_reflectance matches the reference fixture", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(
    terra::values(result),
    terra::values(test_reflectance),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_reflectance produces only finite values", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  values <- terra::values(result, na.rm = TRUE)

  expect_false(any(is.infinite(values)))
  expect_false(any(is.nan(values)))
})

test_that("hsi_calc_reflectance matched darks at equal tint match the single-session path", {
  # darkspec == darkref with tint = c(1, 1) makes the matched-dark path
  # algebraically identical to the single-session path.
  result_single <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  result_matched <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    darkspec = test_darkref,
    tint = c(1, 1),
    in_memory = TRUE
  )

  expect_equal(
    terra::values(result_single),
    terra::values(result_matched),
    tolerance = 1e-6
  )
})

test_that("hsi_calc_reflectance scaled dark path changes the values", {
  # A differing tint without darkspec triggers integration-time scaling, which
  # must move the result away from the equal-tint single-session values.
  result_single <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    tint = c(1, 1),
    in_memory = TRUE
  )

  result_scaled <- suppressWarnings(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(2, 1),
      in_memory = TRUE
    )
  )

  expect_false(
    isTRUE(all.equal(
      terra::values(result_single),
      terra::values(result_scaled)
    ))
  )
})

# Warnings ----

test_that("hsi_calc_reflectance warns only on the scaled dark path", {
  # Scaling (differing tint, no darkspec) warns; matched darks and equal tint
  # do not.
  expect_warning(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(1, 2),
      in_memory = TRUE
    ),
    "Scaling dark reference",
    class = "hsitools_warning"
  )

  expect_no_warning(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      darkspec = test_darkref,
      tint = c(1, 2),
      in_memory = TRUE
    )
  )

  expect_no_warning(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      in_memory = TRUE
    )
  )
})

# File writing ----

test_that("hsi_calc_reflectance writes to file when filename is provided", {
  # Exercises both the in-memory and file-backed write branches.
  temp_memory <- withr::local_tempfile(fileext = ".tif")
  temp_backed <- withr::local_tempfile(fileext = ".tif")

  result_memory <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE,
    filename = temp_memory,
    overwrite = TRUE
  )

  result_backed <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = FALSE,
    filename = temp_backed,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_memory))
  expect_s4_class(result_memory, "SpatRaster")
  expect_true(file.exists(temp_backed))
  expect_s4_class(result_backed, "SpatRaster")
})

test_that("hsi_calc_reflectance errors when the file exists and overwrite is FALSE", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      in_memory = TRUE,
      filename = temp_file,
      overwrite = FALSE
    )
  )
})

# Input validation ----

test_that("hsi_calc_reflectance validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_reflectance,
    list(x = test_x, whiteref = test_whiteref, darkref = test_darkref)
  )
})

test_that("hsi_calc_reflectance rejects non-SpatRaster inputs", {
  expect_error(
    hsi_calc_reflectance(
      x = "not a raster",
      whiteref = test_whiteref,
      darkref = test_darkref
    )
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = "not a raster",
      darkref = test_darkref
    )
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = "not a raster"
    )
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      darkspec = "not a raster"
    )
  )
})

test_that("hsi_calc_reflectance rejects band-count mismatches", {
  whiteref_short <- terra::subset(test_whiteref, 1:10)
  darkspec_short <- terra::subset(test_darkref, 1:10)

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = whiteref_short,
      darkref = test_darkref
    ),
    "same number of bands",
    class = "hsitools_error"
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      darkspec = darkspec_short
    ),
    "same number of bands",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_reflectance rejects invalid tint", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(1, 1, 1)
    )
  )

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(0, 1)
    )
  )
})

test_that("hsi_calc_reflectance rejects non-numeric band names", {
  x_bad_names <- terra::deepcopy(test_x)
  names(x_bad_names) <- paste0("band_", seq_len(terra::nlyr(x_bad_names)))

  expect_error(
    hsi_calc_reflectance(
      x = x_bad_names,
      whiteref = test_whiteref,
      darkref = test_darkref
    ),
    "numeric wavelengths",
    class = "hsitools_error"
  )
})

test_that("hsi_calc_reflectance errors when an unused argument is passed without filename", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      bogus_arg = 1
    ),
    "not used",
    class = "hsitools_error"
  )
})

# Error messages ----
# Dev/CI-only message-quality layer; pins the wording of every abort authored
# in this function. check_* helper errors are covered by their own tests.

test_that("hsi_calc_reflectance error messages are informative", {
  whiteref_short <- terra::subset(test_whiteref, 1:10)
  darkspec_short <- terra::subset(test_darkref, 1:10)
  x_bad_names <- terra::deepcopy(test_x)
  names(x_bad_names) <- paste0("band_", seq_len(terra::nlyr(x_bad_names)))

  expect_snapshot(
    error = TRUE,
    hsi_calc_reflectance(
      x = test_x,
      whiteref = whiteref_short,
      darkref = test_darkref
    )
  )

  expect_snapshot(
    error = TRUE,
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      darkspec = darkspec_short
    )
  )

  expect_snapshot(
    error = TRUE,
    hsi_calc_reflectance(
      x = x_bad_names,
      whiteref = test_whiteref,
      darkref = test_darkref
    )
  )
})
