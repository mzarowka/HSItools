# Test hsi_calc_reflectance ----
# Converts raw DN triplet (x, whiteref, darkref) to calibrated reflectance.
# Key contracts:
#   - Output dimensions and band names match x
#   - in_memory = TRUE and FALSE produce equivalent values
#   - tint argument affects output (integration time scaling is exercised)
#   - All three SpatRaster inputs are validated independently

## Setup ----
test_x <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/capture/testdata.tif"
  )
)

test_whiteref <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/capture/WHITEREF_testdata.tif"
  )
)

test_darkref <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/capture/DARKREF_testdata.tif"
  )
)

test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance returns a SpatRaster", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance preserves number of layers", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(terra::nlyr(result), terra::nlyr(test_x))
})

test_that("hsi_calc_reflectance preserves spatial dimensions", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(terra::nrow(result), terra::nrow(test_x))
  expect_equal(terra::ncol(result), terra::ncol(test_x))
})

# ── Band names ───────────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance preserves band names from x", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_equal(names(result), names(test_x))
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance matches reference fixture", {
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

test_that("hsi_calc_reflectance tint argument affects output values", {
  # tint = c(2, 1) scales the dark reference differently for the white
  # reference denominator; result must differ from the default c(1, 1)
  result_default <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    tint = c(1, 1),
    in_memory = TRUE
  )

  result_scaled <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    tint = c(2, 1),
    in_memory = TRUE
  )

  expect_false(
    isTRUE(all.equal(
      terra::values(result_default),
      terra::values(result_scaled)
    ))
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

# ── Memory and file handling ──────────────────────────────────────────────────
# Exercises all four combinations of in_memory and filename.

test_that("hsi_calc_reflectance in_memory = TRUE, filename = '' returns SpatRaster", {
  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  expect_s4_class(result, "SpatRaster")
})

test_that("hsi_calc_reflectance in_memory = TRUE, filename provided writes file and returns SpatRaster", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(temp_file))

  unlink(temp_file)
})

test_that("hsi_calc_reflectance in_memory = FALSE, filename provided writes file and returns SpatRaster", {
  # withr::local_tempdir() manages per-band intermediates; they are cleaned
  # up automatically when the function exits because the final result is
  # committed to filename before exit.
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = FALSE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(temp_file))

  unlink(temp_file)
})

test_that("hsi_calc_reflectance in_memory = FALSE, filename = '' warns and returns SpatRaster", {
  # Per-band temp files are the backing store of the returned SpatRaster
  # and cannot be cleaned up automatically — a warning is emitted.
  expect_warning(
    result <- hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      in_memory = FALSE
    ),
    "Temporary files will not be cleaned up"
  )

  expect_s4_class(result, "SpatRaster")
})

test_that("hsi_calc_reflectance in_memory = TRUE and FALSE produce equivalent values", {
  result_mem <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE
  )

  temp_file <- tempfile(fileext = ".tif")

  result_disk <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = FALSE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_equal(
    terra::values(result_mem),
    terra::values(result_disk),
    tolerance = 1e-6
  )

  unlink(temp_file)
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_calc_reflectance(
    x = test_x,
    whiteref = test_whiteref,
    darkref = test_darkref,
    in_memory = TRUE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_calc_reflectance errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

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

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_reflectance errors with non-SpatRaster x", {
  expect_error(
    hsi_calc_reflectance(
      x = "not a raster",
      whiteref = test_whiteref,
      darkref = test_darkref
    )
  )
})

test_that("hsi_calc_reflectance errors with non-SpatRaster whiteref", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = "not a raster",
      darkref = test_darkref
    )
  )
})

test_that("hsi_calc_reflectance errors with non-SpatRaster darkref", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = "not a raster"
    )
  )
})

test_that("hsi_calc_reflectance errors when band counts differ", {
  # Subset whiteref to fewer bands than x
  whiteref_short <- terra::subset(test_whiteref, 1:10)

  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = whiteref_short,
      darkref = test_darkref
    ),
    "same number of bands"
  )
})

test_that("hsi_calc_reflectance errors when tint is wrong length", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(1, 1, 1)
    )
  )
})

test_that("hsi_calc_reflectance errors when tint contains zero", {
  expect_error(
    hsi_calc_reflectance(
      x = test_x,
      whiteref = test_whiteref,
      darkref = test_darkref,
      tint = c(0, 1)
    )
  )
})
