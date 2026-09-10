# Test hsi_check_saturation ----
# Marks pixels reaching the sensor's saturation threshold, returning a 0/1
# SpatRaster: one layer per band, or a single collapsed layer.
# Key contracts:
#   - Output is a SpatRaster of 0/1 values, NA preserved
#   - collapse = FALSE: one layer per band, band names of x preserved
#   - collapse = TRUE: a single layer named "saturated"
#   - The comparison is inclusive: a pixel at exactly `limit` is saturated
#   - NA propagates through the collapse rather than reading as unsaturated
#   - `limit` is required and never inferred
#   - Writes to file when filename is given

## Setup ----
# Raw digital numbers: the processing level this function is meant for.
test_capture <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/capture/testdata.tif"
  )
)

# A threshold inside the observed DN range, so some pixels genuinely saturate.
test_limit <- 19000

# Derived rasters are built with terra::setValues(), which returns a new raster.
# SpatRaster is not deep-copied by `<-`, so mutating in place would corrupt the
# shared fixture.

# One pixel unsaturated in every band except band 2, which is NA. Collapsing must
# not report it clean.
na_values <- terra::values(test_capture)
na_values[1, ] <- 0
na_values[1, 2] <- NA_real_
test_na <- terra::setValues(test_capture, na_values)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_check_saturation returns a SpatRaster", {
  result <- hsi_check_saturation(x = test_capture, limit = test_limit)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_check_saturation returns one layer per band by default", {
  result <- hsi_check_saturation(x = test_capture, limit = test_limit)

  expect_equal(terra::nlyr(result), terra::nlyr(test_capture))
  expect_equal(terra::nrow(result), terra::nrow(test_capture))
  expect_equal(terra::ncol(result), terra::ncol(test_capture))
})

test_that("hsi_check_saturation collapse reduces the mask to a single layer", {
  result <- hsi_check_saturation(
    x = test_capture,
    limit = test_limit,
    collapse = TRUE
  )

  expect_equal(terra::nlyr(result), 1L)
})

# ── Band names ───────────────────────────────────────────────────────────────

test_that("hsi_check_saturation preserves the band names of x", {
  result <- hsi_check_saturation(x = test_capture, limit = test_limit)

  expect_equal(terra::names(result), terra::names(test_capture))
})

test_that("hsi_check_saturation names the collapsed layer saturated", {
  result <- hsi_check_saturation(
    x = test_capture,
    limit = test_limit,
    collapse = TRUE
  )

  expect_equal(terra::names(result), "saturated")
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_check_saturation returns only 0, 1 and NA", {
  result <- hsi_check_saturation(x = test_capture, limit = test_limit)

  expect_true(all(terra::values(result) %in% c(0, 1, NA)))
})

test_that("hsi_check_saturation marks exactly the pixels reaching the limit", {
  result <- hsi_check_saturation(x = test_capture, limit = test_limit)

  expected <- terra::values(test_capture) >= test_limit

  expect_equal(as.vector(terra::values(result)) == 1, as.vector(expected))
})

test_that("hsi_check_saturation marks nothing when no pixel reaches the limit", {
  unreachable <- max(terra::minmax(test_capture)[2, ]) + 1

  result <- hsi_check_saturation(x = test_capture, limit = unreachable)

  expect_equal(sum(terra::values(result)), 0)
})

test_that("hsi_check_saturation treats a pixel at exactly the limit as saturated", {
  # The comparison is inclusive: hitting the ceiling counts.
  exact <- min(terra::minmax(test_capture)[2, ])

  result <- hsi_check_saturation(x = test_capture, limit = exact)

  expect_true(sum(terra::values(result)) > 0)
})

# ── NA handling ──────────────────────────────────────────────────────────────

test_that("hsi_check_saturation collapse does not report an NA pixel as unsaturated", {
  # terra's any() propagates NA rather than letting a TRUE override it, so a
  # pixel with any unknown band collapses to NA — never to 0.
  result <- hsi_check_saturation(
    x = test_na,
    limit = test_limit,
    collapse = TRUE
  )

  expect_true(is.na(terra::values(result)[1]))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_check_saturation writes to file when filename provided", {
  temp_file <- file.path(tempdir(), "test_saturation.tif")
  on.exit(unlink(temp_file), add = TRUE)

  result <- hsi_check_saturation(
    x = test_capture,
    limit = test_limit,
    collapse = TRUE,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::names(terra::rast(temp_file)), "saturated")
})

test_that("hsi_check_saturation errors when file exists and overwrite is FALSE", {
  temp_file <- file.path(tempdir(), "test_saturation_overwrite.tif")
  on.exit(unlink(temp_file), add = TRUE)

  hsi_check_saturation(
    x = test_capture,
    limit = test_limit,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_check_saturation(
      x = test_capture,
      limit = test_limit,
      filename = temp_file,
      overwrite = FALSE
    )
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_check_saturation validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_check_saturation,
    list(x = test_capture, limit = test_limit)
  )
})

test_that("hsi_check_saturation errors with non-SpatRaster input", {
  expect_error(
    hsi_check_saturation(x = "not a raster", limit = test_limit)
  )
})

test_that("hsi_check_saturation errors when limit is absent", {
  # The saturation threshold is instrument knowledge and is never inferred.
  expect_error(
    hsi_check_saturation(x = test_capture),
    "absent",
    class = "rlang_error"
  )
})

test_that("hsi_check_saturation errors when limit is not a single number", {
  expect_error(
    hsi_check_saturation(x = test_capture, limit = c(1, 2)),
    "length 1",
    class = "hsitools_error"
  )
})

test_that("hsi_check_saturation errors when collapse is not a logical scalar", {
  expect_error(
    hsi_check_saturation(
      x = test_capture,
      limit = test_limit,
      collapse = "yes"
    ),
    class = "rlang_error"
  )
})

test_that("hsi_check_saturation errors when an unused argument is passed without filename", {
  expect_error(
    hsi_check_saturation(
      x = test_capture,
      limit = test_limit,
      bogus_arg = 1
    ),
    "not used",
    class = "hsitools_error"
  )
})
