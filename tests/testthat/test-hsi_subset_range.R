# Test hsi_subset_range ----
# Extracts all bands whose wavelength falls within [from, to] (inclusive).

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_subset_range returns a SpatRaster", {
  result <- hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700
  )

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_subset_range returns fewer bands than the input", {
  result <- hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700
  )

  expect_lt(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_subset_range preserves spatial dimensions", {
  result <- hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700
  )

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
})

# ── Range correctness ────────────────────────────────────────────────────────

test_that("hsi_subset_range output band names all fall within [from, to]", {
  from <- 600
  to <- 700

  result <- hsi_subset_range(
    x = test_reflectance,
    from = from,
    to = to
  )

  result_wavelengths <- as.numeric(terra::names(result))

  expect_true(all(result_wavelengths >= from))
  expect_true(all(result_wavelengths <= to))
})

test_that("hsi_subset_range with full input range returns all bands", {
  all_wavelengths <- as.numeric(terra::names(test_reflectance))
  from <- min(all_wavelengths)
  to <- max(all_wavelengths)

  result <- hsi_subset_range(
    x = test_reflectance,
    from = from,
    to = to
  )

  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

test_that("hsi_subset_range output bands are a proper subset of the input bands", {
  result <- hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700
  )

  input_wavelengths <- as.numeric(terra::names(test_reflectance))
  result_wavelengths <- as.numeric(terra::names(result))

  expect_true(all(result_wavelengths %in% input_wavelengths))
})

test_that("hsi_subset_range with from == to returns at least 1 band", {
  # Nearest-band matching means a single wavelength still yields a result
  all_wavelengths <- as.numeric(terra::names(test_reflectance))
  single_wl <- all_wavelengths[ceiling(length(all_wavelengths) / 2)]

  result <- hsi_subset_range(
    x = test_reflectance,
    from = single_wl,
    to = single_wl
  )

  expect_gte(terra::nlyr(result), 1)
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_subset_range writes to file when filename provided", {
  temp_file <- tempfile(fileext = ".tif")

  result <- hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("hsi_subset_range errors when file exists and overwrite = FALSE", {
  temp_file <- tempfile(fileext = ".tif")

  hsi_subset_range(
    x = test_reflectance,
    from = 600,
    to = 700,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_subset_range(
      x = test_reflectance,
      from = 600,
      to = 700,
      filename = temp_file,
      overwrite = FALSE
    )
  )

  unlink(temp_file)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_subset_range errors with non-SpatRaster input", {
  expect_error(
    hsi_subset_range(x = "not a raster", from = 600, to = 700)
  )
})

test_that("hsi_subset_range errors when from is not a single numeric", {
  expect_error(
    hsi_subset_range(x = test_reflectance, from = c(600, 620), to = 700)
  )
})

test_that("hsi_subset_range errors when to is not a single numeric", {
  expect_error(
    hsi_subset_range(x = test_reflectance, from = 600, to = c(700, 720))
  )
})

test_that("hsi_subset_range is order-tolerant: from > to gives same result as from < to", {
  # Implementation uses min/max internally so argument order should not matter
  result_fwd <- hsi_subset_range(x = test_reflectance, from = 600, to = 700)
  result_rev <- hsi_subset_range(x = test_reflectance, from = 700, to = 600)

  expect_equal(terra::names(result_fwd), terra::names(result_rev))
  expect_equal(terra::nlyr(result_fwd), terra::nlyr(result_rev))
})
