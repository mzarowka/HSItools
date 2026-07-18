# Test hsi_mask ----
# hsi_mask() applies a single-layer mask to a hyperspectral SpatRaster via
# terra::mask(). Contract (ratified 2026-07-17):
#   inverse = FALSE (keep-mask): nonzero kept, 0 dropped, NA dropped
#   inverse = TRUE  (bad-mask) : nonzero dropped, 0 kept, NA dropped
# Dropped cells become NA across every layer. NA always drops regardless of
# inverse — a deliberate deviation from raw terra::mask(inverse = TRUE), which
# keeps NA cells. A single mask layer is recycled across all layers.
# Note: GeoTIFF round-trips NA as NaN, so file read-back is asserted on the
# is.na() pattern, never against literal NA.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Single-layer mask derived from the fixture with a known 3-state pattern over
# the 81 cells: cells 1-40 nonzero, 41-70 zero, 71-81 NA. Cell ranges are named
# so assertions read against the contract, not magic numbers.
keep_cells <- 1:40
zero_cells <- 41:70
na_cells <- 71:81

test_mask <- terra::setValues(
  terra::subset(test_reflectance, 1),
  c(
    rep(1, length(keep_cells)),
    rep(0, length(zero_cells)),
    rep(NA_real_, length(na_cells))
  )
)

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_mask returns a SpatRaster", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask)

  expect_s4_class(result, "SpatRaster")
})

# ── Output dimensions ────────────────────────────────────────────────────────

test_that("hsi_mask preserves spatial dimensions and layer count", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask)

  expect_equal(terra::nrow(result), terra::nrow(test_reflectance))
  expect_equal(terra::ncol(result), terra::ncol(test_reflectance))
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
})

# ── Band names ───────────────────────────────────────────────────────────────

test_that("hsi_mask preserves band names", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask)

  expect_equal(terra::names(result), terra::names(test_reflectance))
})

# ── Value sanity ─────────────────────────────────────────────────────────────

test_that("hsi_mask keep-mask keeps nonzero cells and drops 0 and NA cells", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask)
  masked_na <- is.na(terra::values(result)[, 1])

  # nonzero kept, 0 dropped, NA dropped
  expect_false(any(masked_na[keep_cells]))
  expect_true(all(masked_na[zero_cells]))
  expect_true(all(masked_na[na_cells]))

  # surviving cells carry the original values unchanged
  expect_equal(
    terra::values(result)[keep_cells, 1],
    terra::values(test_reflectance)[keep_cells, 1]
  )
})

test_that("hsi_mask bad-mask drops nonzero cells and keeps 0 cells", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask, inverse = TRUE)
  masked_na <- is.na(terra::values(result)[, 1])

  # nonzero dropped, 0 kept, NA dropped — the complement of the keep-mask
  expect_true(all(masked_na[keep_cells]))
  expect_false(any(masked_na[zero_cells]))
  expect_true(all(masked_na[na_cells]))
})

test_that("hsi_mask drops NA cells regardless of inverse", {
  # The deliberate deviation from terra::mask(inverse = TRUE): NA cells are
  # dropped on both branches. Pinned independently so a refactor toward raw
  # terra semantics fails loudly here.
  keep <- hsi_mask(x = test_reflectance, mask = test_mask)
  bad <- hsi_mask(x = test_reflectance, mask = test_mask, inverse = TRUE)

  expect_true(all(is.na(terra::values(keep)[na_cells, 1])))
  expect_true(all(is.na(terra::values(bad)[na_cells, 1])))
})

test_that("hsi_mask recycles a single mask layer across all layers", {
  result <- hsi_mask(x = test_reflectance, mask = test_mask)

  # Every layer must share the same NA pattern (30 zero + 11 NA cells = 41).
  na_per_layer <- purrr::map_int(
    seq_len(terra::nlyr(result)),
    \(i) sum(is.na(terra::values(result)[, i]))
  )

  expect_length(unique(na_per_layer), 1)
  expect_equal(unique(na_per_layer), length(zero_cells) + length(na_cells))
})

# ── File writing ─────────────────────────────────────────────────────────────

test_that("hsi_mask writes to file when filename provided", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  result <- hsi_mask(
    x = test_reflectance,
    mask = test_mask,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")

  # NA round-trips through GeoTIFF as NaN; assert the is.na() pattern, not NA.
  written_na <- is.na(terra::values(terra::rast(temp_file))[, 1])
  expect_false(any(written_na[keep_cells]))
  expect_true(all(written_na[c(zero_cells, na_cells)]))
})

test_that("hsi_mask errors when file exists and overwrite = FALSE", {
  temp_file <- withr::local_tempfile(fileext = ".tif")

  hsi_mask(
    x = test_reflectance,
    mask = test_mask,
    filename = temp_file,
    overwrite = TRUE
  )

  expect_error(
    hsi_mask(
      x = test_reflectance,
      mask = test_mask,
      filename = temp_file,
      overwrite = FALSE
    )
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_mask errors with non-SpatRaster x or mask", {
  expect_error(hsi_mask(x = "not a raster", mask = test_mask))
  expect_error(hsi_mask(x = test_reflectance, mask = "not a raster"))
})

test_that("hsi_mask errors when mask has more than one layer", {
  multi_mask <- c(test_mask, test_mask)

  expect_error(
    hsi_mask(x = test_reflectance, mask = multi_mask),
    "exactly 1 layer",
    class = "hsitools_error"
  )
})

test_that("hsi_mask errors when inverse is not a logical scalar", {
  expect_error(
    hsi_mask(x = test_reflectance, mask = test_mask, inverse = NA),
    class = "rlang_error"
  )
})

test_that("hsi_mask validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_mask,
    list(x = test_reflectance, mask = test_mask)
  )
})

# ── Error messages ───────────────────────────────────────────────────────────
# Dev/CI-only message-quality layer for the single authored cli_abort().
# check_spatraster and rlang type-check errors are helper errors, excluded from
# per-function snapshots (precedent: test-hsi_calc_reflectance.R).

test_that("hsi_mask error messages match snapshot", {
  multi_mask <- c(test_mask, test_mask)

  expect_snapshot(
    hsi_mask(x = test_reflectance, mask = multi_mask),
    error = TRUE
  )
})
