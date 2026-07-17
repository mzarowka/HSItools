# Test hsi_apply_mnf ----
# hsi_apply_mnf() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

## Setup ----
skip_if_not_installed("spacetime")

test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# Subset to 8 bands so ncell (81) > nlyr (8), matching hsi_calc_mnf's own
# fixture requirement.
test_8band <- terra::subset(test_reflectance, 1:8)
test_fit <- hsi_calc_mnf(x = test_8band)

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_apply_mnf validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_apply_mnf,
    list(x = test_8band, fit = test_fit, n = 3)
  )
})
