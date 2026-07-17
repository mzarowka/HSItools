# Test relative absorption band area calculation ----
# hsi_calc_raba() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

## Setup ----
test_reflectance <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/REFLECTANCE_testdata.tif"
  )
)

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_calc_raba validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_calc_raba,
    list(x = test_reflectance, continuum_edges = c(650, 700))
  )
})
