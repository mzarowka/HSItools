# Test hsi_find_extent ----
# hsi_find_extent() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

## Setup ----
# Two points spanning part of the fixture's vertical extent (1000-1009,
# 2000-2009), centered so a modest width stays inside the raster bounds.
test_points <- terra::vect(
  data.frame(x = c(1004.5, 1004.5), y = c(2002, 2007)),
  geom = c("x", "y"),
  crs = terra::crs(test_reflectance)
)

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_find_extent validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_find_extent,
    list(x = test_reflectance, points = test_points, width = 4)
  )
})
