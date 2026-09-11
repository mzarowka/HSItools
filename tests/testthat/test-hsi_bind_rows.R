# Test hsi_bind_rows ----
# hsi_bind_rows() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_bind_rows validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_bind_rows,
    list(x = list(test_reflectance, test_reflectance))
  )
})
