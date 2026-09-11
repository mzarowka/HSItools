# Test hsi_destripe ----
# hsi_destripe() has no full test suite yet — a pre-existing gap, not
# introduced by this sweep. This file currently covers only the write-tail
# validation added in the 2026-07-17 sweep (dev-notes/2026-07-17_handoff-
# write-tail-validation-sonnet.md); full §5 coverage (8-12 tests) remains a
# separate, pre-existing gap.

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_destripe validates filename and overwrite", {
  expect_write_tail_validated(
    hsi_destripe,
    list(x = test_reflectance)
  )
})
