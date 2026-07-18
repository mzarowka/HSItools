# Handoff: D2b — hsi_calc_mnf() valid-pixel guard

**Date:** 2026-07-18
**From:** Fable (design agreed with Maury, same day: "we should fix")
**To:** Opus (execution)
**Scope:** **One change**: a valid-pixel guard in `R/hsi_calc_mnf.R` plus its
test. Nothing else. Evidence and motivation: the D2b table in
`dev-notes/2026-07-18_handoff-mask-closeout-opus.md` ("### D2b — separate
finding") — read it first.

---

## 1. The defect, in two sentences

`hsi_calc_mnf()` guards pixel count with `terra::ncell(x) <= lyrs`, which
counts **masked (NA) cells too**, and `spacetime:::mnf.matrix`'s own
rank-deficiency warning likewise sees full row count. A heavily masked raster
therefore passes every guard and returns **silently degenerate eigenvalues**
(probed 2026-07-18: 50 valid pixels → 0.45–1.63 on a true ≈1.0; 9 valid →
3.3e-16; first hard error only at 6).

## 2. Locked design (agreed with Maury — do not reopen)

- **Build the matrix once.** Currently both branches of the trim conditional
  call `spacetime::mnf(terra::as.matrix(...))` inline. Restructure: apply the
  trim subset if `trim != 0L`, then `mat <- terra::as.matrix(x)`, then the new
  guard, then `result <- spacetime::mnf(mat, ...)`. One code path, matrix
  materialized exactly once (the function already materializes it — this adds
  zero memory cost).
- **The guard:** abort when `sum(stats::complete.cases(mat)) <= lyrs`.
  Same mathematical bound as the existing ncell check, applied to *valid*
  rows. `class = "hsitools_error"`. Message shape (final wording at
  implementation; show Maury the rendered text):

  ```r
  cli::cli_abort(
    c(
      "Not enough valid (non-{.val NA}) pixels in {.arg x}.",
      "i" = "{.val {n_valid}} valid pixel{?s} for {.val {lyrs}} band{?s}; MNF needs more valid pixels than bands.",
      "i" = "Masked or cropped input? Check the mask coverage before computing MNF."
    ),
    class = "hsitools_error"
  )
  ```

- **The existing `ncell` check stays** — it is the cheap pre-read structural
  guard in the validation block; the new check is a data-quality guard that
  can only run after the read. Two checks, different jobs. The new one sits
  immediately after `mat` is built, *not* in the validation block — a
  data-dependent check cannot precede the data (accepted placement; same
  category as `hsi_write_scaled`'s range check).
- **Deliberately NOT included** (decided, do not add): a warning for the
  silently-distorted zone (valid pixels between ~1× and ~6× band count). Any
  threshold multiple would be an invented number — speculative generalization.
  The hard bound only.
- **No roxygen changes.** The error is self-explanatory; `@details` untouched,
  so no `devtools::document()` churn expected. If you believe a doc sentence
  is warranted, propose it in your report — do not add it.

## 3. Tests

`tests/testthat/test-hsi_calc_mnf.R` (exists, full suite, **no snapshot
block** — verified 2026-07-18, and its other authored aborts are asserted via
message/class only). Follow that file's precedent exactly:

- **One new test** in its Input validation section: a raster masked down to
  ≤ `nlyr` valid pixels errors with a stable fragment (`"valid pixel"` or
  similar) and `class = "hsitools_error"` riding it (§5.6). Build the sparse
  input from the file's existing `test_8band` fixture via `terra::setValues()`
  — e.g. NA out all but 5 cells. `spacetime` skip-guard applies as in the rest
  of the file.
- **No snapshot** — the file has none; adding a full §5.6 message-quality
  block for all its aborts is a separate backlog item, not this change.
- Sanity: the existing tests (unmasked fixtures) must be unaffected — the new
  guard only fires when valid ≤ bands.

## 4. Procedure

1. Baseline: `Rscript -e "devtools::test()"` — expect the post-commit
   baseline (560 + any drift; record what you see).
2. The one change + test, `air format` on both files.
3. Full suite, show output, show the rendered error message (trigger it once
   in a scratchpad script — file-based, never `Rscript -e` one-liners; they
   segfault on this machine).
4. Stop. Maury reviews wording and commits (§0 rule 8).

## 5. Out of scope

- The distortion-zone warning (decided against), `hsi_apply_mnf`, the
  snapshot backlog for this file, templates (deferred by Maury 2026-07-18),
  CLAUDE.md (already updated to 1.8.1 by Fable — hands off), git.
