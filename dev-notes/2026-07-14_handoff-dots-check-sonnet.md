# Handoff: `check_dots_write()` — the `...` sink fix, design locked, ready for sweep

**Date:** 2026-07-14
**From:** Fable (design with Maury, this session; empirical probes by Opus/Fable, same session)
**To:** Sonnet (implementation sweep)
**Scope:** One new internal helper in HSItools + a mechanical sweep of every Shape A function
in HSItools and zarowka. Convention-level change; CLAUDE.md amendment itself is Maury's, not
yours. Background: `zarowka/dev-notes/2026-07-14_dots-sink-hazard.md` (read it, especially the
addendum — the original §4–§5 recommendation there is withdrawn; the addendum is the design).

---

## 1. The problem, in two sentences

Every Shape A function ends with `filename = "", overwrite = FALSE, ...` and splices `...`
into terra write options. When `filename == ""` (the default), `wopt` is never read, so **any
argument the function does not have — typo, removed argument, sibling's argument — silently
does nothing.** This defeated zarowka's test suite for three weeks (details in the dev-note).

## 2. Verified facts — do not re-derive, do not contradict

Probed on terra 1.9.34, Windows, 2026-07-14:

- `terra::writeRaster(..., wopt = list(names = "a", method = "fcls"))` →
  **`ERROR: [write] unknown option(s): method`** — a hard error, not a warning.
- Same for typo'd real names (`datatpye`).

So the **write path (`filename != ""`) is already loud** — terra validates `wopt` itself.
The **only** silent path is `filename == ""`. The fix therefore needs no allow-list of valid
wopt names (that idea is withdrawn — it duplicates terra, couples to terra versions, and
misses valid-but-pointless options like `datatype` with no filename).

Also verified: `rlang::check_dots_used()` cannot work here — `rlang::list2(...)` forces every
dot, marking them all "used", so it would never fire. Do not propose it.

## 3. Locked design (do not reopen)

| Decision | Locked value |
|---|---|
| The check | Abort when `filename == ""` **and** `...` is non-empty. Structural, not vocabulary: with no filename, nothing in `...` can have any effect. |
| Helper name | `check_dots_write()` — confirm with Maury before coding (§6), but this is the proposed default. |
| Home | `HSItools/R/utils-checks.R`, alongside the other `check_*` helpers. `@noRd`, documented for developers per §4.7. |
| Conventions | Standard §3.3: takes `call = rlang::caller_env()`, threads `call` into the abort, `class = "hsitools_error"` hardwired. No `arg` parameter (the message names the offending dots itself). |
| Input shape | Takes the **already-evaluated list** (`wopt_user`) plus `filename` — not raw `...`. |
| Write path | Untouched. terra's own validation covers it. Accepted residual cost: a write-path typo errors late (after compute), terra-voiced. Deliberately deferred — reopen only if it bites in practice. |
| Message | As pinned below. Wording tweaks allowed only if cli pluralization forces them; substance fixed. |

Helper body (reference implementation — adjust only for house formatting):

```r
check_dots_write <- function(
  dots,
  filename,
  call = rlang::caller_env()
) {
  if (filename == "" && length(dots) > 0) {
    cli::cli_abort(
      c(
        "Argument{?s} {.arg {names(dots)}} {?was/were} not used.",
        "i" = "{.arg ...} holds write options for {.fn terra::writeRaster}; without {.arg filename} they have no effect.",
        "i" = "Check for misspelled or removed argument names."
      ),
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(dots)
}
```

Edge to handle gracefully: unnamed elements in `dots` (`hsi_calc_x(x, "stray")`) must still
abort; if `names(dots)` contains `""` and the message renders badly, substitute a placeholder
rather than special-casing the logic. Show Maury the rendered message in the first iteration.

## 4. Call-site placement (locked — this is the part a sweep gets inconsistent)

The check must fire **before computation** (fail fast is the entire point), so
`rlang::list2(...)` moves **up into the validation block**. Canonical shape after the sweep:

```r
  # Validate inputs
  check_spatraster(x)
  ...other checks...
  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)

  ...computation...

  # Build write options
  wopt_default <- list(names = index_name)
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)
```

The `# Build write options` block keeps `wopt_default` and the merge; only the `list2()` line
moves. This deviates from the current §3.4 snippet in CLAUDE.md — that amendment is **Maury's
edit, after the sweep proves out**. Never touch CLAUDE.md yourself.

## 5. Sweep procedure

1. **Baseline first.** Run the full HSItools suite before changing anything and record the
   result — it has not been run in the design session; do not assume green. zarowka baseline
   as of 2026-07-14: `FAIL 0 | WARN 1 | SKIP 0 | PASS 117` (the WARN is unmixR-internal,
   known, not ours).
2. **Enumerate, then confirm.** Inventory criterion: any function with `filename = ""` default
   **and** `...` forwarded to `terra::writeRaster()`. Grep for the `rlang::list2(...)`/`wopt`
   pattern across both `R/` directories. Excluded by construction: Shape B functions
   (`hsi_write_scaled` — always writes, terra validates), functions without `...`
   (`hsi_calc_snr`, `hsi_calc_sam`, extraction functions). Watch `hsi_tiled()` — if its dots
   serve any purpose besides writeRaster, exclude it and flag. Present the list to Maury
   before edit #1.
3. **Helper + one simple function first** (suggest `hsi_calc_ndi`), with its test. Show
   `devtools::test()` output, wait for go/no-go.
4. **Then batches**, grouped by family (indices, smoothing, calibration…), full suite run per
   batch, go/no-go between batches. Batch size is Maury's call (§6).
5. **zarowka last** (or per §6 deferred): `hsi_calc_abundance()` and `hsi_check_saturation()`
   get the same line via `HSItools:::check_dots_write(...)` — precedent for `:::` into
   HSItools internals already exists in zarowka and is sanctioned (Maury, 2026-07-14: zarowka
   never goes to CRAN). **Sequencing constraint:** zarowka tests run against the *installed*
   HSItools, so Maury must reinstall HSItools locally after the helper lands and before the
   zarowka iteration.

## 6. Micro-decisions to confirm with Maury before coding

- Helper name: `check_dots_write` (proposed) vs `check_dots_wopt` vs other.
- Batch size for the sweep (one function per iteration vs family batches).
- zarowka in the same sweep or deferred to the next promotion.

## 7. Tests

- Per swept function, **one** new test in its existing file: unknown argument with no filename
  aborts. `expect_error(hsi_calc_x(x, ..., bogus_arg = 1), "not used", class = "hsitools_error")`
  — class assertion riding a message assertion, per §5.6. Section: `Input validation`.
- **No new snapshots.** The abort lives in a shared `check_*` helper; per the established
  pattern (see the "Error messages" comment block in `test-hsi_calc_reflectance.R`), helper
  errors are excluded from per-function snapshot suites, and `check_*` helpers are not tested
  directly (§5.8).
- The two file-writing tests per function (§5.5) already pass `filename` and are unaffected.

## 8. Things that may break — stop and show Maury, do not improvise

- Any existing test, example, vignette, or zarowka template that passes a write option
  **without** a filename relied on the sink and will now abort. Grep examples/vignettes/
  templates for wopt-ish names (`datatype`, `gdal`, `NAflag`, `progress`, `memfrac`) before
  the sweep; a hit is a finding for Maury, not something to silently fix.
- If any function's `...` turns out to serve a non-writeRaster purpose, it contradicts §3.1 —
  flag it, exclude it from the sweep, do not redesign it.

## 9. Out of scope

- CLAUDE.md §3.2/§3.4/§3.5 amendments — Maury's, after the sweep.
- The write-path late-error ergonomics (allow-list) — deliberately deferred, evidence-gated.
- Deprecation shims for `method`/`index_name` on `hsi_calc_abundance()` — rejected in the
  dev-note; the generic check covers them.
- Git operations of any kind — Maury commits (§0 rule 8). Note both repos currently carry
  uncommitted work from 2026-07-14 (SNR/saturation tests, suite repairs, DESCRIPTION edits).

## 10. Suggested first move

Read the dev-note + addendum, run the HSItools baseline suite, produce the sweep inventory,
put the §6 micro-decisions to Maury in one message. Then helper + `hsi_calc_ndi` + its test as
iteration one.
