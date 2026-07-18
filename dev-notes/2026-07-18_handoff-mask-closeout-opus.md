# Handoff: hsi_mask close-out — test file + downstream NA-tolerance checks

> **CLOSED 2026-07-18.** Item 1 (test file + snapshot) and item 2 (NA-tolerance
> pass) complete and committed. All findings resolved: D1 fixed (savgol roxygen),
> D2 closed benign (probe handoff `..._d2-mnf-mask-probe-opus.md`; recorded in
> CLAUDE.md 1.8.1 §7), D2b fix handed off (`..._handoff-d2b-mnf-guard-opus.md`),
> D3 fixed (comment), D4 resolved (reinstall). **Item 3 (masking in templates):
> DEFERRED by Maury** — his call, with the observation that the template set has
> "grown a little too much and got spread too much"; a template consolidation
> look should precede any masking additions. The three-stage design sketch
> (saturation mask in `reflectance_*`, nearest-neighbour warp in
> `coregister_swir`, combine-and-apply in `postprocess_*` after the transect
> crop) is agreed direction and lives in the 2026-07-18 chat record + the mask
> design note's recipe section — pick it up when templates are revisited.

**Date:** 2026-07-18
**From:** Fable (contract ratified + implementation verified with Maury, 2026-07-17)
**To:** Opus (execution)
**Scope:** Two items, strictly in order: (1) write `test-hsi_mask.R`; (2) run the
downstream NA-tolerance spot-checks and report findings. Both are HSItools-side
except one zarowka check in item 2. A third item (zarowka masking template) is
**deliberately not in this handoff** — its timing is an open question with Maury.
Background you must read first: `dev-notes/2026-07-17_design_hsi-mask-ratified.md`
(the contract, the terra probe table, the verification record, and the two
test-writing gotchas — all normative for this work).

---

## 1. State of play

`hsi_mask()` is ratified, implemented, verified against the truth table on toy and
fixture data, fully documented, committed, and pushed. It is the **only** function
from the 2026-07-17 write-tail sweep without a test file. Do not edit
`R/hsi_mask.R` — your work is a test file and a read-only verification pass.

Contract (normative table in the design note):

```r
hsi_mask(x, mask, inverse = FALSE, filename = "", overwrite = FALSE, ...)
```

| mask cell | `inverse = FALSE` (keep-mask) | `inverse = TRUE` (bad-mask) |
|---|---|---|
| nonzero | keep | drop |
| `0` | drop | keep |
| `NA` | **drop** | **drop** |

Dropped cells become `NA` across all layers. NA-always-drops is a deliberate
deviation from raw `terra::mask()` and is stated in the roxygen `@details`.

## 2. Verified facts — do not re-derive, do not contradict

Probed terra 1.9.34, Windows, 2026-07-17 (evidence in the design note):

- **GeoTIFF round-trip turns `NA` into `NaN`.** `is.na()` is `TRUE` for both, but
  `identical()` / type-strict `expect_equal()` against `NA` **fails** on values
  read back from disk. Every file-writing assertion must test the `is.na()`
  pattern, never compare against literal `NA`.
- **The two branches return different datatypes on integer input** (`inverse =
  FALSE` → double, `inverse = TRUE` → integer). Irrelevant on the float fixture —
  which is exactly why the fixture, not a toy integer raster, is the test
  substrate.
- **A multi-layer mask does not error in terra** — it masks band-by-band. That is
  why the function's single-layer check exists and why it must be tested.
- **Geometry mismatch errors loudly inside terra** (`[mask] number of rows and/or
  columns do not match` — a `simpleError`, not `hsitools_error`). The house
  geometry check is **deferred by decision** (Maury, 2026-07-17). Do **not** add a
  geometry-mismatch test: terra's message is not version-stable, it is terra's
  interface not ours (§5.3), and testing it would harden a deliberately
  unimplemented behaviour.

## 3. Item 1 — `test-hsi_mask.R` (one change, then stop for go/no-go)

Target ~10–12 tests per §5.8; consolidate per §5.3 (one *behaviour* per test,
multiple expectations fine). Sections in §5.1 order.

**Setup.** Load `REFLECTANCE_testdata.tif` (9×9×101, float, 81 cells) via
`system.file()`. Build the mask **from the fixture** per §5.2 — a single-layer
template from `terra::subset(test_reflectance, 1)`, values assigned via
`terra::setValues()` with a known pattern containing all three cell states
(nonzero, `0`, `NA`) at known cell indices. Fixtures are read-only; derive, never
mutate.

**Output type** — returns a `SpatRaster` (standard opener).

**Output dimensions** — nrow/ncol/nlyr preserved (one test, three expectations).

**Band names** — the 101 wavelength names pass through untouched.

**Value sanity** — the heart of the file:

1. *Keep-mask branch*: nonzero cells keep their values, `0` and `NA` cells are
   `NA` — assert the `is.na()` pattern against the known mask indices **and**
   that surviving cells equal the input values.
2. *Bad-mask branch* (`inverse = TRUE`): the complement — nonzero drops, `0`
   survives, `NA` drops.
3. *NA always drops regardless of `inverse`*: its own test, both branches on the
   same mask, the `NA`-cell positions dead in both. This is the deliberate
   terra deviation — it gets pinned independently so a future refactor toward
   raw `terra::mask()` semantics fails loudly.
4. *Single-layer mask recycles across all layers*: per-layer `NA` counts
   identical across all 101 bands (`purrr::map_int`, not a loop).

**File writing** — the §5.5 pair (`writes when filename provided`, `errors when
file exists and overwrite = FALSE`) with `withr::local_tempfile()`. Several older
test files use bare `tempfile()` + `unlink()` — that is a pre-existing §5.5
violation; do **not** copy it. Read-back value assertions follow the `is.na()`
rule from §2.

**Input validation** —

- non-SpatRaster `x`; non-SpatRaster `mask` (bare `expect_error` or with
  `class = "hsitools_error"` riding — they come from `check_spatraster`).
- multi-layer mask: assert a stable message fragment (`"exactly 1 layer"`) with
  `class = "hsitools_error"` riding it (§5.6 — class rides, never drives). The
  realistic regression is `hsi_check_saturation(collapse = FALSE)` output (one
  layer per band) fed straight in; building the bad mask as
  `c(mask, mask)` is fine.
- `inverse = NA`: `class = "rlang_error"` (§3.3 carve-out — never
  `hsitools_error`).
- `expect_write_tail_validated(hsi_mask, list(x = ..., mask = ...))` — the shared
  helper from `tests/testthat/helper-write-tail.R` already exists; `hsi_mask` is
  the only swept function still missing its call.

**Snapshot layer** — one `expect_snapshot(error = TRUE)` exercising the
multi-layer abort, the file's **single** authored `cli_abort()`. The
`check_spatraster` and rlang errors are helper errors — excluded from
per-function snapshots per the precedent recorded in
`test-hsi_calc_reflectance.R`'s comment block. No runtime paths appear in the
message, so no `transform` is needed. The first run creates
`_snaps/hsi_mask.md` silently — run the file **twice** to prove the snapshot
passes on re-run, and tell Maury it needs committing.

**Then:** `air format` the file, run `Rscript -e "devtools::test()"`, show the
output (baseline to beat: 532 pass, 0 fail, 0 warn), and **wait for Maury's
go/no-go before item 2**.

## 4. Item 2 — NA-tolerance spot-checks (run-and-report; no code changes)

The deferred verification half of the 0.6 mask leanings (succession memo §3):
does the downstream pipeline survive masked, NA-rich input? **You are observing,
not fixing.** Any fragility found is a finding for Maury — §0 rules 1/5; do not
patch functions, do not add guards, do not "improve" docs mid-pass.

**Procedure.** Scratchpad script (never in the repo): load the fixture, build a
keep-mask dropping roughly half the 81 pixels, `x_masked <- hsi_mask(x, mask)`,
then run each target and record: error / warning / silent success, and whether
NA propagated sanely (masked pixels NA in output, unmasked pixels unchanged
vs. the unmasked run).

| Target | Notes |
|---|---|
| `hsi_calc_mnf()` | **Named suspect.** Needs `ncell > nlyr` — use the 8-band subset trick from `test-hsi_calc_mnf.R`. `spacetime` must be installed (it is). |
| `hsi_calc_stretch()` | **Named suspect.** Safe call: `type = c(700, 620, 540)` (presets exceed the 517.58–772.19 nm fixture range by design). |
| `hsi_plot_raster()` / `hsi_plot_composite()` | Stretch/plot family — build the ggplot objects; a render check is enough. |
| `hsi_calc_rabd()` | `continuum_edges = c(590, 730)`, `absorption_band = 670`, `index_type = "strict"`. |
| `hsi_calc_rmean()` / `hsi_calc_rsd()` | All-NA pixels under `na.rm = TRUE` — watch for `NaN`/warnings from empty-vector stats. |
| `hsi_smooth_median()` | Focal — watch NA bleed into *neighbouring* unmasked pixels (focal windows straddle the mask edge). |
| `hsi_smooth_savgol()` | Code maps any-NA pixels to all-NA; roxygen says NA pixels "cause the function to fail". **Observe which is true — if the doc is stale, that is a finding, not an edit.** |
| `hsi_remove_continuum()` | Documented NA→NA; confirm. |
| zarowka `hsi_extract_spectra(n = Inf)` | Docs promise "every *valid* pixel" — confirm masked pixels are dropped, row count ≈ unmasked count. |
| zarowka `hsi_calc_snr()` | Per-band stats under `na.rm = TRUE` on a half-masked scene. |

**Deliverable:** a `## Findings — NA tolerance (2026-07-18)` section appended to
**this** handoff: one line per target (behaviour + sane/fragile verdict), plus a
short list of anything needing a Maury decision. Show the raw script output in
chat as well.

## 5. Things that may break — stop and show Maury, do not improvise

- If any truth-table test fails against the committed `hsi_mask()`, **stop
  immediately** — the function was verified 2026-07-17, so a failure means either
  a fixture/test bug or an environment change (terra version). Diagnose which,
  report; never adjust the contract to make a test pass.
- If the snapshot's rendered message reveals a cli pluralization or formatting
  wart, show Maury the rendered text before pinning it.
- Item 2: a crash (not an error — a segfault) anywhere in the chain is a
  first-class finding. Note that inline `Rscript -e` heredocs segfault
  spuriously on this machine — **always run scripts from files** in the
  scratchpad, never `-e` one-liners.

## 6. Out of scope

- `R/hsi_mask.R` itself, its roxygen, and the deferred geometry check.
- The zarowka masking template (item 3 of the close-out plan) — pending a timing
  decision with Maury; do not start it.
- Fixing anything item 2 finds. Findings are decisions for Maury.
- Full §5 suites for the six minimal-file sweep functions — separate backlog.
- CLAUDE.md — never touch (§0 rule 9).
- Git operations — Maury commits (§0 rule 8). Both repos were clean and pushed
  as of 2026-07-17 EOD; expect only your own changes in `git status`.

## 7. First move

Read the design note end to end (contract, probe table, gotchas). Then write
`test-hsi_mask.R` per §3, format, run the suite twice (snapshot proof), show
output, and stop for go/no-go.

---

## Item 1 — DONE (2026-07-18, Opus)

`tests/testthat/test-hsi_mask.R` written: 13 tests / 28 expectations, §5.1
section order. Every assertion pre-confirmed against the fixture in a scratchpad
probe before being written. Suite **560 pass / 0 fail / 0 warn** (was 532).
Snapshot `tests/testthat/_snaps/hsi_mask.md` created on run 1, passed clean on
run 2; rendered message is `` `mask` must contain exactly 1 layer, not 2. `` —
no path, no pluralization wart, no `transform` needed. **Both files need
committing.** No geometry-mismatch test added (deferred behaviour, §2).
`R/hsi_mask.R` untouched; all truth-table tests passed against it unmodified.

## Findings — NA tolerance (2026-07-18)

Method: keep-mask over the 9×9×101 REFLECTANCE fixture keeping cells 1–41 and
dropping 42–81, then each target run on the masked raster and compared against
the same call on unmasked input. "Sane" = masked cells `NA` in output **and**
kept cells identical to the unmasked run.

| Target | Behaviour | Verdict |
|---|---|---|
| `hsi_calc_rmean()` | Silent. dropped→NA; kept cells match unmasked exactly. | **Sane** |
| `hsi_calc_rsd()` | Silent. dropped→NA; kept match. No empty-vector `NaN`/warnings. | **Sane** |
| `hsi_calc_rabd()` | Silent. dropped→NA; kept match. | **Sane** |
| `hsi_smooth_median()` | Silent. **Zero focal bleed** — 0 of 41 kept cells newly NA. The hardcoded `na.rm = TRUE` in its `terra::focal()` call means NA neighbours are ignored rather than poisoning the window. | **Sane** (better than suspected) |
| `hsi_smooth_savgol()` | Silent. Returns a full 101-band SpatRaster; masked pixels all-NA across every band, kept pixels finite across every band. **Does not fail.** | **Sane — but doc is stale, see D1** |
| `hsi_remove_continuum()` | Silent. dropped→NA. Matches its documented NA→NA contract. | **Sane** |
| `hsi_calc_mnf()` | **Named suspect — did NOT fail.** Silent; returns a valid `mnf`/`prcomp`. Keeps all 81 rows (`nrow(fit$x) = 81`), propagating NA into the scores rather than dropping masked rows. Eigenvalues finite in both runs. | **Works — but see D2** |
| `hsi_apply_mnf()` | Silent on a masked fit (`n = 3`): 3 layers, dropped→NA, finite elsewhere. | **Sane** |
| `hsi_calc_stretch()` | **Named suspect — did NOT fail.** Silent; 3 layers, finite values present. | **Sane** |
| `hsi_plot_raster()` | Silent on both a single masked band and a masked `hsi_calc_rmean()` output. ggplot object builds **and** renders (`ggplot_build()` succeeds). | **Sane** |
| `hsi_plot_composite()` | **Skipped, structural.** Signature is `(x, plots, ...)` — it composes a *list of existing plots*, not a raster. Not an NA-tolerance surface; its inputs are plot objects already built by the plotters above. | **N/A** |
| zarowka `hsi_extract_spectra(n = Inf)` | Silent. Returns exactly 41 rows (= kept cells) vs 81 unmasked; no NA in the matrix; rownames are all kept-cell numbers, no dropped cell present. Honours the "every *valid* pixel" promise precisely. | **Sane** |
| zarowka `hsi_calc_snr()` | Silent. 101 rows, all `snr` finite. Range shifts 8.32–9.33 → 7.60–9.17, expected from a smaller pixel population. | **Sane** |

**Headline: no fragility found.** Both named suspects (MNF, stretch/plot family)
survived masked input; nothing errored, nothing warned, nothing crashed. No
segfaults anywhere. The 0.6 leanings' concern does not reproduce on this fixture.

### Decisions for Maury

> **Status update (2026-07-18, later same day):** D1 **fixed** (Sonnet —
> savgol `@details` now states the NA→NA pass-through; only its Rd changed,
> suite 560 green). D3 **fixed** (Sonnet — comment rewritten; re-measurement
> showed the "dimensional blocker" fallback theory was also wrong, `prcomp()`
> fits the full 81×101 stack fine; the true reason is that the recovery test
> compares against `stats::predict()` on the same training matrix, so the
> values must be fully controlled; zarowka suite 125 green). D4 **resolved**
> (Maury reinstalled HSItools 0.5.3.9001; verified `hsi_mask` exported,
> zarowka probes re-run through the real function — identical results to the
> `terra::mask()` fallback, cell-for-cell). **D2 remains open.**

- **D1 — `hsi_smooth_savgol()` roxygen is stale.** `@details` states "Pixels with
  `NA` values will cause the function to fail." The code does the opposite: its
  `terra::app()` lambda maps any-NA pixels to `rep(NA_real_, length(x))`, and the
  probe confirms a clean return with masked pixels all-NA. Since masking now
  *routinely* produces NA-rich rasters, this sentence will actively mislead.
  Left unedited per handoff §4 (observe, don't fix) — one-line doc correction is
  yours to authorise.
### D2 probe results (2026-07-18) — **BENIGN REGIME CONFIRMED**

Probe brief: `dev-notes/2026-07-18_handoff-d2-mnf-mask-probe-opus.md`.
Verdict: **masking does not bias MNF's noise estimate.** D2 can close.

**Probe A — source inspection (decisive on its own).** `hsi_calc_mnf()` passes
`terra::as.matrix(x)` straight to `spacetime::mnf()` with no `na.omit`,
`complete.cases`, or imputation; `terra::as.matrix()` keeps `NA` rows *in
place*, preserving spatial order (verified on a 3×3 toy: 9 rows in, 9 rows
out, `NA` row still at position 5). The whole noise path in
`spacetime:::mnf.matrix` (spacetime 1.3.3) is:

```r
function (x, ..., Sigma.Noise, use = "complete.obs")
{
    if (nrow(x) <= ncol(x))
        warning("matrix rank deficient: covariance matrices will be singular")
    Sigma = cov(x, use = use)
    if (missing(Sigma.Noise))
        Sigma.Noise = 0.5 * cov(apply(x, 2, diff), use = use)
    MNF(Sigma.Noise, Sigma, x)
}
```

`apply(x, 2, diff)` differences **first**, on the matrix with `NA` rows still
in position; `cov(..., use = "complete.obs")` **then** drops difference-rows
containing `NA`. Nothing removes rows before differencing, so no
non-adjacent pixel pair is ever fabricated. Benign regime, verbatim.

**Probe B — ground truth, 60×60×8, 10 seeds.** Noise-variance estimate vs
known truth, mean relative error (0 = unbiased):

| Case | condition | dropped | mean relerr | sd | paired shift vs clean |
|---|---|---|---|---|---|
| A: pure noise | clean | 0.0% | +0.0097 | 0.0107 | — |
| A: pure noise | light | 8.7% | +0.0097 | 0.0111 | **+0.00005** (sd 0.0040) |
| A: pure noise | heavy | 31.3% | +0.0096 | 0.0148 | **−0.00012** (sd 0.0068) |
| B: smooth signal + noise | clean | 0.0% | +0.0324 | 0.0102 | — |
| B: smooth signal + noise | light | 8.7% | +0.0330 | 0.0105 | +0.0006 (sd 0.0041) |
| B: smooth signal + noise | heavy | 31.3% | +0.0368 | 0.0141 | +0.0044 (sd 0.0068) |

Case A is the decisive one — with no signal, truth is exactly recoverable, and
masking moves the estimate by ~1e-4, **two orders of magnitude below the
seed-to-seed scatter**. Exactly zero effect. Case B shows a tiny positive shift
growing with hole density (+0.44 pp at 31% dropped, on a 3.2% baseline,
t ≈ 2.0 — marginal), attributable to *which* signal-gradient pairs survive when
contiguous blobs are removed, not to fabricated adjacency. For scale: if the
toxic regime held, ~1100 crack-edge pairs at 31% masking would each inject a
full signal jump — the bias would be order +100%, not +0.4 pp.

*Calibration note:* a first pass used a signal varying too fast at pixel scale
and showed **+317% error on clean, unmasked data** — the estimator was
measuring `diff(signal)`, not `diff(noise)`. That is a property of the lag-1
estimator (it assumes pixel-scale-smooth signal), not a masking effect, and it
is why Case A exists. Worth knowing independently: **on real data with sharp
spatial structure, MNF's noise estimate is inflated regardless of masking.**

*Context, pre-existing and not a masking effect:* `diff()` runs down the
row-major cell matrix, so each raster-row boundary contributes one
non-adjacent "wrap" pair — 59 of 3599 pairs (1.6%) at 60×60, ~0.26% on a
1408-column scan. Masking does not increase these; it only removes pairs.

### D2b — separate finding: the pixel-count guard doesn't count *valid* pixels

Not a bias question, but surfaced by the same probe and worth a decision.
`hsi_calc_mnf()` guards with `terra::ncell(x) <= lyrs`, which counts **all**
cells including masked ones; `mnf.matrix`'s own `nrow(x) <= ncol(x)`
rank-deficiency warning likewise sees the full row count. So a heavily masked
raster passes both while having almost no valid rows. Pure-noise scene, 3600
cells, 8 bands (true eigenvalues all ≈ 1.0):

| valid pixels | outcome | eigenvalues |
|---|---|---|
| 3600 | OK | 0.94 – 1.06 (correct) |
| 500 | OK | 0.84 – 1.21 |
| 50 | **OK, silent** | 0.45 – 1.63 (badly distorted) |
| 12 | **OK, silent** | 0.10 – 2.10 (garbage) |
| 9 | **OK, silent** | 3.3e-16 – 2.22 (degenerate) |
| 6 | ERROR | `system is computationally singular` |
| 3 | ERROR | `system is computationally singular` |

Between ~`nlyr` and a few × `nlyr` valid pixels the result is **silently
degenerate** — no error, no warning, no guard. Unreachable on a real core scan
(millions of pixels), but reachable on a small ROI or an aggressive mask.

**Proposed** (Maury's call, nothing implemented): count valid pixels in the
guard — e.g. compare `sum(complete.cases(terra::as.matrix(x)))` against `lyrs`
rather than `terra::ncell(x)` — or warn when valid pixels fall below some
multiple of band count. Both are behaviour changes to a shipped function, so
they are proposals only.

### Proposed close-out for D2

Benign verdict → the guidance sentence Fable sketched, offered as **proposed
text only** for §7's orbit (CLAUDE.md is Maury's, §0 rule 9):

> Masking before MNF is safe: `spacetime::mnf` differences before dropping
> `NA` pairs, so mask holes shrink the noise-estimate sample without biasing
> it. Dilate crack masks by 1–2 px to exclude physically disturbed edge pixels.

The dilation half is **not** an empirical result from this probe — it is the
unprobeable physical-contamination question (smearing, moisture halos beside
cracks) carried over as context. Treat it as domain judgment, not evidence.

Original D2 statement follows for the record.

- **D2 — MNF noise estimation across mask holes.** `hsi_calc_mnf()` retains all
  81 rows and lets NA flow into the scores rather than dropping masked rows.
  CLAUDE.md §7 already records that MNF's lag-1 row-difference noise estimate is
  corrupted by discontinuities (there: seam-crossing pixel pairs in a bound
  multi-scene raster). Masking introduces structurally the same discontinuity.
  The probe cannot separate "different data" from "corrupted noise estimate" —
  eigenvalues differ between clean and masked runs, but the data genuinely
  differs too. **This is a domain judgment, not an empirical one**, and it is
  flagged rather than settled. If it matters, the §7 guidance may want a sentence
  about masked input.
- **D3 — `test-hsi_apply_reduction.R` setup comment looks inaccurate.** It states
  the REFLECTANCE fixture "carries NA-prone edge bands (>= 1 NA per pixel across
  the full stack)". Measured: **0 of 81 cells carry any NA anywhere in the stack**;
  every cell is fully finite. The real blocker for fitting `prcomp` on the full
  fixture is dimensional (81 cells < 101 bands), not NA. The synthetic-clean-stack
  workaround the comment justifies is still correct; only the stated reason is
  wrong. Not edited.
- **D4 — installed HSItools is stale.** It predates `hsi_mask()`, so
  `HSItools::hsi_mask` is unreachable from zarowka (`not an exported object`).
  The zarowka probes therefore reproduced the identical NA pattern with
  `terra::mask(x, m, maskvalues = c(NA, 0))` — equivalent by the item-1
  verification. A local reinstall is needed before zarowka work touches
  `hsi_mask()`; same cross-repo sequencing noted in the 2026-07-14 dots handoff.
