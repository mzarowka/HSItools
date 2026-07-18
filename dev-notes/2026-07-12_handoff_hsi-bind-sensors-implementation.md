# Implementation handoff — `hsi_bind_sensors()` (milestone 0.7)

**Date: 2026-07-12**
**Author: Claude Fable 5, design session with Maury**
**Executor: Opus (or Fable), with Maury running all R locally per SKILL.md §0.7**
**Scope: full contract for `hsi_bind_sensors()`, retirement of `hsi_bind_layers()`, roxygen and test requirements. All design decisions below are LOCKED by Maury on 2026-07-12 — do not reopen. The paired offset-experiment document is separate and NOT part of this handoff.**

---

## 1. Milestone context

Per the 2026-07-08 roadmap re-cut, 0.7 is a lean milestone: `hsi_bind_sensors()` alone (the spectral metadata sidecar shipped early in 0.5.x). This handoff closes the four open decisions previously listed in SKILL.md §10 plus the sub-decisions surfaced during the 2026-07-12 design session. After implementation lands, SKILL.md §10 needs its `hsi_bind_sensors` entry removed (skill bump — coordinate with Maury; skill carries durable conventions only, so the contract itself does NOT go into the skill).

**Method (locked 2026-06-22, unchanged):** hard cut + median-ratio gain from the overlap + no blending.

---

## 2. Locked decisions (2026-07-12)

1. **Name: `hsi_bind_sensors()`.** `hsi_bind_layers()` is **retired entirely** (see §6). Rationale: the trivial bind (resample to common grid, then `c(x, y)`) is achievable with existing package pieces plus one line of terra; a wrapper earns nothing.
2. **`cut` is required** — numeric scalar, wavelength in nm, no default. No duplicated spectral regions in the output.
3. **`x` is the grounding sensor and source of truth.** Gain is applied to `y` only; `x` is never modified.
4. **No offset parameter.** Rationale goes in `@details` (see §7). Evidence that could ever justify one comes from the separate flat-reference experiment (own handoff, own session).
5. **Wavelengths come from `names()`**, parsed via the existing centralized wavelength-parsing helper — do NOT introduce a fourth copy of the parsing logic (refactor debt already flagged). Documentation states plainly: layer names are wavelengths in nm, full stop. Unit conversion is the user's job.
6. **`tol` parameter** (new, locked 2026-07-12): non-negative numeric scalar in nm, **default `0`**. Symmetrically widens the gain-estimation window (see §4 step 2). Touches gain estimation ONLY — never output band membership.
7. **Warnings** (both `cli::cli_warn()` with `class = "hsitools_warning"`):
   - **Zero pairs** in the (post-tol) overlap window → warn that output is a simple concatenation (gain = 1), proceed.
   - **Exactly one pair** → warn that the gain rests on a single band pair, proceed with that gain.
8. **Overlap membership is inclusive on both ends** (`>=` / `<=`). Rationale on record: on Maury's rig the interval-defining bands (VNIR 1000.92 / SWIR 999.70) must not exclude themselves.
9. **Pairing rule for gain (unequal counts allowed):** for each `y`-band inside the window, pair it with the **nearest `x`-band inside the window**; one ratio per `y`-band; reuse of an `x`-band across pairs is allowed. Scalar gain = **median of the per-pair ratios**, where each ratio = `median(x_band values) / median(y_band values)` (spatial medians per band). The gain MUST collapse to a scalar — anything wavelength-dependent is blending through the back door, which was rejected.

---

## 3. Signature

```
hsi_bind_sensors(x, y, cut, tol = 0, filename = "", overwrite = FALSE, ...)
```

- `x` — SpatRaster, grounding sensor (lower wavelength range). Source of truth; never modified.
- `y` — SpatRaster, higher wavelength range. Receives the gain.
- `cut` — numeric scalar, nm. Junction: keep `x` bands with wavelength `<= cut`, keep `y` bands with wavelength `> cut`. Required, no default.
- `tol` — numeric scalar `>= 0`, nm. Default `0`. Widens the gain window symmetrically.
- `filename`, `overwrite`, `...` — standard Shape B write block (§3 house conventions).

**Precondition (documented, not enforced beyond validation below):** `x` and `y` are already co-registered on a common spatial grid — same extent, resolution, and CRS. Producing that grid is the user's responsibility (existing resampling/co-registration functions). Function assumes larger-than-memory data throughout — lazy terra ops only, no premature materialization.

---

## 4. Algorithm (order is load-bearing: gain BEFORE cut, because the cut destroys the overlap)

1. **Validate** (see §5).
2. **Overlap window:** `[min(wl_y) - tol, max(wl_x) + tol]`, inclusive bounds. Identify bands of each raster inside it (using full, uncut inputs).
3. **Gain:**
   - 0 `y`-bands or 0 `x`-bands in window → zero-pair warning, `gain <- 1`.
   - Else form pairs per §2.9; if exactly one pair → one-pair warning. Compute scalar gain.
4. **Apply:** `y <- y * gain`. (`x` untouched.)
5. **Cut:** subset `x` to `wl <= cut`, subset `y` to `wl > cut`. Strict boundary → no band survives on both sides by construction.
6. **Concatenate:** `c(x_kept, y_kept)` (terra method). Layer order is monotonic in wavelength because validation guarantees `x` below `y`. Names carry through unchanged.
7. **Write/return** per Shape B, standard section comments verbatim (`# Validate inputs` / `# Build write options` / `# Write to file` / `# Return result`). Remember the `filename != ""` guard and reassignment from `terra::writeRaster()` return value.

---

## 5. Validation block (all aborts `cli::cli_abort(class = "hsitools_error")`; exported function → no `call` threading)

- `x`, `y` are SpatRasters (house `check_spatraster()` or equivalent existing helper).
- Names of both parse numerically via the centralized helper; abort loudly if not.
- Spatial grids compatible (same extent/resolution/CRS — `terra::compareGeom()` or house equivalent; check what exists before inventing).
- `cut` — numeric scalar (house `check_numeric()`); must lie so that at least one band survives on each side (`cut >= min(wl_x)` region non-empty AND `y` bands `> cut` non-empty) — abort otherwise.
- `tol` — numeric scalar, `>= 0` (house `check_numeric()`; positivity/non-negativity constraint IS locked here, so apply it).
- **Range ordering:** abort if `x`'s wavelength range is not below `y`'s (no silent swapping). Locked: `min(wl_x) < min(wl_y)` and `max(wl_x) < max(wl_y)` — abort with a message telling the user which argument should be which.
- `overwrite` — logical scalar per house pattern.
- No aborts inside purrr lambdas; collect-all-offenders if applicable.

---

## 6. `hsi_bind_layers()` retirement checklist

1. Delete `R/hsi_bind_layers.R` (incomplete stub past its validation block).
2. Confirm it is absent from NAMESPACE (2026-06-10 audit reported it missing already — verify against live dev).
3. Grep zarowka templates and HSItools vignettes for `hsi_bind_layers` references (2026-06-10 audit flagged dangling references) and purge/replace. zarowka sweep can be its own iteration.
4. If any test files reference it, remove them in the same iteration as the source deletion.
5. NEWS.md entry: retired without deprecation shim (never exported/released — confirm before wording).

---

## 7. Roxygen requirements (§4 house rules apply verbatim)

- Tag order per ROXYGEN_GUIDELINES; `@export` last; `@returns` = SpatRaster canonical string; `@family` in Title Case (likely `Sensor Fusion` — confirm existing family names with a grep before inventing).
- Canonical `@param` strings for `x` (adapted to note grounding role), `filename`, `overwrite`, `...` per catalogue.
- `@details` MUST contain, in prose:
  - Names-are-wavelengths-in-nm contract ("move the decimal place" guidance).
  - Gain-before-cut ordering and why (`cut` destroys the overlap).
  - The scalar-gain rationale (no wavelength-dependent correction = no blending).
  - **The no-offset paragraph:** residual junction offsets in well-calibrated rigs are typically low-SNR edge artifacts, not calibration errors; an additive offset would "correct" noise. No offset option is provided; evidence from a flat-reference experiment would be required to justify one.
  - `tol` semantics: widens gain window only, never output membership; user asserts spectral flatness across the widened window.
- Examples in `\dontrun{}`, result named `x_bound` (or similar `x_<suffix>`).

---

## 8. Test outline (§5 house rules: seven sections, one behaviour per test, withr tempfiles, 517.58–772.19 nm fixture constraint)

Fixture note: the standard fixture range is single-sensor. Build two synthetic SpatRasters with numeric-parseable names simulating a lower and an upper range (small, in-memory scale is fine for tests; keep wavelength values arbitrary — do NOT encode VNIR/SWIR-specific values as meaningful constants, sensor agnosticism applies to tests too).

Behaviours to pin (target 8–12 tests):

1. Round-trip shape: output nlyr = kept_x + kept_y; names monotonic in wavelength; names preserved.
2. Hard cut correctness: no band appears on both sides; boundary band (`wl == cut`) lands in `x` side.
3. Gain applied to `y` only; `x` values bit-identical to input.
4. Scalar gain math on a known synthetic overlap (multi-pair case): median of nearest-neighbour ratios.
5. `tol = 0` vs `tol > 0` changes gain-window membership but never output band membership.
6. Zero-pair path: warning with `hsitools_warning` class (class assertion rides on the message assertion, never standalone), gain = 1 verified numerically.
7. One-pair path: warning fires; gain equals the single ratio.
8. Inclusive bounds: a band exactly at the window edge is included.
9. Validation aborts: non-parseable names; ranges not ordered `x` below `y`; `cut` leaving an empty side; negative `tol`. `expect_snapshot(error = TRUE)` layer for messages (volatile-path `transform` if paths appear).
10. Shape B: file-backed write honoured (`withr::local_tempdir()`, reassign from `writeRaster` return); `filename = ""` stays in memory.

---

## 9. Explicitly out of scope

- The flat-reference junction-offset experiment (separate handoff, separate session; edge-window median-ratio protocol on grey reference at specimen exposure).
- Wavelength units on ingest (§10 — stays open; this function's contract merely *declares* nm).
- Any resampling/co-registration convenience wrapping.
- `hsi_resample()` design (still pending, independent).

---

## 10. Rig sanity numbers (for reviewer intuition only — NEVER encode in package or tests)

Maury's untrimmed VNIR/SWIR edge: overlap `[999.70, 1000.92]` (1.22 nm). At `tol = 0`: one pair (1000.92 ↔ 999.70). At `tol = 0.5`: window `[999.20, 1001.42]`, two `x`-bands (999.63, 1000.92), one `y`-band (999.70) → still one pair, but the better one (999.63 ↔ 999.70, Δ0.07 nm). After SNR trimming (~950/~1100 nm) the overlap is empty → zero-pair branch. His workflow therefore exercises both warning paths naturally.

---

## 11. Session log entry

| Date | Session |
|---|---|
| 2026-07-12 | 0.7 design (Fable + Maury): all four `hsi_bind_sensors` §10 decisions closed + `tol`, pairing rule, warning semantics, inclusive bounds, `hsi_bind_layers` retirement. This handoff + offset-experiment handoff (separate). |
