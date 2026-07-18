# Handoff: `hsi_calc_snr()` — design locked, ready for implementation

**Date:** 2026-07-12
**From:** Fable (design session with Maury)
**To:** Opus (implementation)
**Scope:** One new diagnostic function. Out of current roadmap scope (0.6–1.0 milestones unaffected); this is a standalone side thread.

---

## 1. What this is

A per-band signal-to-noise diagnostic. SpatRaster in, tibble out. Primary motivation: identifying bands to drop (bands whose SNR collapses relative to neighbours). Secondary use discovered during design: run on raw DN of a white-reference scan to get a legitimate sensor-SNR estimate — the WR panel is a homogeneous target, so spatial SD ≈ noise, not scene structure.

The function itself knows nothing about any of this. It computes per-band statistics; interpretation (apparent SNR on a scene vs. sensor SNR on a homogeneous target) belongs to the user and the docs.

## 2. Locked decisions (do not reopen)

| Decision | Locked value |
|---|---|
| Name | `hsi_calc_snr()` (house `hsi_calc_*` pattern) |
| Home | **zarowka first** (promotion flow; battle-test before any HSItools promotion) |
| Statistics engine | `terra::global()` over the **full raster** — lazy, file-backed, exact. **No sampling, no `size` argument, no `spatSample`.** Sampling is deferred until a real cube proves `global()` too slow (speculative generalization otherwise). |
| Signature | Minimal: essentially `hsi_calc_snr(x)`. Every additional parameter must be argued for, not assumed. |
| Output | Tibble, one row per band: `wavelength`, `mean`, `sd`, `snr` (= mean/sd). Pure data. |
| Not included | No plotting. No thresholding. No band-dropping logic. No method argument. User subsets bands themselves. |
| Method | Global per-band mean/SD only. LMLSD/homogeneous-block (Gao 1993) and decorrelation-based estimators were considered and **rejected** as out of proportion for a diagnostic. Do not add them. |
| Processing-level agnostic | Accepts DN, radiance, or reflectance — just numbers in a raster. No assumptions about calibration state, sensor, vendor, or material. |

## 3. Documentation requirements (`@details`)

These caveats live in roxygen, not in code branches:

1. **On heterogeneous scenes this is *apparent* SNR** — per-band SD is dominated by scene variability, so absolute values are scene-relative. Still valid for *relative* band comparison within one cube (the band-dropping use case), because the scene-variability offset largely cancels across bands.
2. **On a homogeneous target (e.g., a white-reference scan in raw DN) this approximates true sensor SNR** at that signal level.
3. **SNR is signal-level-dependent.** An underexposed capture yields SNR at that DN level — useful for judging exposure adequacy, not the SNR available at proper exposure.
4. **Saturation inflates SNR** — clipped values compress SD toward zero. Users must screen for saturation before trusting SNR from a possibly-saturated capture. See §5: no screening tooling exists yet; this is a docs warning only, with no cross-reference to any function.

Wording of caveats is Opus's to draft; substance above is settled.

## 4. Implementation notes

- All house rules apply — load `hsitools-development` skill (v1.6.0) at session start. In particular: approach confirmed here, so implementation may proceed, but **one change per iteration**, Maury runs `devtools::test()` locally, explicit go/no-go between steps.
- zarowka conventions: same style baseline as HSItools (`|>`, `\(i)`, `purrr`, `::` everywhere, `cli::cli_abort()` with validation block).
- `terra::global()` returns a data.frame with stat columns; access columns **by name, never position** (same discipline as the `terra::extract()` rule). Verify exact column naming (`mean`, `sd` vs `rmse` etc.) against the installed terra version before writing assertions — same verify-first rule as `spatSample(cells = TRUE)` in skill §10.
- Wavelengths for the output tibble: from the raster's band names/values per existing house idiom in the `hsi_calc_*` family — follow whatever the index functions already do; do not invent a new wavelength-parsing path (duplicate wavelength-parsing logic is already flagged for refactor — don't add a fourth copy).
- `NA` handling (`na.rm`) was **not discussed**. Recommend defaulting to the house canonical `na.rm = TRUE` if a parameter is warranted at all — but confirm with Maury before adding any parameter beyond `x`.
- Tests: standard §5 discipline (seven sections, fixtures in 517.58–772.19 nm, `withr::local_tempfile()`, property-based assertions). A degenerate-input case worth covering: constant-valued layer → SD 0 → `Inf` SNR; decide behaviour with Maury (likely: return `Inf`/`NaN` honestly, no special-casing).

## 5. Correction to the record: saturation screening does NOT exist

During design, Fable asserted that saturation screening "is already an R-side responsibility per protocol." **Maury corrected this: true saturation screening was discussed but never matured into anything concrete. No implementation exists** — not in HSItools, not in zarowka. The protocol intent (screening happens in R, not in hsical) stands, but it is an unbuilt intent.

Consequences:
- The saturation caveat in `hsi_calc_snr()` docs must not reference any screening function.
- A future `hsi_check_saturation()` (or similar) is a **potential new backlog item, not scheduled, not designed**. Whether/where it lands (zarowka vs core), its contract, and its name are all open. Do not design or implement it in the SNR session unless Maury explicitly asks. If it comes up, treat as a fresh design conversation.

## 6. Explicitly parked / out of scope for the implementation session

- Sampling path (`spatSample`) — only if `terra::global()` proves too slow on real data, and then as a fresh design conversation.
- LMLSD or any noise-decorrelation method — rejected, not parked.
- Saturation screening — see §5.
- Any promotion talk (zarowka → HSItools) — premature until battle-tested on real campaigns.
- Roadmap integration — this thread is outside 0.6–1.0 sequencing; do not renumber milestones.

## 7. Suggested first move for Opus

Confirm with Maury: (a) exact zarowka file placement/naming, (b) whether `na.rm` earns a parameter or is hardcoded `TRUE`, (c) constant-layer/`Inf` behaviour. Then draft the function body, hand the diff to Maury, wait for local test results.
