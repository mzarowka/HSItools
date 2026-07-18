# Handoff: `hsi_check_saturation()` — design locked, ready for implementation

**Date:** 2026-07-12
**From:** Fable (design session with Maury)
**To:** Opus (implementation)
**Scope:** One new QC/check function. Companion to `2026-07-12_handoff-snr-opus.md` — both threads are sized for the same implementation session. **This document supersedes §5 of the SNR handoff**: saturation screening is no longer "not designed" — it is designed here and cleared for implementation. The SNR docs caveat still must not cross-reference this function until it actually exists in zarowka.

---

## 1. What this is

A pixel-wise, per-band saturation check. SpatRaster in (meant to be raw DN), logical 0/1 SpatRaster out marking saturated pixels. The mask is the primitive; everything else (per-band counts, session go/no-go, first-pass target-space masking) is user-side interpretation or trivial aggregation of it (`terra::global(mask, "sum")` reproduces the per-band report we originally sketched — deliberately not shipped as a function).

Motivations, in order discovered:
1. A saturated white reference silently corrupts reflectance for the whole session; the damage is invisible after calibration and can only be caught on raw DN.
2. Run on a *target* scan, the collapsed mask is a first-pass mask of over-saturated pixels in target space (specular hits etc.) — a natural future *producer* for whatever `hsi_mask()` becomes in 0.6, without overlapping it.
3. The `hsi_calc_snr()` docs warn that saturation inflates SNR; this is the tool that check eventually points at.

## 2. Locked decisions (do not reopen)

| Decision | Locked value |
|---|---|
| Name | `hsi_check_saturation()` — it *checks*; mask vs. report is the user's interpretation. Sits beside `hsi_check_gcp()`. Deliberately does **not** stake out `hsi_mask_*` namespace ahead of the 0.6 `hsi_mask()` design. |
| Home | **zarowka first** (promotion flow). |
| Signature | `hsi_check_saturation(x, limit, collapse = FALSE, filename = "", overwrite = FALSE, ...)` |
| Saturation limit | **Required explicit argument, no default, no inference from data.** Sensor-agnosticism forbids hardcoding bit-depth values (no 4095 anywhere); inferring from the data maximum is a rejected trap (clean scans false-positive their brightest pixel). The user supplies their instrument's saturation DN. Lab-specific values may eventually live in a zarowka template — instrument knowledge in the template layer, never in the function. |
| Limit argument name | Maury leans `limit` (lean); final pick is his — confirm before coding. |
| Output | Logical 0/1 SpatRaster. Default (`collapse = FALSE`): multi-layer, same `nlyr` as input, one layer per band (information-preserving primitive). `collapse = TRUE`: single layer via `any()` across layers (terra Summary group generic — stays lazy). |
| Flag name | `collapse`, **not** `global` — `terra::global()` reduces spatially to scalars, the opposite axis; fluent terra users would guess wrong. |
| Collapse semantics | Plain `any()`. Tolerant collapse ("saturated in more than *k* bands") is **user-side** — an analysis judgment the function must not make. |
| Write handling | Standard Shape A/B: `filename`/`overwrite`/`...` to `terra::writeRaster()`, `if (filename != "")` guard, reassign from `writeRaster()` return. Lazy/file-backed throughout; the comparison and the `any()` reduction are both lazy. |
| Not included | No tibble/report output (derivable via `terra::global()`; a thin summary wrapper may *earn* its place later if the two-step proves a per-session annoyance — not now). No thresholding, no automatic masking, no band-dropping. No processing-level enforcement. |

## 3. Documentation requirements (`@details`)

1. **Meant for raw DN.** The function cannot verify processing level; running it on radiance/reflectance is meaningless but not detectable. Docs caveat, not code enforcement — same division of responsibility as `hsi_calc_snr()`.
2. **The limit is instrument knowledge.** Users must supply their sensor's saturation DN; the function refuses to guess.
3. **Session-level consequence worth stating:** a saturated white reference makes the entire session's reflectance suspect — the intended workflow is checking WR (and captures) right after acquisition, while re-scanning is still an option.
4. **Natural companion to `hsi_calc_snr()`** — per-band saturation counts (via `terra::global(mask, "sum")`) join the SNR tibble on wavelength. Mention the pattern; cross-reference only once both functions exist.

Wording is Opus's to draft; substance is settled.

## 4. Micro-decisions to confirm with Maury before coding

Named in design, deliberately left open:

- **Limit argument name** — `limit` (Maury's lean) vs. `saturation_dn` vs. other.
- **Layer naming** — per-band output inherits input band/wavelength names (keeps `terra::global()` summaries and SNR-tibble joins aligned); collapsed layer name proposal: `"saturated"`. Confirm both.
- **`NA` handling** — proposed: honest propagation (`NA` in → `NA` out; no silent coercion to "not saturated"). Note `any()` with `na.rm = FALSE` propagates `NA` into the collapsed layer. Fable's recommendation is honest propagation as default posture; Maury has not ruled.

## 5. Constraints restated so they don't get relitigated

- **Contract v3.0 rejected a `saturation_ratio` sidecar field.** This function's output is a derived data product (or an ephemeral diagnostic), never scan-record metadata. Revisiting that is a contract-first conversation, not a side effect of this function.
- Sensor-, manufacturer-, material-agnostic at every level — the required-limit design exists precisely to keep bit-depth knowledge out of the function.
- hsical is untouched: screening happens in R, in zarowka — the app never processes spectral data.
- All house rules apply — load `hsitools-development` skill (v1.6.0) at session start. One change per iteration; Maury runs all R locally; explicit go/no-go gates.

## 6. Explicitly parked / out of scope

- Any relationship to `hsi_mask()` beyond "future producer" — 0.6 design owns that.
- Tolerant/threshold collapse variants — user-side, possibly forever.
- Summary-report wrapper — only if real use proves the two-step annoying.
- Acquisition-time screening in hsical — permanently out (app never touches spectral data).
- Promotion to HSItools — after battle-testing, not before.

## 7. Suggested session shape for Opus

Two functions, same session, SNR first (smaller, zero open micro-decisions beyond its own §7 list). For this one: confirm §4 micro-decisions with Maury → draft function body → hand diff → wait for local `devtools::test()` results → tests next iteration. Standard test discipline (§5 of the skill): fixtures in 517.58–772.19 nm, `withr::local_tempfile()`, one behaviour per test, snapshot layer for cli errors. Degenerate cases worth covering: all-saturated layer, no-saturated layer, `NA` pixels, `collapse = TRUE` on single-band input.
