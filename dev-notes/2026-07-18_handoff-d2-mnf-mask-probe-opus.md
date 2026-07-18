# Handoff: D2 probe — is MNF's noise estimate biased by mask holes?

**Date:** 2026-07-18
**From:** Fable (decomposition discussed with Maury, same day)
**To:** Opus (execution)
**Scope:** Two probes, strictly report-only — a source inspection and a
ground-truth experiment. **No source edits anywhere, in either repo.** The
deliverable is a findings section plus a recommendation; every consequence
(doc sentence, CLAUDE.md guidance, workflow rule) is Maury's decision.
Background: `dev-notes/2026-07-18_handoff-mask-closeout-opus.md` — Findings
§D2 — and the mask design note of 2026-07-17.

---

## 1. The question, precisely

`hsi_calc_mnf()` (wrapping `spacetime::mnf`) estimates noise from lag-1
differences between spatially adjacent pixels. `hsi_mask()` now routinely
punches `NA` holes (cracks, impurities) into rasters *before* any analysis —
masking-precedes-everything is ratified convention. Does a holey raster
**bias** the MNF noise estimate, or merely shrink its sample?

Two possible regimes, with opposite verdicts:

| Regime | Mechanism | Verdict |
|---|---|---|
| **Benign** | Differences computed first; pairs containing `NA` drop out. Estimate uses only valid–valid adjacent pairs, all inside unmasked regions. | Unbiased; smaller sample. Cracks cost precision, not correctness. |
| **Toxic** | `NA` rows removed from the pixel matrix *before* differencing. Formerly non-adjacent pixels become "adjacent", fabricating a seam-style poisoned pair at **every crack edge** (the §7 seam problem, miniaturized and everywhere). | Noise estimate inflated; MNF on masked input untrustworthy. |

Known from the 2026-07-18 NA-tolerance pass (do not re-derive): masked input
does **not** error; eigenvalues come back finite; `fit$x` keeps all cells
(81 rows on the fixture) with `NA` propagated into scores. None of that
distinguishes the regimes — that is this probe's job.

## 2. Probe A — source inspection

1. Read `R/hsi_calc_mnf.R` end to end: what preprocessing happens to the
   values matrix before `spacetime::mnf` is called (any `na.omit`,
   `complete.cases`, imputation, zero-fill)?
2. Read the **installed** `spacetime::mnf` source — the actual code, not the
   help page (`getAnywhere(mnf)` / `spacetime:::` as needed; print method
   bodies, follow internal helpers). Locate the difference/noise-covariance
   step and determine exactly how `NA` flows through it: differenced then
   dropped, rows dropped then differenced, imputed, or propagated into the
   covariance (and if propagated, what makes the eigenvalues finite anyway —
   e.g. `cov(..., use =)`).
3. Record the answer **with function names and the decisive lines quoted** in
   the findings. If the code path is ambiguous, say so — Probe B then carries
   the verdict alone.

## 3. Probe B — ground-truth experiment

Scratchpad script, run from a file (inline `Rscript -e` segfaults on this
machine). This is a probe, not a test file — a from-scratch synthetic raster
is fine here (§5.2's fixture rule binds `tests/`, not scratchpad probes).

**Design:**

1. Synthetic scene with *known* noise: e.g. 60×60×8 `SpatRaster`. Signal =
   smooth, spatially correlated field per band (a 2-D gradient or broad
   Gaussian bumps — MNF's premise requires correlated signal); noise = iid
   Gaussian with a **known per-band sd** (vary sd across bands so the noise
   spectrum has shape). Truth: noise covariance = `diag(sd^2)`.
2. `hsi_calc_mnf()` on the clean scene → record `fit$values` (and whatever
   noise-covariance estimate the object exposes) vs truth. This calibrates
   what "recovered correctly" looks like at this geometry.
3. Mask **crack-like slits**: a handful of 1-pixel-wide lines (vertical +
   horizontal + one diagonal), roughly 5–10% of cells dropped, applied with
   `hsi_mask()` (installed HSItools 0.5.3.9001 exports it — verified
   2026-07-18). Re-run MNF → compare estimates to truth and to the clean run.
4. Also one **heavier** variant (~40% dropped, mixed slits + blobs) to see
   whether any bias grows with hole density — a bias that scales with crack
   count is the toxic-regime fingerprint (each edge contributes poisoned
   pairs).
5. Repeat over ~10 seeds; report mean ± sd of the noise-estimate error for
   clean / light-crack / heavy-crack. One seed proves nothing at this size.

**Reading the result:**

- Masked estimates ≈ clean estimates ≈ truth (within seed scatter), no trend
  with hole density → **benign regime confirmed**.
- Masked estimates inflated vs clean, growing with hole density →
  **toxic regime confirmed**.
- Anything murkier (e.g. bias only in some bands, or Probe A and B
  disagreeing) → report exactly what was seen, no verdict forced.

## 4. Deliverable

Append `### D2 probe results (2026-07-18)` to the Findings section of
`dev-notes/2026-07-18_handoff-mask-closeout-opus.md`: Probe A answer (with
quoted lines), Probe B numbers (compact table: truth / clean / light / heavy,
mean ± sd), verdict, and a **proposed** next step for Maury —

- benign → a one-sentence guidance note for §7's orbit ("mask holes shrink
  MNF's noise sample but don't bias it; dilate crack masks 1–2 px for edge
  contamination") — *proposed text only, Maury edits CLAUDE.md*;
- toxic → a proposed `@details` warning for `hsi_calc_mnf()` and a
  workflow rule (MNF before masking, or extraction-based PCA instead) —
  *proposed, not implemented*.

Show raw script output in chat either way.

## 5. Out of scope

- Any edit to `R/` in either repo, `hsi_calc_mnf` docs included — findings
  only, even if the verdict seems obvious.
- The crack-edge physical-contamination question (smearing, moisture halos
  beside cracks): **not empirically probeable here**; it is masking-width
  guidance for the future template. Mention it in findings only as context.
- CLAUDE.md (§0 rule 9), git (§0 rule 8), the item-3 template.

## 6. First move

Probe A first — if the source answers cleanly, Probe B becomes confirmation
rather than discovery, and its design can be trimmed accordingly. Then run,
append findings, show output.
