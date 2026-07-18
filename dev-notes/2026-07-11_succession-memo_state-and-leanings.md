# 2026-07-11 — post-0.5.3 state and Fable's design leanings (succession memo)

**Date: 2026-07-11**
**Author: Claude Fable 5, at Maury's request, anticipating loss of Fable access**
**Purpose: (1) release state of record; (2) the next-session queue; (3) Fable's recorded *leanings* — with rationale — on every open design question, so future design sessions (any model) start warm. Leanings are NOT decisions: every one still requires Maury's explicit call in-session, per SKILL §0 rule 5.**

---

## 1. State of record

- **HSItools v0.5.3 released 2026-07-11**: merged dev → main, tagged `v0.5.3`, GitHub Release published, main CI green, dev reopened at `0.5.3.9000`. NEWS.md carries real history 0.1.0 → 0.5.3.
- SKILL at **v1.6.0** (§8 = contract v3.0 flat sidecar; §0 rule 7 = executors edit, Maury runs R).
- Interface contract **v3.0**; sidecar schema **1.1.0** (32 keys, flat, `notes` rejected).
- **hsical rebuild in flight** (Opus session) per `2026-07-11_opus-handoff_hsical-rebuild.md`; electron `bundled` is phase 2. hsical `Remotes:` can now point at `@main`.
- CRAN worklist seeded in `2026-07-11_closeout_053-ready-cran-worklist.md` §2 — execute at 0.9, not before.
- CI known limitation: Windows skips vignette rebuild (upstream quarto `R_LIBS` loss; documented in the workflow file).

## 2. Next-session queue, in order

1. **hsical build completion** (Opus, running) — then electron phase 2.
2. **0.6 design session** — brief: `2026-07-10_design-brief_0.6-mask-endmembers.md`, still fully current. §3 leanings below pre-seed it.
3. **0.7 design session** — `hsi_bind_sensors` four decisions + `hsi_bind_layers` stub adjudication (§4 below).
4. Backlogged mechanical pass (Sonnet, anytime): wavelength-parsing consolidation onto `check_wavelengths()` + the two `check_one_of()` conversions — snapshot churn expected, that's fine.

## 3. Fable's leanings — 0.6 (`hsi_mask()` + `hsi_endmembers`)

**`hsi_mask()`:**

- **Applier only, no creator.** `hsi_mask(x, mask, filename = "", overwrite = FALSE, ...)`. Mask *creation* is where material assumptions live (what counts as "background" is domain knowledge), so it belongs in zarowka templates/user code. One function, fewest assumptions — textbook house style. If a creator is ever promoted, it's a separate later function with a concrete need behind it.
- **Mask representation: single-layer `SpatRaster`, keep-where-non-NA-and-nonzero, i.e. `terra::mask()` semantics.** Don't invent a mask class; don't accept polygons in v1 (sf masking = one `terra::rasterize()` call away, add only on concrete need). Masked cells become `NA` across all layers.
- **Delegate to `terra::mask()` internally** — it's lazy, tiled-friendly, and battle-tested. HSItools adds: validation (`check_spatraster()` on both, geometry agreement check — same extent/res; abort `hsitools_error` on mismatch, no silent resampling ever), wopt/filename plumbing per §3.5, Shape A return.
- **Downstream NA-tolerance is a verification task, not a design task**: the `na.rm` conventions should already cover `hsi_calc_*`; one pass of spot-checks (esp. MNF and the stretch/plot family, which are the likely NA-fragile spots) belongs in the 0.6 execution handoff, not the design session.
- The unmixing pipeline invariant stands: masking ALWAYS precedes endmember search (background pixels corrupt N-FINDR-style searches).

**`hsi_endmembers` S3 class:**

- **Fields:** `spectra` (plain numeric **matrix**, rows = wavelengths, cols = endmembers — matrix over tibble because every consumer is linear algebra: nnls, projections; tibble would be converted at every use), `wavelengths` (numeric vector, length = nrow(spectra)), `locations` (tibble: one row per endmember — cell/x/y/id — tibble is right here, it's tabular metadata not math), `source` (free-text provenance: which raster, which search, date). **No `schema_version` until it serializes** — deliberate non-default; add it the day a writer exists (sidecar precedent: version is a serialization concern).
- **Three-layer pattern exactly as `hsi_metadata`**: `new_hsi_endmembers()` (pure assembler) → `validate_hsi_endmembers()` (dimension agreement: `ncol(spectra) == nrow(locations)`, `nrow(spectra) == length(wavelengths)`; finite, non-negative spectra; no NA wavelengths) → exported `hsi_endmembers()` front door. Maury writes it — this is the S3 learning arc's payoff.
- **`[` semantics** (decided in principle 2026-06-28): `x[i]` subsets endmembers — columns of `spectra` + rows of `locations` together, wavelengths untouched, returns validated `hsi_endmembers`. Lean: no `[,j]` wavelength subsetting in v1 (that's a resampling concern, 0.8's territory).
- **`print` method:** one screen — n endmembers, n bands, wavelength range, source line, and a `locations` head. cli-formatted, `invisible(x)`.
- **Serialization: don't build it in 0.6.** The concrete need (persist endmember sets across sessions / share between cores) hasn't materialized. When it does: §3.11 pattern, YAML probably wrong (matrix-heavy — more likely RDS or a two-file sidecar), decide then.
- Pre-flight before ANY threshold logic (from the brief, unchanged): verify `hsi_calc_sam` degrees-vs-radians from source; verify `terra::spatSample(cells = TRUE)` column naming against installed terra.

## 4. Fable's leanings — 0.7 (`hsi_bind_sensors` four decisions + stub)

1. **Name: `hsi_bind_sensors`.** It's bind-family (bind_rows sibling), the "fuse/merge" alternatives suggest resampling magic it deliberately doesn't do.
2. **Default cut: hard cut at the boundary wavelength, VNIR keeps the overlap side below, SWIR above; boundary defaults to the crossover point but is an explicit argument** (`cut_nm`), no default magic number hidden in the body. Rationale: agreed direction was hard-cut; the only open sub-question was the default, and an explicit required-or-defaulted argument keeps it honest.
3. **Correction: median-ratio gain on the overlap region as the only built-in option, on/off via a single logical** (`correct = TRUE`). No method enum until a second method has a concrete need (speculative-generalization rule). No blending, ever — blending fabricates spectra.
4. **Wavelength source: from the rasters' band names/metadata, never from a sidecar argument.** The function binds what it's given; sidecar integration is the caller's job. This also keeps it independent of the wavelength-units-on-ingest question (§5.3).
5. **`hsi_bind_layers` stub: remove the file.** Its validation logic now lives in `check_spatraster_list()`; the function has no body, no tests, no exports, and its one NOTE (`result`) dies with it. If layer-binding gets a concrete need later it returns as a designed function, not a resurrected stub. (This is the strongest of my leanings — carrying dead code into an API-freeze cycle is pure liability.)

## 5. Fable's leanings — remaining SKILL §10 / guarded threads

1. **`cli_alert_info()` in `hsi_check_gcp.R`:** keep the alerts, they're the function's *product* (it's a check/report function); just ensure they're suppressible — lean: leave as-is, close the question as "no migration."
2. **Wavelength units on ingest:** lean **normalize-to-nm at ingest adapters** (zarowka side), HSItools assumes nm everywhere — over an explicit units field. Rationale: a units field makes every downstream function conditional; normalize-once matches the µm-extent precedent (one canonical unit invariant per axis). But this one is genuinely close; the sidecar's existence is the best argument for the field option. Needs a real session.
3. **In-core search function naming:** moot until post-1.1 (search stays in zarowka through 1.0); don't spend a session on it.
4. **Spatial calibration extent-unit invariant:** still unconfirmed as *decided*; treat µm-canonical as the working assumption it has always been, and confirm it explicitly whenever a calibration function is next touched — cheapest closure path.
5. **hsical YAML reader:** SETTLED 2026-07-11 (no second reader) — listed here only so nobody reopens it.
6. **Duplicate wavelength parsing (3 sites) + `check_one_of()` conversions:** mechanical, bundled, Sonnet-ready anytime; snapshot churn expected and acceptable. Not a design question — just needs Maury to say go.

## 6. Working-with-models notes for post-Fable sessions

- The division of labor survives model changes: design sessions (anything touching §10 or architecture) go to the strongest available model; mechanical sweeps to Sonnet. SKILL v1.6.0 §0 rules are the contract either way — especially rule 5 (no unilateral settling) and rule 7 (Maury runs all R).
- Handoff-document discipline is the institutional memory: dated `.md`, one per session arc, closeouts append the session log. This memo is readable by any model; the SKILL is the conventions authority; the contract v3.0 is the interop authority; the roadmap docs are the sequencing authority. Keep those four lanes separate — it's what made this week's pace safe.
