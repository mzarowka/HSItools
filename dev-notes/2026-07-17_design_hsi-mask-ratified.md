# hsi_mask() — contract ratified (2026-07-17)

Ratified by Maury + Fable, 2026-07-17, closing the `hsi_mask()` half of the
0.6 design queue (succession memo §3 leanings, 2026-07-11; design brief
2026-07-10, chat-side). The `hsi_endmembers` S3 class half of 0.6 remains
open. Motivating evidence: the 2026-07-17 SWIR fire run (zarowka dev-notes,
test-plan note §6b) — an unmasked scene seated N-FINDR vertices on tray,
plastic, and coin, and the near-zero dark endmember blew nnls abundances up
to 16.4 (per-pixel sums to 17.6).

## Signature

```r
hsi_mask(x, mask, inverse = FALSE, filename = "", overwrite = FALSE, ...)
```

- `x` — SpatRaster, the data cube.
- `mask` — single-layer SpatRaster on the same geometry (extent/res);
  mismatch aborts `hsitools_error`, never silently resamples.
- `inverse` — terra-style polarity flip (decided in an earlier chat,
  reaffirmed at ratification). `FALSE`: `mask` is a keep-mask. `TRUE`:
  `mask` is a bad-mask.

## Semantics (normative truth table)

| mask cell | `inverse = FALSE` (keep-mask) | `inverse = TRUE` (bad-mask) |
|---|---|---|
| nonzero | keep | drop |
| `0` | drop | keep |
| `NA` | **drop** | **drop** |

Dropped cells become `NA` across all layers of `x`.

**NA always drops, regardless of `inverse`.** `NA` in a QC mask means "no
information" and a conservative masker drops unknowns. This deliberately
deviates from raw `terra::mask(inverse = TRUE)` (which keeps NA cells under
inversion) and must be stated in `@details`. `inverse` is purely a polarity
flip on the 0/nonzero axis.

## Scope (unchanged from the leanings)

- **Applier only, no creator.** Mask creation is domain knowledge and lives
  upstream (zarowka templates / user code).
- **One mask per call.** Multi-source workflows combine masks in mask space
  first (cheap, single-layer) and touch the cube once. Chained calls compose
  correctly (NA union) but are not the designed-for path — the cube is the
  expensive side (§7 mask-materialization caution).
- **Raster masks only in v1.** SpatVector input is deferred WITH a
  documented concrete need (Maury's QGIS-digitized cracks, 2026-07-17) —
  the promotion trigger is the rasterize recipe recurring in templates.
- No mask class; Shape A return; filename/wopt plumbing per §3.5.

## Upstream recipe (goes into a zarowka template as part of 0.6 execution)

Standard convention: **everything becomes a `{0, 1}` bad-mask, union with
`terra::any()`, one `hsi_mask(x, bad, inverse = TRUE)`.**

1. Saturation: `hsi_check_saturation(raw, limit, collapse = TRUE)` is
   already a bad-mask.
2. Vector sources (QGIS cracks etc.): `terra::union()` multiple layers
   *as vectors* (union is vector-only in terra — there is no raster
   union), then rasterize once onto the cube's grid.
3. Known reversal bites, all at the raster/vector seam:
   - `terra::rasterize()` background is `NA`, not `0` — always pass
     `background = 0`, or `0 | NA = NA` poisons the combined mask across
     the whole non-polygon area.
   - Polarity flips union into intersection: bad-masks combine with
     `any()`, keep-masks with `all()`. Mixing conventions fails silently.
     Hence the single bad-mask convention.
   - Rasterized values may be polygon IDs (1, 2, 3, …), not `1` — test
     nonzero, never `== 1`, or rasterize a constant field.
   - Thin features: default rasterization is cell-center; hairline cracks
     can vanish. `touches = TRUE` is the conservative setting for defect
     masks.

## terra facts (probed 2026-07-17, terra 1.9.34)

`maskvalues` is a **kill list** when `inverse = FALSE` ("these mask values
mean destroy the cell") and a **survivor list** when `inverse = TRUE`
("destroy everything that is NOT this"). Default `maskvalues = NA` is why
raw `terra::mask()` keeps zeros.

| call | result on mask `c(1, 0, NA, 2, 0)`, data `1:5` | |
|---|---|---|
| `mask(x, m)` | `1 2 NA 4 5` | raw: keeps `0` ✗ |
| `mask(x, m, inverse = TRUE)` | `NA NA 3 NA NA` | raw: keeps ONLY NA ✗ |
| `mask(x, m, maskvalues = c(NA, 0))` | `1 NA NA 4 NA` | = keep-mask ✓ |
| `mask(x, m, maskvalues = 0, inverse = TRUE)` | `NA 2 NA NA 5` | = bad-mask ✓ |

The truth table is therefore a one-liner: `maskvalues = if (inverse) 0 else
c(NA, 0)`. NA drops on both branches for free — terra never counts NA as
matching under inversion, even when NA is in `maskvalues`.

Other probe results:
- `filename` **and** `overwrite` are both explicit named arguments.
  `wopt = list(overwrite = TRUE)` is silently inert: it does not error as an
  unknown option (bogus names do: `[write] unknown option(s): nonsense`) but
  has no effect. Never route `overwrite` through `wopt`.
- **`overwrite = NA` silently overwrites an existing file** (no error, file
  modified). terra's other bad-`overwrite` messages are poor: `NULL` →
  `Expecting a single value: [extent=0]`; `"yes"` → `Not compatible with
  requested type:`. This is the evidence behind validating `overwrite` (and
  `filename`) — see the sweep question below.
- Geometry mismatch **errors loudly** in terra (`[mask] number of rows
  and/or columns do not match`, `[mask] extents do not match`) — it does NOT
  silently resample. The ratification text above overstated this; our own
  geometry check would be message-quality (class + arg names), not
  corruption prevention.
- A **multi-layer mask does not error** — terra masks band-by-band. Since
  `hsi_check_saturation()` defaults to `collapse = FALSE` and returns one
  layer per band (matching a cube's `nlyr` exactly), the single-layer check
  in `hsi_mask()` is load-bearing, not cosmetic.

## Verification (2026-07-17, run against the implemented function)

Implementation verified on the toy truth-table raster and the 101-band
fixture. Confirmed: both truth-table branches exact; single-layer mask
recycles across every layer with band names preserved; writes on
`filename`; errors on existing file when `overwrite = FALSE`; `wopt` names
honoured in-memory and typo'd dots rejected by terra (exemption vindicated);
all six validation branches fire with clean messages. On the fixture, 43
flagged pixels became NA identically across all 101 bands.

**§3.2 dots-exemption: RESOLVED — `hsi_mask()` is exempt, no
`check_dots_write()`.** Both halves of the exemption rationale hold for
`terra::mask()`: `wopt = list(names = "custom")` is honoured at
`filename = ""`, and unknown wopt names are rejected even in-memory. The
check would reject working in-memory calls while terra already catches typos.

### Test-writing gotchas discovered (for the test file)

- **In-memory `NA` reads back from GeoTIFF as `NaN`.** `is.na()` is TRUE for
  both; `identical()` / type-strict `expect_equal()` against `NA` fails on
  the round-trip. File-writing tests must assert the `is.na()` pattern.
- **Datatype differs between branches on integer input**: `inverse = FALSE`
  returns double, `inverse = TRUE` returns integer (terra promotion via
  `maskvalues`). Invisible on float data (all real captures), but any
  `typeof`/waldo-strict assertion on a toy integer raster is branch-dependent.

## Decided after verification (2026-07-17)

- **Geometry check: DEFERRED** (Maury). `hsi_mask()` leans on terra, which
  errors loudly on both rows/cols and extent mismatch — it does not silently
  resample, so nothing is at risk beyond message polish. The cost is a
  `simpleError` that names neither `x` nor `mask` instead of an
  `hsitools_error`. Revisit only if the terra-voiced message confuses someone
  in practice.
- **`overwrite`/`filename` validation: SWEEP** (Maury). Driven by the
  silent-overwrite probe above. `hsi_mask()` is the model; the sweep covers
  29 functions (26 HSItools + 3 zarowka). Handoff:
  `dev-notes/2026-07-17_handoff-write-tail-validation-sonnet.md`.

## Still open

- **Downstream NA-tolerance verification** (from the leanings, unchanged):
  spot-check `hsi_calc_*` under masked (NA-rich) input — MNF and the
  stretch/plot family are the suspected fragile spots.
- Real-data shakedown will validate the NA-always-drops choice; if it
  surprises in practice, that row of the truth table is the thing to
  revisit, not the signature.
