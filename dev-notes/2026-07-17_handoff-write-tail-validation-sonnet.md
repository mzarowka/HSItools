# Handoff: write-tail validation (`filename` / `overwrite`) — mechanical sweep

> **COMPLETED 2026-07-17** (Sonnet, five batches; reviewed by Fable same day).
> Final: 28/28 in-scope functions (25 HSItools + 3 zarowka), suites green
> (HSItools 532, zarowka 125 + known unmixR warning). §6 decisions: shared
> helper (b), family batches, `overwrite = 1` rejection accepted. Deviations
> from this spec, all sound: `hsi_bind_layers` excluded (unexported,
> unimplemented stub — always errors `object 'result' not found`; decide
> implement-or-delete separately); six functions had NO test file and got
> minimal sweep-scoped ones (`hsi_calc_raba`, `hsi_destripe`, `hsi_apply_mnf`,
> `hsi_write_scaled`, `hsi_bind_rows`, `hsi_find_extent`) — full §5 suites
> remain open backlog; helper signature evolved to `(.f, args)` +
> `utils::modifyList()` to survive required-`filename` functions (Shape B),
> duplicated identically into zarowka (test helpers don't ship). CLAUDE.md
> §3.2 amendment remains Maury's.

**Date:** 2026-07-17
**From:** Fable (design with Maury, this session; empirical probes by Fable, same session)
**To:** Sonnet (implementation sweep)
**Scope:** Mechanical sweep of every function with a write tail in HSItools and zarowka. No new
helpers, no new conventions invented. Convention-level change; the CLAUDE.md amendment itself is
Maury's, not yours. Background: `dev-notes/2026-07-17_design_hsi-mask-ratified.md` (the terra
facts section carries the probe evidence).

---

## 1. The problem, in two sentences

Every function with a write tail accepts `filename = "", overwrite = FALSE` and forwards both to
terra without validating either. **`overwrite = NA` silently overwrites an existing file** — no
error, file modified on disk — and terra's messages for other bad values are near-useless.

## 2. Verified facts — do not re-derive, do not contradict

Probed on terra 1.9.34, Windows, 2026-07-17 (evidence in the design note):

| input | terra's behaviour |
|---|---|
| `overwrite = NA` on an existing file | **NO ERROR — file silently overwritten** |
| `overwrite = NULL` | `Expecting a single value: [extent=0]` |
| `overwrite = "yes"` | `Not compatible with requested type: [type=character; target=logical]` |

`rlang::check_bool()` gives clean lines for all of these (`` `overwrite` must be `TRUE` or
`FALSE`, not `NA`. ``). That contrast is the whole justification — nothing else about the
functions changes.

Also verified, so nobody re-litigates it:
- `filename` **and** `overwrite` are both explicit named arguments to terra primitives.
  `wopt = list(overwrite = TRUE)` is silently inert (accepted name, no effect). Never route
  `overwrite` through `wopt`.
- **No churn found anywhere.** No test, example, vignette, or zarowka template passes a non-bool
  `overwrite`; every call site uses `TRUE`/`FALSE` literals. The only snapshot mentioning
  `overwrite` is `_snaps/hsi_write_metadata.md`, and that is its own file-exists error raised
  from a *valid* `overwrite = FALSE` — unaffected by this sweep. (`zar_templates.R:49`
  `overwrite = NULL` is a `switch()` branch label, not an argument. Ignore it.)
- `hsi_read_metadata()` **already** validates with `rlang::check_string(filename)` — it is the
  existing in-house precedent for the idiom. Match its style; do not change it.

## 3. Locked design (do not reopen)

| Decision | Locked value |
|---|---|
| The checks | `rlang::check_string(filename)` and `rlang::check_bool(overwrite)`. Nothing else. |
| Why rlang, not house helpers | §3.3 rlang type-check carve-out. These are sanctioned for scalar types where no house `check_*` exists. **Never wrap them; never write a house `check_string()`/`check_bool()`.** |
| Error class | Stays rlang-classed. These errors do **NOT** carry `hsitools_error`, and that is correct and accepted (§3.3). Do not add a class. |
| Placement | Last in the validation block, in §3.1 argument-order sequence (`filename` is arg 10, `overwrite` arg 11) — i.e. after every substantive check, immediately before the `wopt_user <- rlang::list2(...)` line where one exists. |
| Model function | `hsi_mask()` (already carries both checks). **Caveat:** its checks currently sit *above* its mask-layer check; per the placement rule they belong below it. Fix that ordering as part of iteration one. |

Canonical shape after the sweep:

```r
  # Validate inputs
  check_spatraster(x)
  ...other substantive checks, in argument order...
  rlang::check_string(filename)
  rlang::check_bool(overwrite)

  # Store user input in a spliceable list
  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)   # only where it already exists — see §6
```

## 4. Scope — 29 functions (26 HSItools + 3 zarowka)

Enumerated authoritatively from function formals (not grep — grep misses several).

**HSItools (26):** `hsi_apply_mnf`, `hsi_bind_layers`, `hsi_bind_rows`, `hsi_calc_difference`,
`hsi_calc_ndi`, `hsi_calc_raba`, `hsi_calc_rabd`, `hsi_calc_ratio`, `hsi_calc_rcv`,
`hsi_calc_reflectance`, `hsi_calc_remp`, `hsi_calc_rmean`, `hsi_calc_rmedian`, `hsi_calc_rsd`,
`hsi_calc_stretch`, `hsi_coregister`, `hsi_destripe`, `hsi_find_extent`, `hsi_remove_continuum`,
`hsi_smooth_median`, `hsi_smooth_savgol`, `hsi_subset`, `hsi_subset_range`, `hsi_tiled`,
`hsi_write_metadata`, `hsi_write_scaled`

**zarowka (3):** `hsi_apply_reduction`, `hsi_calc_abundance`, `hsi_check_saturation`

**Excluded by construction — do not touch:**

| Function | Why |
|---|---|
| `hsi_mask` | Already done; it is the model (except the ordering fix above). |
| `check_dots_write` | Internal helper; its `filename` is data to inspect, not a write target. |
| `hsi_normalize` | Internal, unexported, lives inside `hsi_calc_reflectance.R`. Validate at the exported boundary only. |
| `hsi_read_metadata` | Reader — `filename` is an input path, there is no `overwrite`, and it already validates. |

Notes on three of the in-scope ones:
- `hsi_write_metadata` and `hsi_coregister` have **no `...`** — they still get both checks; only
  the `wopt_user` line is absent.
- `hsi_subset` and `hsi_subset_range` are exported but live in `utils.R` — a pre-existing §3.8
  deviation. **Sweep them like any other function; do not refactor the file split.** That is a
  separate decision for Maury.

## 5. Sweep procedure

1. **Baseline first.** Run the full HSItools suite and record it before changing anything — do
   not assume green. Then zarowka. **The working tree is not clean and that is expected** —
   `hsi_mask()` and its regenerated docs are uncommitted; read §7 before touching anything.
2. **Iteration one:** `hsi_mask` ordering fix + one simple function with `...` (suggest
   `hsi_calc_ndi`) + one no-dots function (suggest `hsi_write_metadata`, which exercises the
   Shape B shape). Show `devtools::test()` output, wait for Maury's go/no-go.
3. **Then batches** grouped by family (indices, smoothing, calibration, co-registration, …),
   full suite per batch, go/no-go between batches. Batch size is Maury's call (§6).
4. **zarowka independently.** Unlike the 2026-07-14 dots sweep, this one adds **no cross-repo
   dependency** — `rlang::check_*` belongs to rlang, not HSItools — so zarowka needs no HSItools
   reinstall and can be swept in any order.
5. `devtools::document()` after any roxygen touch — but this sweep should touch **no** roxygen:
   the `@param filename`/`@param overwrite` catalogue strings (§4.3) already describe the
   validated behaviour and need no edit.

## 6. Micro-decisions to confirm with Maury before coding

- **Tests: what shape?** Options: (a) two `expect_error()` per function (~58 new tests, pure
  boilerplate); (b) **one shared `helper-*.R` expectation helper** called once per function —
  the §5.5 precedent explicitly recommends this to kill write-tail boilerplate while keeping
  per-function coverage; (c) no new tests. **Recommend (b).** If (a) or (b): assert on
  `class = "rlang_error"` or a stable fragment, **never** `hsitools_error` (§5.6). Section:
  `Input validation`.
- **Batch size** for the sweep (one function per iteration vs family batches).
- **Accepted behaviour change:** `check_bool()` rejects `overwrite = 1` / `overwrite = 0`.
  Nothing in either repo does this, but user code in the wild might. Presumed acceptable
  pre-1.0 — confirm.

## 7. In flight — hands off

`hsi_mask()` is new, complete, and **uncommitted**. It is finished work, not work in progress:

- **Its roxygen is final** (Fable, 2026-07-17). `devtools::document()` has been run and is clean
  — no warnings, `tools::checkRd()` silent, `air format` no-op. Do not edit the block.
- **Expect a dirty working tree at baseline: ~22 modified files.** `document()` wrote
  `man/hsi_mask.Rd` (new), `NAMESPACE` (`export(hsi_mask)`), and **20 sibling `man/*.Rd` files**
  — every one a single line adding `hsi_mask()` to its "Other HSI Transformations" cross-links,
  because the function joined that `@family`. This is expected and verified. **Do not revert,
  clean, or "tidy" any of it**, and do not report it as unexplained churn. Re-running
  `document()` should now be a no-op.
- **Your only edit to `R/hsi_mask.R` is the §3 validation-order fix** (moving
  `check_string`/`check_bool` below the mask-layer check). Nothing else in that file is yours.
- **The geometry check is deferred** (Maury, 2026-07-17). A geometry mismatch raises terra's
  `simpleError` rather than an `hsitools_error`. Known, accepted, deliberate — not a gap to close.
- There is no `test-hsi_mask.R` yet. Not your scope.

## 8. Things that may break — stop and show Maury, do not improvise

- Any function where `filename` turns out **not** to be a plain output path (e.g. accepting a
  vector, or `NULL` as a sentinel) — `check_string()` would newly reject it. Flag, exclude, do
  not redesign.
- If a function validates `overwrite` implicitly further down (a hand-rolled `if (!is.logical…)`),
  the new check makes it dead code. Flag it; removing it is Maury's call, not a silent cleanup.

## 9. Out of scope

- CLAUDE.md amendments (the §3.2 validation-block convention gains two lines) — Maury's, after
  the sweep proves out. Never touch CLAUDE.md.
- The §3.8 `utils.R` deviation (`hsi_subset`/`hsi_subset_range`).
- `hsi_mask` roxygen, its geometry check, and its test file.
- Git operations of any kind — Maury commits (§0 rule 8).

## 10. Suggested first move

Read §2 of this file and the terra-facts section of
`dev-notes/2026-07-17_design_hsi-mask-ratified.md`. Run both baselines and record them. Confirm
the inventory in §4 against the installed packages yourself (formals, not grep). Put the §6
micro-decisions to Maury in one message. Then iteration one per §5.2.
