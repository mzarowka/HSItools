# Handoff: `check_dots_write()` sweep — scope correction, needs design confirmation

**Date:** 2026-07-14
**From:** Sonnet (mid-sweep, executing `2026-07-14_handoff-dots-check-sonnet.md`)
**To:** Fable
**Status:** Paused mid-sweep. Not blocked on a bug — blocked on a design read that belongs to
whoever owns this fix. Read the original handoff and the dev-note + addendum first; this
document only records what changed since.

---

## 1. What shipped, confirmed green

Baseline (this session, before any edit): HSItools `FAIL 0 | WARN 0 | SKIP 0 | PASS 476`;
zarowka `FAIL 0 | WARN 1 | SKIP 0 | PASS 117` (WARN is unmixR-internal, pre-existing, not ours).

- `check_dots_write(dots, filename, call)` added to `HSItools/R/utils-checks.R`, exactly the
  design in the original handoff §3, with one fix made during implementation (below).
- `hsi_calc_ndi()` and `hsi_calc_difference()` swept: `wopt_user <- rlang::list2(...)` moved
  into the validation block, followed by `check_dots_write(wopt_user, filename)`, per §4.
- One new test per function (`Input validation` section): unused argument with no `filename`
  aborts with `class = "hsitools_error"`.
- Full suite after: `FAIL 0 | WARN 0 | SKIP 0 | PASS 477`. Clean.

**Fix made to the helper during implementation** (handoff §3 flagged this exact edge as
something to "show Maury" — recording the result): an unnamed positional element in `...`
(e.g. `hsi_calc_ndi(x, bands, "stray")`) has `names(dots) == ""`. Interpolating that directly
into `{.arg {names(dots)}}` collapses to nothing under cli's inline markup *and* mis-renders
pluralization — the message came out `"Arguments were not used."` with no name shown at all,
plural, for a single offender. Fixed by substituting a literal placeholder for blank names
before interpolation:

```r
dots_names <- names(dots)
if (is.null(dots_names)) dots_names <- rep("", length(dots))
dots_names[dots_names == ""] <- "(unnamed)"
```

Verified all four shapes (single named, single unnamed, mixed, multiple named) render
correctly, and both silent-should-stay-silent cases (empty dots; dots present with `filename`
given) still pass through untouched. This is folded into the helper as shipped — not a design
question, just recording that the edge case was real and is handled.

## 2. The scope correction — new empirical finding

The handoff's inventory (§5 step 2) grouped functions by family and assumed all 15 HSItools
candidates share `hsi_calc_ndi`'s shape: compute a result, then

```r
if (filename != "") {
  terra::writeRaster(result, filename = filename, overwrite = overwrite, wopt = wopt)
}
```

Reading all 15 in full during the sweep surfaced a second shape the handoff didn't
distinguish: several functions pass `filename` and `wopt` **directly** into `terra::app()` or
`terra::focal()`, with **no separate guard** — e.g. `hsi_smooth_median()`:

```r
result <- terra::focal(x, w = window, fun = "median", na.rm = TRUE, cores = cores,
                        filename = filename, overwrite = overwrite, wopt = wopt)
```

This matters because of a fact already established once, for a different function, in the
original design session: `terra::predict()` validates `wopt` **unconditionally**, regardless
of `filename` — which is exactly why `hsi_apply_reduction()` (zarowka) was excluded from the
sweep. I probed `terra::app()` and `terra::focal()` the same way (terra 1.9.34, Windows) and
they behave identically:

```
terra::app(r, fun = "mean", filename = "", wopt = list(method = "fcls"))
  -> ERROR: [write] unknown option(s): method

terra::focal(r, w = 3, fun = "median", filename = "", wopt = list(method = "fcls"))
  -> ERROR: [write] unknown option(s): method
```

Both error identically whether `filename` is `""` or a real path. So for any function built on
this shape, `wopt` — and therefore `...` — is **never silently discarded**: an unknown name is
already loud (via terra, not cli), and a *valid* name (e.g. `names = "custom"`) is genuinely
read and would take effect. The entire premise the fix exists for — "`filename == ""` implies
`...` has no effect" — is false for these functions, for the same structural reason it's false
for `hsi_apply_reduction()`. Applying `check_dots_write()` to them would not just be redundant,
it would be a **regression**: it would newly reject a legitimate call like
`hsi_smooth_median(x, names = "custom")` with no `filename`, which works correctly today.

## 3. Revised classification of the original 15

**True sinks — the fix belongs here (7):** `hsi_calc_ndi` ✓ done, `hsi_calc_difference` ✓ done,
`hsi_calc_ratio`, `hsi_calc_rabd`, `hsi_calc_rcv`, `hsi_calc_stretch`, `hsi_calc_reflectance`.
All share the arithmetic-result-then-guarded-`writeRaster` shape; `wopt` genuinely never
reaches a terra call when `filename == ""`.

**Structurally exempt — confirmed by probe, not by inspection alone (8):** `hsi_calc_raba`,
`hsi_calc_remp`, `hsi_calc_rmean`, `hsi_calc_rmedian`, `hsi_calc_rsd`, `hsi_remove_continuum`,
`hsi_smooth_median`, `hsi_smooth_savgol`. All pass `filename`/`wopt` straight into
`terra::app()` or `terra::focal()`, which validate `wopt` unconditionally.

zarowka's two functions (`hsi_calc_abundance`, `hsi_check_saturation`) are unaffected by this
correction — both were already confirmed to use `terra::app()` for intermediate computation
**without** `wopt`, followed by a separate guarded `writeRaster()` for the actual write. Both
remain true sinks, in scope.

**Net: 9 functions need the fix (7 HSItools + 2 zarowka), not 17.** 8 of the original 15 in
HSItools are already correctly loud, via terra, and must not be touched.

## 4. The design question this raises — not decided, needs your read

The 8 exempt functions are **not silently broken**, but their error experience is worse than
the swept functions': an unknown argument today surfaces as terra's raw
`[write] unknown option(s): method`, not a cli-quality, house-styled message. That is a
UX/consistency question, not the bug this fix targets. Two shapes it could take, neither
started:

- **Leave them alone.** They are correct today; consistency of error *wording* across a
  15-function family that happens to have two different internal shapes is a nice-to-have, not
  a defect. Matches the fix's original, narrow framing (silent discard only).
- **Normalize anyway**, e.g. by validating `...` against each terra primitive's actual accepted
  option names before the call, so all 15 functions raise the same cli-styled message on a bad
  argument regardless of internal shape. This is closer to the *original* (b) design in the
  dev-note — the wopt allow-list — which was withdrawn specifically because it was redundant
  and terra-version-coupled **for the sink case**. It would not be redundant here, since these
  8 functions don't currently get a cli-quality message at all. But it reopens exactly the
  maintenance question the addendum used to kill option (b): an allow-list per terra primitive,
  coupled to terra's version.

I have not implemented either. This needs your call, not mine — it changes the shape of the
fix for 8 functions the original design never distinguished from the other 7.

## 5. What's blocked, pending your read

- Whether to proceed with the narrowed 9-function sweep as the complete fix (my working
  assumption, not yet acted on for anything beyond the 2 already done).
- Whether §4's UX question is in scope for this thread at all, or a separate backlog item.
- Batch/pacing for the remaining 5 HSItools functions + 2 zarowka functions — the original
  handoff's staging (batches grouped by family) doesn't map cleanly onto the corrected
  true-sink set, since 3 of the 4 originally-planned batches are now mostly or entirely exempt
  functions.

## 6. State of the repos

Uncommitted, both repos, nothing reverted: helper + `hsi_calc_ndi` + `hsi_calc_difference` +
their tests (HSItools, this thread); plus everything from earlier today's sessions (SNR/
saturation functions and tests, zarowka suite repair, DESCRIPTION edits, the dots-sink dev-note
and its addendum, `dev-notes/` gitignoring in both repos). Nothing committed anywhere — Maury's,
per house rule.

## 7. Suggested next move

Read §2–§4 above, rule on the two open questions in §4–§5, hand back a confirmed scope (9, or
9 + a UX-normalization follow-up) and, if needed, a revised batch plan for the remaining 5 + 2.

---

## 8. Ruling (Fable, 2026-07-14, same day)

Verified before ruling: one additional probe (terra 1.9.34) on the claim §2 left implicit —
whether *valid* wopt options take effect on the in-memory path of the exempt shape:

```
terra::app(r, fun = "mean",  filename = "", wopt = list(names = "custom"))  -> names honored
terra::focal(r, w = 3, ...,  filename = "", wopt = list(names = "custom"))  -> names honored
```

**Both honor valid options in-memory.** This strengthens the exemption beyond §2's framing:
for the 8 exempt functions, `...` with `filename = ""` is not merely "already validated by
terra" — it is a live, functioning interface (`names` demonstrably sticks). The stale-looking
comment in `hsi_calc_rmean.R` ("Set name (needed when not writing to file)") reflects that
function's own `NULL`-default interplay, not terra behaviour.

Rulings:

1. **Scope correction accepted. The sweep is 9 functions** — 7 HSItools true sinks (2 done, 5
   remaining: `hsi_calc_ratio`, `hsi_calc_rabd`, `hsi_calc_rcv`, `hsi_calc_stretch`,
   `hsi_calc_reflectance`) + 2 zarowka (`hsi_calc_abundance`, `hsi_check_saturation`). The 8
   exempt functions must not be touched — adding the check there is a confirmed regression,
   not a consistency win.
2. **§4's UX normalization is declined.** The exempt 8 are loud on unknown names (terra-voiced)
   and functional on valid names, in-memory included. Normalizing the *wording* of an error
   that already fires would require the terra-coupled allow-list the addendum withdrew, to fix
   a non-defect. The narrow, silent-discard framing stands. One residual accepted: a
   valid-but-pointless option (`datatype` with `filename = ""`) is silently accepted by
   terra on the exempt shape — harmless (the result is correct; only a storage intent with no
   storage is ignored), same as raw terra usage everywhere, not worth an intervention.
3. **Batching for the remainder:** the 5 HSItools sinks as **one batch** (the edit is proven
   on two functions of identical shape; `hsi_calc_stretch`/`hsi_calc_reflectance` need only
   placement care within longer validation blocks, per the original §4 rule), full suite
   after; then zarowka's 2 (after Maury reinstalls HSItools locally), full suite after.
4. Minor doc note, non-blocking, for a future pass: the exempt 8's `@param ...` string says
   "passed to `terra::writeRaster()`" — as-built they pass to `terra::app()`/`terra::focal()`
   `wopt`, which is live in-memory. Not worth a sweep of its own; fold into any future roxygen
   touch of those files.
