# HSItools Ecosystem Development Guidelines (CLAUDE.md)

> **Version 1.11.0 — 2026-09-10.** This file is the **canonical source** of development
> conventions for the HSItools ecosystem. Claude Code reads it automatically at session
> start; the claude.ai `hsitools-development` skill is a mirror refreshed from this file
> at milestone boundaries (the skill adds only trigger frontmatter). If the two disagree,
> this file wins. Check the version here at the start of every session.
>
> Consolidated 2026-07-02. Supersedes CODING_GUIDELINES.md, TESTING_GUIDELINES.md,
> ROXYGEN_GUIDELINES.md and scattered design notes as the single reference.
> These rules are non-negotiable house style; do not improvise alternatives.

---

## 0. How to work on this project (read first)

These behavioral rules outrank everything below. Violating them wastes the user's time even if the code is correct.

1. **Approach before code.** When discussing code, first talk approach and design; never code immediately. Wait for agreement on the contract, then implement.
2. **One change at a time.** Propose a single change, let the user run tests, then continue. Never batch several unrelated edits into one response.
3. **No hardcoding unless absolutely necessary.** Strive for simple, reusable code.
4. **Do not generalize speculatively.** Defer convenience branches and abstractions until reuse is confirmed. If tempted to add "flexibility", ask first.
5. **Do not settle open design questions unilaterally.** See §10 — several designs are deliberately unresolved; ask before locking them.
6. **The user pushes back on overcomplication** ("we're overcomplicating this") — treat that as a signal to strip the design down, not to defend it.
7. **Verification runs where R lives.** If this session can execute commands in the repo (Claude Code): after each single change, run `Rscript -e "devtools::test()"` (plus `devtools::document()` whenever a roxygen block or signature changed), show the output, and wait for Maury's go/no-go before the next change. If this session cannot execute R (chat sessions — sandboxes lack the system dependencies and CRAN access, confirmed empirically 2026-07-11): make source edits and hand off patches; Maury runs all verification locally. In either mode, never claim a verification step ran unless its output was actually shown or Maury reported it.
8. **Git belongs to Maury.** Never run git commands that modify state — no `add`, `commit`, `checkout`, `branch`, `merge`, `push`, `stash`, `restore`, or similar. Read-only inspection (`git status`, `git diff`, `git log`) is permitted for reviewing work. Maury performs all commits and branch operations himself.
9. **This file carries conventions only — and is hands-off.** Never add milestone state, session notes, TODOs, or roadmap items here; those live exclusively in dated documents (`YYYY-MM-DD_*.md`, see §9). Never modify this file unless Maury explicitly asks for the change.

---

## 1. The ecosystem

Three packages, one workflow:

| Package | Role |
|---|---|
| **HSItools** (`mzarowka/HSItools`) | Core R package: processing, analysis, visualization of hyperspectral raster data. Stable, CRAN-quality surface. |
| **zarowka** | Front-end scaffolding layer: workflow templates and experimental functions. Battle-tests new functionality before promotion to HSItools. |
| **hsical** | Standalone Shiny app (packaged, `hsical::run_app()`): scan-session calibration companion and logger. Never touches spectral data. |

**Promotion flow:** new functionality lands in zarowka first; it is promoted to HSItools only once the pattern is proven in real analyses.

**Sensor-, manufacturer-, and MATERIAL-agnostic (imperative):** HSItools must work with hyperspectral data regardless of material provenance and nature — sediment cores, outcrops, mineral matter, biological specimens, art conservation — and regardless of which sensor or vendor produced it. The current lab context (Specim VNIR/SWIR pushbroom scanners, mostly lake sediments) is *incidental*, not a design constraint: never bake sediment-, Specim-, Lumo-, or wavelength-range-specific assumptions into HSItools function contracts. Domain and instrument specificity lives in templates (zarowka), thin ingest adapters, and the logging app (hsical) — never in the analysis functions. When a proposed contract only makes sense for cores or for one vendor's output, redesign it.

**Design philosophy (non-negotiable):**
- **Design-first.** Discuss function contracts and key decisions before writing any code. When talking code, talk approach first — never code immediately.
- **Always atomic, single-responsibility functions with the simplest possible input and output.** As few assumptions and decisions per function as possible. One clear input shape in, one clear output shape out. No convenience branches embedded inside functions. Generalization is deferred until reuse is confirmed.
- **Lean signatures.** Every parameter must earn its place.
- **Loud failure.** Invalid input fails immediately via `cli::cli_abort()`, never silently coerces.
- **Assume data is too big for memory.** Default to lazy, file-backed, terra-native processing. `in_memory = TRUE` is an opt-in for small data, never the default code path.
- Do not hardcode values unless absolutely necessary; strive for simple reusable code.

---

## Repo map and mechanics (HSItools repo)

> Machine-discoverable orientation for the HSItools repository. Initially auto-generated
> by Claude Code (2026-07-13), then corrected and merged by hand. This section describes
> the repo *as built*; the numbered sections are normative — on any conflict, they win.

### What this is

HSItools is an R package for processing, analyzing, and visualizing hyperspectral imagery — sensor-, manufacturer-, and material-agnostic by design (§1 is imperative; the original auto-generated description baked in core-scanning framing and was rewritten). It takes raw digital-number captures through radiometric calibration to reflectance, then spectral smoothing, continuum removal, spectral indices (RABD, RABA, NDI, ratio, difference), spatial co-registration, and profile/spectrum extraction. Built on `terra` for file-backed raster processing so datasets larger than memory are supported.

### Commands

All commands run from the package root in R (this is a standard R package, not a script project).

```r
devtools::load_all()          # load package for interactive dev
devtools::test()              # run full test suite (testthat edition 3)
devtools::test_active_file()  # run the currently open test file
testthat::test_file("tests/testthat/test-hsi_calc_rabd.R")  # run a single test file
devtools::check()             # full R CMD check
devtools::document()          # regenerate NAMESPACE and man/*.Rd — required after touching any roxygen block
```

Formatting is owned by **air** (see §2): run `air format` on any file you edit before handing work back; `air.toml` at the repo root marks the project and pins settings. Never hand-format.

Vignettes are Quarto (`.qmd`) documents built via `quarto`, listed under `VignetteBuilder` in `DESCRIPTION`. Windows CI skips vignette building/checking (`--ignore-vignettes`) due to an upstream quarto knitr-engine limitation losing the temp package library on Windows runners — see the comment block in `.github/workflows/R-CMD-check.yaml`. Vignette builds are still verified locally on Windows and in CI on macOS/Linux.

`Suggests` packages (`mirai`, `carrier`, `sf`, `spacetime`, etc.) are optional; tests that need them guard with `testthat::skip_if_not_installed()` rather than relying on `_R_CHECK_FORCE_SUGGESTS_`, which CI disables intentionally.

Test fixture data lives under `inst/testdata/` (`capture/` for raw captures plus white/dark refs, `products/` for precomputed reflectance/continuum-removed/median/savgol outputs) and is referenced in tests via `system.file(package = "HSItools", "testdata/...")`. Fixture chain and wavelength constraint: §5.2.

### Architecture

**One exported function per file in `R/`**, named `hsi_<verb>_<noun>.R` matching the function it defines (e.g. `hsi_calc_rabd.R` defines `hsi_calc_rabd()`). Shared internals live in `utils.R` (wavelength lookup, unit conversion, the `hsi_metadata` constructor/validator) and `utils-checks.R` (the `check_*` family of input validators — all `@noRd`, not exported). Functions are grouped via roxygen `@family` tags — check the `@family` tag on a function to find its siblings rather than guessing from filenames.

**Processing pipeline** (each stage a `hsi_calc_*`/`hsi_smooth_*` function taking and returning a `SpatRaster`, chainable with `|>`):

1. `hsi_calc_reflectance()` — raw DN → reflectance from white/dark references. Three calibration paths depending on `darkspec`/`tint` (single-session, matched-darks (recommended), scaled-dark fallback); exact formulas in the roxygen `@details` and §6.1.
2. `hsi_smooth_median()` / `hsi_smooth_savgol()` — spectral smoothing.
3. `hsi_remove_continuum()` — continuum removal.
4. `hsi_calc_rabd()` / `hsi_calc_raba()` / `hsi_calc_ndi()` / `hsi_calc_ratio()` / `hsi_calc_difference()` / `hsi_calc_remp()` — spectral indices. The `spectral_indices` data object (`R/data.R`) is a lookup table of literature-defined index presets; its columns map directly to the argument names of the corresponding `hsi_calc_*` function, so a row can be spread directly into a call.
5. `hsi_extract_profile()` / `hsi_extract_spectrum()` — pull spatial profiles or per-pixel spectra out of a processed raster.

**Wavelength addressing**: band names on a `SpatRaster` are the canonical wavelength labels (numeric strings in nm). `wavelength_position()` (in `utils.R`) resolves requested wavelengths to band indices by nearest match and is the shared primitive underneath `hsi_subset()`, `hsi_subset_range()`, and the index functions — read it first when working on anything wavelength-related.

**Spatial calibration / co-registration toolchain** (`@family HSI Co-registration`): `hsi_calibration_from_scale()` / `hsi_calibration_from_dims()` / `hsi_calibration_direct()` establish pixel-to-unit scaling; `hsi_set_extent()` / `hsi_find_extent()` / `hsi_pixels_to_units()` / `hsi_drop_crs()` manipulate georeferencing; `hsi_check_gcp()` → `hsi_match_gcp()` → `hsi_coregister()` is the ground-control-point workflow for aligning one sensor's raster to another's grid (GCPs embedded into a GDAL VRT, then warped via `sf::gdal_utils()`). **`hsi_coregister()` requires `x` to have a file source on disk — GDAL warps files, not in-memory rasters.**

**Capture metadata sidecars** (`@family HSI Metadata`): `hsi_create_metadata()` builds an unvalidated `hsi_metadata` list (`new_hsi_metadata()` + `validate_hsi_metadata()` in `utils.R`); `hsi_write_metadata()` / `hsi_read_metadata()` round-trip it to a flat YAML file (32-field schema, only `name` required). `hsi_read_metadata()` checks `schema_version` on read and aborts on an unsupported version — bump the schema version and the check together if the field set changes. Interop rules: §8.

**Parallel/tiled processing** (`hsi_tiled.R`): splits a `SpatRaster` into tiles via `terra::makeTiles()`, processes each tile in a separate `mirai` daemon, then reassembles via a GDAL VRT mosaic. The caller must initialize daemons first (`mirai::daemons(n)`); the function never spins them up or down. `fun` must be a fully self-contained anonymous function with explicit `HSItools::` namespacing and only literal argument values — variables from the calling environment are **not** visible inside `mirai` workers. This is the pattern to reach for whenever data won't fit in memory; every other `hsi_calc_*`/`hsi_smooth_*` function processes the whole raster in one shot (in-memory or file-backed via `terra`, but not chunked). Implementation conventions: §3.10.

### File-backing conventions

Most transformation functions share this signature tail: `filename = ""` (write to disk only if a path is given; otherwise stay in-memory or file-backed by a temp file), `overwrite = FALSE`, `...` passed to `terra::writeRaster()`. Some (`hsi_calc_reflectance`) additionally expose `in_memory` to force full in-RAM processing.

Two temp-file lifetimes coexist in `R/`, and the choice is **deliberate, not accidental** — read the inline comments before changing either pattern:

- `withr::local_tempdir()` / `withr::local_tempfile()` for temp files whose lifetime must not outlive the current call.
- Plain `tempfile()` (session-scoped, not cleaned on function exit) when the temp file **is** the backing store of the returned `SpatRaster` (`hsi_tiled()`, the no-`in_memory`/no-`filename` branch of `hsi_calc_reflectance()`), or when a `mirai` daemon may still hold an open GDAL handle to it after the function returns.

The normative rule and its documented exceptions live in §3.6; this paragraph is the as-built view of the same policy.

### Error/warning conventions (pointer)

All package-raised conditions use `cli::cli_abort()` / `cli::cli_warn()` with `class = "hsitools_error"` / `"hsitools_warning"` so callers can distinguish them programmatically from terra/GDAL or base-R conditions. Validate inputs at the top of exported functions with the `check_*` helpers in `utils-checks.R` — current inventory: `check_spatraster`, `check_spatvector`, `check_numeric`, `check_wavelengths`, `check_geom_type`, `check_one_of`, `check_has_cols`, `check_crs_null`, `check_list_has`, `check_data_frame`, `check_spatraster_list`, `check_dots_write` — never ad hoc checks. They auto-detect argument name and call site via `rlang::caller_arg()` / `rlang::caller_env()`, and list-input checkers report *all* offending elements in one error rather than failing on the first. Normative rules, carve-outs, and call-attribution: §3.2–§3.3a.

---

## 2. Language and style baseline

### 2.0 Anti-pattern table — scan before writing any code

| Never write | Always write |
|---|---|
| `x %>% f()` | `x |> f()` |
| `function(i) i + 1` | `\(i) i + 1` |
| `for (i in seq_along(x)) ...` / `lapply()`, `sapply()`, `vapply()` | `purrr::map()` / `purrr::map_*()` / `purrr::pmap()` |
| `stop()`, `rlang::abort()`, `rlang::warn()`, `rlang::inform()` | `cli::cli_abort()`, `cli::cli_warn()`, `cli::cli_inform()` |
| `library(pkg)` in package code; bare `rast()` | `terra::rast()` — explicit `::` everywhere |
| `dplyr::tibble()` | `tibble::tibble()` (canonical home package) |
| `terra::names(x) <- ...` | `names(x) <- ...` (base replacement form) |
| `terra::extract(...)[[2]]` | `terra::extract(...)[["col_name"]]` |
| `tempfile()` / `tempdir()` bare | `withr::local_tempdir()` (exceptions in §3.6 only) |
| `HSItools:::helper()` inside `R/` | bare `helper()` |
| `return(result)` at function end | bare `result` as final expression |
| Loading whole raster into memory by default | Lazy, file-backed terra pipeline; `in_memory = TRUE` is opt-in |
| `cli::cli_abort()` inside a purrr lambda | Collect offenders first (`purrr::map_lgl()`), then **one** abort after the loop listing all offending indices — lambda-frame aborts blame purrr internals and force whack-a-mole fixes |
| `match.arg()` | `check_one_of()` — cli-consistent errors, same semantics |
| `terra::app(x, fun = <inline lambda>, cores = cores)` | rebind the closure to a minimal environment first (§3.10) — the inline lambda drags the SpatRaster to every worker, measured 5.5× slower than serial |
| `cores` on a built-in fun (`terra::app(fun = "mean")`, `terra::focal(fun = "median")`) | no `cores` — built-ins are C++/TBB and ignore it; size them with `terraOptions(threads = )` |
| `raw$schema` on deserialized/external data | Spell the full name — `raw$schema_version` or `raw[["schema_version"]]` — `$` partial matching silently returns the wrong element on raw external data |
| `rlang::list2(...)` into `wopt` with no dots check (guarded-write functions) | `wopt_user <- rlang::list2(...)` then `check_dots_write(wopt_user, filename)` in the validation block — a silent `...` sink hides typos and removed arguments (§3.2 has the exemption for direct-to-terra functions) |
| `terra::crop()` on a raw integer capture | `terra::window()` — lazy ROI, reads and writes nothing, so it cannot relabel a saturated reading as NoData |

Applies to all source, test, template, and example code in every package:

- **Native pipe `|>` only.** Never `%>%`.
- **Anonymous functions as `\(i)`**, never `function(i)`.
- **No `for` loops, no apply family.** Use `purrr::map*()` / `purrr::pmap()` etc. for iteration.
- **tidyverse first**: prefer `tibble`, `dplyr`, `purrr` where applicable; tidyverse style guide throughout.
- **terra for all raster operations.** `sf` is allowed for vector/GDAL work.
- **Explicit `package::function()` double-colon calls everywhere**, always from the canonical home package (`tibble::tibble()`, not `dplyr::tibble()`).
- **Exception — bare internal calls**: within a package's own `R/` files, call internal functions bare (no `HSItools:::`). Self-referential `:::` raises an R CMD check NOTE.
- **Migrate `rlang::` alert calls to `cli::` wherever possible.** `rlang::abort()`, `rlang::warn()`, `rlang::inform()` are never used for messaging.
- **Formatter: air** (Posit R formatter). Do not manually align after `<-`, `=`, or roxygen tags. air is the single source of truth for formatting — do not fight it. `air.toml` at the repo root marks the project and pins settings; sessions that can execute commands run `air format` on files they edit before handing work back. Never conclude from a missing config that hand-formatting is expected.
- **Windows-aware paths**: development happens on Windows/PowerShell. Use `normalizePath(..., mustWork = FALSE)`; be alert to cross-drive path issues.

### terra gotchas (memorize)

- Read layer names with `terra::names(x)`; **assign with base `names(x) <-`** — the terra replacement form is not exported and errors at runtime.
- `terra::extract()`: always use named column access (`[["col_name"]]`), never positional (`[[2]]`); return shape varies by input type (matrix vs SpatVector).
- `terra::writeRaster()` errors on `filename = ""` — always guard with `if (filename != "")`.
- `terra::rast()` is lazy (stores paths, not data). Any SpatRaster backed by a temp file must be re-backed (reassigned from `terra::writeRaster()` return) before that temp file is cleaned up.
- `terra::subset()` and `terra::mask()` can force per-scene materialization — see §7 "Bind first, trim later".
- **Never `terra::crop()` a raw integer capture — use `terra::window()`.** A crop that materialises writes the copy in the source datatype and reserves the datatype maximum as NoData, so genuine saturated readings read back as `NA`. Whether it materialises depends on terra's memory budget, so the same script yields different products on different machines and terra versions. `terra::window()` sets a lazy region of interest; downstream functions read it straight from the source, and nothing is written. This covers inspection too — read rows with `terra::values(x, row =, nrows =)`. A physical subset, if genuinely needed, must be written float (`datatype = "FLT4S"`); that is correct but costly, measured 25× slower than windowing and dominated by the serial read-back of terra's default block layout rather than by the write. (Cost several debugging passes on a saturated uint16 ENVI cube, 2026-08-18; GKUT VNIR 2026-09-09, 24,816 saturated cells silently lost to a materialised crop.)
- **`cores` and `threads` are different mechanisms.** `cores` (in `terra::app()`, `focal()`, `predict()`) spawns PSOCK worker processes and only does anything when `fun` is a **user-supplied R function**; built-in character funs (`"mean"`, `"median"`, `"sd"`, …) run in C++ and ignore it silently. Those built-ins are threaded internally via TBB, sized by `terra::terraOptions(threads = )`, which defaults to 16 regardless of machine size. Never add a `cores` argument to a function whose `fun` is a built-in name — it is a silent no-op (removed from `hsi_smooth_median()` for exactly this reason, 2026-08-19).

---

## 3. Function structure (HSItools/zarowka source code)

### 3.1 Canonical argument order

Every signature follows this exact left-to-right sequence; inapplicable arguments are simply omitted, relative order preserved:

```r
function(
  x,            # 1. Primary raster input (always first)
  y,            # 2. Secondary raster input
  whiteref,     # 3. Named reference rasters,
  darkref,      #    in logical pipeline order
  tint,         # 4. Acquisition metadata coupled to references
  <processing>, # 5. Substantive algorithm arguments, logical order
  index_name,   # 6. Output layer naming
  na.rm,        # 7. NA handling
  in_memory,    # 8. Memory control
  cores,        # 9. Parallelism
  filename,     # 10. Output path (write args last)
  overwrite,    # 11. Overwrite flag
  ...           # 12. Pass-through to terra::writeRaster()
)
```

- Never reorder to group optional vs required — position encodes meaning.
- `filename` defaults to `""` (in-memory); `overwrite` defaults to `FALSE`.
- `...` exists only when the function can write via `terra::writeRaster()`.

### 3.2 Validation block

Every exported function opens with a self-contained validation block before any computation, using early exits so the happy path is never nested:

- All `check_*` calls first, in argument-order sequence. `check_spatraster()` is the canonical raster validator (do not mix with plain `inherits()` checks — reconcile toward `check_spatraster`).
- `check_one_of()` is the canonical validator for enumerated strings. `match.arg()` is not used anywhere in the codebase (audited 2026-07-06) — its base-R errors break cli consistency.
- **Guarded-write Shape A functions end the validation block with the dots check** (decided 2026-07-14, swept same day): `wopt_user <- rlang::list2(...)` immediately followed by `check_dots_write(wopt_user, filename)`. With no `filename`, nothing in `...` can take effect, so a non-empty `...` is a misspelled or removed argument and must abort *before* computation. **Exemption (do not "fix"):** functions that pass `filename`/`wopt` directly into a terra primitive (`terra::app()`, `terra::focal()`, `terra::predict()` — e.g. the smoothers, per-pixel statistics, `hsi_remove_continuum`, `hsi_tiled`, zarowka's `hsi_apply_reduction`) never get this check. terra validates `wopt` names unconditionally and honours valid options even in-memory (probed 2026-07-14, terra 1.9.34: `names = "custom"` sticks at `filename = ""`), so their `...` is a live interface and adding the check would reject working calls.
- **`cli::cli_abort()` is the only permitted validation error** — never `stop()`, never `rlang::abort()`.

```r
cli::cli_abort(
  c("{.arg x} must be a {.cls SpatRaster}.",
    "i" = "Got a {.cls {class(x)}} instead."),
  call = call
)
```

### 3.3 Condition classes and call attribution (decided 2026-07-06)

**Umbrella class only.** Every authored `cli::cli_abort()` in the `check_*` helpers carries `class = "hsitools_error"`, **hardwired inside the helper** — do not add a `class` parameter to helpers (speculative generalization). Rationale: the sole concrete need is CRAN-safe test assertions (`expect_error(class = "hsitools_error")`); snapshots don't run on CRAN and message matching is brittle. There is **no category-level taxonomy** (`_input`, `_wavelength`, …). A more specific class may be added later, individually, only when a test genuinely needs to distinguish two failure modes from the same call — and it must then be supplied *alongside* the umbrella (`class = c("hsitools_error_x", "hsitools_error")`). A parallel **`hsitools_warning` umbrella is settled convention** (confirmed 2026-07-09, swept 2026-07-10): every authored `cli::cli_warn()` site carries `class = "hsitools_warning"`, same single-umbrella logic — no category taxonomy, hardwired at the site.

**rlang type-check carve-out (decided 2026-07-08).** rlang's exported type-check helpers (`rlang::check_string()` and siblings) are sanctioned for validating simple scalar types where no house `check_*` helper exists. Their errors stay rlang-classed — they do **not** carry `hsitools_error`, and this is accepted. Never wrap them, never write house duplicates (no house `check_string()`). Test consequence in §5.6.

**Call attribution rule.** Truly *internal* helpers (called by exported functions; e.g. `wavelength_sub`, `to_um`, `from_um`, the `check_*` family) take the standard tail `arg = rlang::caller_arg(x), call = rlang::caller_env()` and pass `call = call` into every abort, so errors blame the function the user actually called. Helpers calling helpers thread `call = call` explicitly down the chain. **Exported** functions abort under their own name — never add `call` threading to them; an exported function blaming its caller is wrong.

### 3.3a Messaging — always cli

| Situation | Function |
|---|---|
| Validation failure, bad input | `cli::cli_abort()` |
| Recoverable issue, continues | `cli::cli_warn()` |
| Progress / informational | `cli::cli_inform()` |
| Bullet lists in messages | `c("!" = ..., "i" = ...)` style |

### 3.4 Return values — exactly two shapes

**Shape A — compute functions (default).** Computes a SpatRaster; writing is optional via `filename`. Result returned as the final bare expression — no `return()`, no `invisible()`:

```r
  # Validate inputs
  check_spatraster(x)
  # ...other check_* calls...
  wopt_user <- rlang::list2(...)
  check_dots_write(wopt_user, filename)

  # ...computation...

  # Build write options
  wopt_default <- list(names = index_name)
  wopt <- purrr::list_modify(wopt_default, !!!wopt_user)

  # Write to file
  if (filename != "") {
    result <- terra::writeRaster(result, filename = filename, overwrite = overwrite, wopt = wopt)
  }

  # Return result
  result
```

`wopt_user` is captured in the **validation block** (so `check_dots_write()` fires before any computation); the `# Build write options` block keeps `wopt_default` and the merge. Functions that hand `filename`/`wopt` directly to a terra primitive skip the check entirely — see the exemption in §3.2.

The `result <-` reassignment in the guard is **required** whenever `withr::local_tempdir()` manages intermediates — it re-backs the lazy SpatRaster to the permanent file before withr cleanup orphans the pointer.

**Shape B — write-primary functions** (e.g. `hsi_write_scaled`): sole purpose is encoding/writing an existing raster; return `invisible(x)` so it sits quietly in a pipeline.

`return()` is permitted **only** for early exits inside the validation block.

### 3.5 wopt construction

Always the same two-variable merge; names are always `wopt_default` and `wopt_user`:

- `wopt_default` contains at minimum `names` (layer name).
- `wopt_user <- rlang::list2(...)` — captured in the validation block, immediately followed by `check_dots_write(wopt_user, filename)` in guarded-write functions (§3.2).
- `purrr::list_modify(wopt_default, !!!wopt_user)` — user values win.
- Never specify `datatype` in `wopt_default` — terra chooses — unless a documented reason exists (e.g. SWIR float32 requirement, §7).
- Never expose `wopt` directly to users; `...` is the interface.

### 3.6 Temp files — always withr

`withr::local_tempdir()` is the only permitted temp-file pattern; never bare `tempfile()`/`tempdir()`. `withr` goes in `Imports` if any exported function uses it. Do not manually `unlink()` — withr cleans up.

**Documented exceptions:**
- When the temp file is the backing store of the *returned* SpatRaster (`in_memory = FALSE`, `filename = ""`), withr cannot own it — use plain `tempfile()` with an explicit comment.
- `hsi_tiled()` uses session-lifetime `tempfile()` + `dir.create()` deliberately, to avoid Windows GDAL handle races with `withr::local_tempdir()`.

### 3.7 Comments

- No section-divider comments (`# ---- x ----`) in source files (test files may use them).
- Full-sentence comments explaining *why* or labelling a logical step.
- **Canonical structural comments — never vary the wording:**

| Block | Comment |
|---|---|
| `check_*` calls | `# Validate inputs` |
| wopt merge | `# Build write options` |
| filename guard | `# Write to file` |
| final bare result | `# Return result` |

### 3.8 File layout

- One exported function per file; file name matches function name.
- Internal helpers live in `utils.R`.
- Tests mirror source structure: `test-hsi_calc_x.R` ↔ `R/hsi_calc_x.R`.

### 3.9 tryCatch policy

Apply `tryCatch` only when all three hold: third-party code can error on *legitimate* input data; the failure maps cleanly to `NA`; pre-validation is impractical. (Established precedent: `hsi_remove_continuum` wrapping `prospectr::continuumRemoval()`.) Everywhere else, fail loudly.

### 3.10 Parallelism

**`terra::app(cores = )` is the backend for per-pixel spectral operations** (settled 2026-08-19, after measurement on GKUT and GRF17). Any exported function that hands a **user-supplied R function** to `terra::app()` takes `cores` at §3.1 slot 9, validated with `check_numeric(cores, len = 1, positive = TRUE)`. Current holders: `hsi_smooth_savgol()`, `hsi_remove_continuum()`, `hsi_calc_raba()`, `hsi_calc_remp()`. Built-in character funs never get it (§2 terra gotchas).

**The minimal-environment rebind is mandatory and unconditional.** `terra::app()` serializes `fun` to its workers once per write block, and a closure serializes together with its environment — a lambda defined inline inside an exported function carries that call frame, including the SpatRaster, which serializes to ~40 MB regardless of raster size. Measured consequence of the naive pass-through: **5.5× slower than single-threaded**. Immediately before the `terra::app()` call, rebind:

```r
# `terra::app(cores = )` serializes `fun` to its workers once per write block:
# the frame drags `x` along, and a SpatRaster serializes to ~40 MB regardless
# of raster size, which measured 5.5x *slower* than single-threaded. The body
# needs nothing beyond `pkg::fun()`, and `::` lives in base.
environment(my_fun) <- rlang::new_environment(
  data = list(<only what the body references>),
  parent = baseenv()
)
```

`data = list()` is valid and **load-bearing** when the body references nothing from the frame (`hsi_calc_raba()`): its job is removing the fat environment, not adding to it — never "clean it up". `parent = baseenv()` suffices because `::` lives in base and namespaces serialize by reference; this makes explicit `pkg::` namespacing in the body load-bearing too, not just house style. Applied unconditionally, including at `cores = 1`, so there is one code path (§0 rule 4). Verified: `tryCatch` handlers survive the trip and fire on workers; outputs are bit-identical to serial.

**`mirai` remains available for genuinely custom orchestration**, but is no longer the default answer. `hsi_tiled()` pattern (unchanged, fate open — §10): `mirai::mirai_map()` with `.args = list(fun = fun)` for explicit argument passing (never rely on environment scoping); `carrier::crate()` for worker environment isolation; `terra::makeTiles()` with corrected tile-size conversion; VRT-based mosaic; `BIGTIFF=YES` hardcoded for mosaic intermediates. Measured 2–5× **slower** than a direct call for per-pixel ops on both narrow and wide rasters, with ~99 % of the cost in the serial VRT→TIFF mosaic read-back, not in `makeTiles()`.

**terra version floor (performance, not correctness):** from terra 1.9-46 the built-in focal statistics `max`/`min`/`median`/`modal`/`sd` are TBB-threaded (previously only `sum`/`mean`), which makes `hsi_smooth_median()` ~3.9× faster with no code change; values are bit-identical either way. Templates set `terra::terraOptions(threads = )` to the core count, since terra's own default caps at 16.

### 3.11 S3 serialization — readers bypass the constructor

Functions that deserialize an S3 object from disk assemble it via `structure(raw, class = "...")` and then run the shared validator — **never** through the constructor. Constructors stamp provenance fields (e.g. `schema_version`); re-running one on read would overwrite what the file actually says, defeating any schema gate. Pattern established by `hsi_read_metadata()` (2026-07-08).

---

## 4. Roxygen documentation rules

### 4.1 Tag order (canonical; `@export` always last, `@family` always first)

```
Title (one line, no period)
@family <Family Name>          — Title Case, consistent across family
@param ...                     — in function signature order
@returns ...                   — use @returns, never @return
@description                   — optional; omit when title is self-sufficient; never duplicate title
@details                       — optional; algorithm detail, caveats, references
@seealso                       — between @details and @examples when present
@examples                      — always \dontrun{} when touching files or real data
@export
```

### 4.2 @param format

`Type. Description. Default `value`.` — capitalised type descriptor, every line ends with a period, defaults in backticks. Vector types state length (`Numeric vector of length 2. ...`). terra/sf objects use the markdown link form, never prose:

```r
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
```

### 4.3 Standard parameter catalogue (verbatim strings — open-ended)

The catalogue is **not closed**: new functions and parameters will be added. When a new parameter recurs across functions, coin one canonical string in the same `Type. Description. Default \`value\`.` format, use it verbatim everywhere, and propose adding it to this catalogue. Never invent a second phrasing for a parameter that already has an entry.

| Param | Canonical string |
|---|---|
| `x` | `` A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data. `` |
| `whiteref`/`darkref` | Standard SpatRaster opener + `Must have the same bands and wavelengths as `x`.` |
| `filename` | `` Character. Output filename. Default `""` keeps result in memory. `` |
| `overwrite` | `` Logical. Overwrite existing file. Default `FALSE`. `` |
| `...` | `` Additional arguments passed to [`terra::writeRaster()`]. `` |
| `index_name` | `` Character. Name for the output layer. Default `NULL`. `` (never function-specific qualifiers) |
| `na.rm` | `` Logical. Remove `NA` values. Default `TRUE`. `` |
| `in_memory` | `` Logical. Process entirely in RAM. Default `FALSE`. Set `TRUE` only when data fits comfortably in available memory. `` |
| `cores` | `` Positive integer. Number of parallel cores. Default `1`. `` |
| `bands` | `` Numeric vector of length 2. Wavelengths in nm. `` |
| `continuum_edges` | `` Numeric vector of length 2. Continuum anchor wavelengths in nm. `` |
| `window` | `` Positive odd integer. Focal window size. Default `3`. `` |

Canonical parameter names: `x`, `y`, `whiteref`, `darkref`, `tint`, `filename`, `overwrite`, `index_name`, `na.rm`, `bands`, `continuum_edges`, `absorption_band`, `index_type`, `in_memory`, `fun`, `window`, `n_tiles`, `cores`.

### 4.4 @returns

`` A [`SpatRaster`][terra::SpatRaster-class] with <x> values. `` — or the appropriate type (`A [tibble][tibble::tibble] with columns: ...` with `\item{}{}` list; `A named list containing: ...`; `` `NULL`, called for side effects. ``).

### 4.5 Examples

`\dontrun{}` always; show (1) minimal call, (2) call with `filename`/`overwrite`. Standard data load line: `x <- terra::rast("REFLECTANCE_testdata.tif")`. Result objects named `x_<suffix>` matching the function suffix (`x_rabd`, `x_savgol`).

### 4.6 @family catalogue

`HSI Transformations` (all `hsi_calc_*`, `hsi_smooth_*`, `hsi_remove_continuum`, `hsi_write_scaled`) · `HSI Calibration` · `HSI Extraction` · `HSI Diagnostics` · `Plotting` · `Utilities`. Catalogue is open — new families may be added as the package grows; keep Title Case and consistency within a family.

### 4.7 Internal helpers

`@noRd` (not `@export`); document `@param`/`@return` for developers. `arg`/`call` rlang error-helper params are always last with the standard one-liners (`Auto-detected via [rlang::caller_arg()]` / `[rlang::caller_env()]`).

---

## 5. Testing standards (testthat 3e)

Calibrated against r-pkgs (2e) test design and testthat 3e practice (2026-07-02 audit). Guiding principles: test the **external interface** only; test **each behaviour in one and only one test** (multiple expectations per test are fine — one behaviour is not one expectation); tests are **hermetic** (self-contained setup and teardown; a fresh session + `devtools::load_all()` must be able to run any single test).

### 5.1 Structure

- One test file per exported function: `test-<function_name>.R`, mirroring `R/`.
- File opens with a comment block (what the function does, key contracts), then `## Setup ----` loading shared fixtures **once at top level**. Fixtures are read-only — never mutate a top-level fixture inside a test; derive per-test copies via `terra::setValues()` / `terra::subset()`.
- Never `source()` inside `tests/testthat/`. Shared expectation helpers live in `tests/testthat/helper-*.R` (auto-loaded by testthat and `devtools::load_all()`).
- Sections in this fixed order (omit only if genuinely N/A), RStudio section style:
  `Output type` → `Output dimensions` → `Band names` → `Value sanity` → `File writing` → `Input validation`. Tibble-returning functions swap in `Output structure` / `Column contracts`.
- Test names: `"<function_name> <what is being tested>"`, plain-English sentences. Never "works correctly".

### 5.2 Fixtures and wavelength constraint

Installed test data via `system.file()`, fixture chain: `REFLECTANCE_testdata.tif` (9×9×101 bands, **517.58–772.19 nm**) → `MEDIAN_` → `SAVGOL_` → `CONREM_`. Start from the fixture matching the function's pipeline position (`hsi_remove_continuum` tests start from SAVGOL). All wavelength arguments must fall inside 517.58–772.19 nm; presets (RGB, NIR, CIR, SWIR) will error by design. Safe happy-path choice: `c(700, 620, 540)`. Synthetic edge-case rasters are built from fixtures, not from scratch.

### 5.3 Property-based thinking, right-sized

Assert properties that must always hold (type, dimensions, bounds, finiteness, band names, guaranteed errors). One **behaviour** per test, not one expectation per test:

- Dimension preservation is **one** test with `nlyr`/`nrow`/`ncol` expectations inside — never three tests.
- Value sanity: finiteness (no `Inf`/`NaN`) and mathematically guaranteed bounds (continuum removal → [0, 1]) are separate behaviours — separate tests.
- Fixture-match (`expect_equal` vs reference file) is at most one test per function, never the only test.
- Never re-test what terra/GDAL itself guarantees (format handling, CRS propagation) — that is their interface, not ours.

### 5.3a Testing functions that subset before computing

Index functions cut their input to a wavelength window (`x_range`) before the pixel function ever runs. A test that injects a value — `NA` especially — into a band **outside** that window is not a failing test, it is a **vacuous** one: it passes forever while exercising nothing. Derive the target band from `terra::names()` and the window the test call actually uses; never hardcode a band index. (Caught mid-implementation on `hsi_calc_raba()`, 2026-08-19.) The same care applies to any function that masks, subsets or crops before the code under test.

For functions carrying `cores` (§3.10), the per-function set is: `cores = 2` output `expect_equal` to `cores = 1` output on the fixture; the `NA` path under `cores = 2` (proving the guard runs *on workers*); and invalid `cores` rejected with a message fragment plus `class = "hsitools_error"`. Never a timing test — §5.8 stands.

### 5.4 Tolerance

`expect_equal(tolerance =)` is **relative** — wrong for quantization/fixed-precision guarantees. For absolute bounds compute `max(abs(actual - expected))` and use `expect_lte(max_abs_error, 1 / scale_factor)`. Reserve `expect_equal(..., tolerance = 1e-6)` for cross-platform floating-point fixture comparisons only.

### 5.5 File-writing tests — withr, never manual unlink

Every function with `filename`/`overwrite`: (1) writes file when filename provided, (2) errors when file exists and `overwrite = FALSE`. Temp paths via `withr::local_tempfile(fileext = ".tif")` — cleanup is guaranteed even when the function under test errors. **Never** bare `tempfile()` + manual `unlink()` in tests (same withr policy as source code, §3.6):

```r
test_that("hsi_xxx writes to file when filename provided", {
  temp_file <- withr::local_tempfile(fileext = ".tif")
  result <- hsi_xxx(x = test_reflectance, filename = temp_file, overwrite = TRUE)
  expect_true(file.exists(temp_file))
  expect_s4_class(result, "SpatRaster")
})
```

Since the write block is duplicated per function by design (§3.4), the pair is required per function; implement it as a shared `helper-*.R` expectation helper to kill the boilerplate while keeping per-function coverage.

### 5.6 Error and message testing — two layers

- **Correctness layer (runs everywhere, CRAN included):** every guaranteed error gets an `expect_error()`. For our own `cli::cli_abort()` errors, prefer asserting on `class =` or a short stable message fragment; for terra/GDAL internals, bare `expect_error()` only — their messages are not version-stable. Failures raised by sanctioned rlang type-check helpers (§3.3 carve-out) assert on `class = "rlang_error"` or a stable fragment — never `hsitools_error`.
- **Volatile paths in snapshots:** any `expect_snapshot(error = TRUE)` whose message interpolates a runtime-generated path (`withr::local_tempfile()`, tempdirs — anything a `cli_abort()` renders via `{.file {filename}}`) must redact it to a stable placeholder via `transform`, or the snapshot can never pass twice — the first run silently "adds" it, every later run fails as a phantom regression:

  ```r
  expect_snapshot(
    hsi_write_metadata(x, filename = temp_file),
    error = TRUE,
    transform = \(lines) gsub(temp_file, "<temp_file>", lines, fixed = TRUE)
  )
  ```

  `fixed = TRUE` is required — Windows paths contain backslashes that would otherwise be mangled as regex.
- **Message-quality layer (dev/CI only):** one `expect_snapshot(error = TRUE)` block per function, exercising every `cli_abort()` branch we author, so the full user-facing message is reviewed and pinned. Snapshots live in `_snaps/*.md`, are committed to git, and reviewed in PRs like code. Snapshot tests are skipped on CRAN by default — never the sole test of a behaviour.
- `cli::cli_warn()` paths get `expect_warning(class = )` or a snapshot; testthat 3e no longer swallows messages — wrap known-noisy calls in `suppressMessages()` or assert them deliberately.
- **Class assertions ride, never drive:** `class = "hsitools_error"` / `"hsitools_warning"` is added alongside an existing message/behaviour assertion — never write a test whose sole content is a class check, and never add `class =` to a bare `expect_error()` that asserts nothing else (that's a class-only test by the back door). New abort/warn sites earn class assertions organically when their functions next get real test work.

### 5.7 S3 class testing (endmember class and successors)

For each S3 class: constructor returns the class (`expect_s3_class`); validator rejections via the two-layer error pattern (§5.6); `print()` method via `expect_snapshot()` — the canonical snapshot use case; `[` subsetting semantics as property tests (subsetting preserves class, `$spectra` columns and `$locations` stay aligned and named). Test methods through the generic (external interface), not internal helpers.

### 5.8 Scope and counts

Do **not** test: internal `check_*` helpers directly (validated once in isolation / indirectly through public functions), stub plot functions, deprecated functions, or the `cores` argument. Parameterized tests / shared helpers for function families; keep individual tests focused on unique logic. Target **~8–12 tests** per raster-transforming function, **6–10** for tibble-returning extraction functions (counts assume consolidated behaviours per §5.3; more only when the function has genuinely more contracts). `Suggests` packages guarded with skip helpers for CRAN compliance. Prune suites to reflect current function surfaces — dead tests are debt; testthat auto-deletes dangling snapshots, but guard conditional/skipped snapshot tests with `announce_snapshot_file()`.

---

## 6. Calibration and acquisition constraints (physics you must not violate)

### 6.1 Reflectance formula and matched darks

Dual-exposure protocol: white reference (ET_white) and target (ET_target) scanned at different integration times; Lumo's built-in corrections are never applied. One formula for both sensors:

```
numerator   = specimen - dark × (ET_target / ET_dark)
denominator = white    - dark × (ET_white  / ET_dark)
R = numerator / denominator × (ET_white / ET_target)
```

- Dark current scales linearly with integration time — the `tint_dark` scaling is essential. Unsubtracted SWIR dark (~3000–9000 DN) was the dominant historical calibration error; VNIR (~100 DN) barely affected but corrected anyway.
- **Matched dark references are always available**: Lumo captures DARKREF at specimen integration time in the capture folder. Prefer matched dark subtraction; the scaled-dark approximation was a workaround for a non-existent problem. Matched darks eliminate striping and negative-reflectance artifacts, critical for SWIR.
- ET_dark = ET_white by protocol, so the denominator scaling is 1.0.

### 6.2 References and panels

- **BaSO₄ white reference is contaminated for SWIR**: OH absorption at ~1450, ~1950, ~2500 nm corrupts the denominator at scientifically important wavelengths. Spectralon/PTFE is correct for SWIR work.
- **Grey panel / relative reflectance** is analytically sufficient for shape-based analyses (RABD, continuum removal, CR-based SAM) — these are invariant to smooth multiplicative panel factors. The cost is SNR degradation in SWIR water windows (~1400, ~1900 nm).

### 6.3 SWIR encoding

Write SWIR as **float32, never uint16** — quantization zeros corrupt column medians and break destriping.

### 6.4 Destriping

Column-wise **multiplicative** correction (Gadallah et al. 2000; Rogaß et al. 2011), gain-only (offset already removed by matched dark subtraction). Replace zero/NA column medians with the global band median so the correction factor becomes 1.0. Pipeline order: reflectance → **destripe** → median smooth → savgol (destripe while stripes are crisp single-column features). Crop to specimen extent first — background/tape columns contaminate medians (user responsibility; keep the signature minimal).

### 6.5 Sensor fusion (VNIR–SWIR)

- Trim edge bands aggressively by SNR (VNIR to ~950 nm, SWIR from ~1100 nm); the ~950–1100 nm dead zone is honestly unmeasured.
- `hsi_bind_sensors()` (in design): hard cut at a `cut` wavelength (default overlap midpoint), gain correction via per-band median ratio across the full overlap window, **no blending**.
- Co-registration is GCP-based (steel pins, ≥10 per section, digitised in GIS) via affine transform + GDAL warp.
- Vendor-agnostic ingest: treat cross-vendor spectral tilt as a non-problem at the analysis layer by defaulting to continuum-removal-first methods; localize vendor differences in a thin ingest adapter keyed on processing stage.

### 6.6 Saturation

**A clipped reading carries no information about the specimen.** The digital number is pinned at the ceiling, so what survives calibration is the shape of the white reference: a smooth, plausible-looking curve that is pure instrument response. Measured on a GKUT VNIR transect (2026-09-09), a sixty-band clipped run sat flat at 65535 in raw DN and climbed from 0.2565 to 0.3259 in reflectance, and a linear chord across the gap reproduced those values within 0.002. Passing clipped cells through and interpolating across them are therefore the same fiction, and neither recovers anything.

**The clipping threshold is instrument knowledge, is never inferred from the data, and normally sits below the datatype ceiling.** Detector response compresses before it clips: immediately outside that run, unflagged bands read 64824 and 64450 rather than returning to a normal level, so an exact-ceiling test understates the damaged region. Inferring the threshold from the data maximum would make a clean capture flag its own brightest pixel; hardcoding a bit depth would tie the check to one sensor. `hsi_check_saturation()` takes it as a required argument for exactly this reason.

**Masking is whole-pixel, via the collapsed screen.** A pixel that clipped anywhere is suspect everywhere — charge spill into neighbouring bands, plus that compressed shoulder — and no line can be drawn between contaminated and clean. Per-band masking would assert a distinction the data does not support. A consequence worth stating: whole-pixel masking makes `hsi_smooth_savgol()`'s all-or-nothing `NA` rule *correct* rather than an amplifier, since it only propagates a decision already taken upstream.

**Mask while the product is still on the raw pixel grid**, i.e. after calibration and before any flip or co-registration. The screen is written full-frame and the product is usually windowed, so narrowing the screen to the product's extent is the alignment step; both share the raw index space only until the product's geometry changes.

**`hsi_calc_reflectance()` never adjudicates saturation.** It marks calibration failures (a zero or negative denominator) and nothing else. Detection is a separate primitive on raw digital numbers, and masking is the caller's deliberate step — the same composition rule as §7's masking-precedes-unmixing.

---

## 7. Unmixing and big-raster restraints (hard-won — do not relearn)

- **Bind first, trim later.** Applying `terra::subset()` (trim_edges) per scene *before* `hsi_bind_rows` forces per-scene materialization and can cause hours-long non-convergence. Bind untouched file-backed pointers (GDAL VRT stays lazy), then trim, deferring pixel reads to a single extraction pass. `terra::mask()` may similarly materialize — same caution.
- **Search once, select by subsetting.** N-FINDR re-optimizes simplex volume on every run and sheds low-abundance but spectrally distinct vertices when `n` is reduced (a rare phase present at n=15 vanished at n=5). Correct workflow: over-specify endmembers, then subset columns (`em$spectra[, keep]`). Never re-search at lower `n`.
- **PCA over MNF for pooled unmixing.** MNF's lag-1 row-difference noise estimate is corrupted by seam-crossing pixel pairs in a bound multi-scene raster. Pooled `stats::prcomp` is the reduction front-end.
- **Masking before MNF is safe** (probed 2026-07-18): `spacetime::mnf` differences neighbours *before* dropping NA pairs, so mask holes shrink the noise-estimate sample without biasing it. Caveat: dilating crack masks by 1–2 px to exclude smeared edge pixels is sensible practice, but is domain judgment, not a probe result.
- **Cumulative explained variance undercounts endmembers** — it buries low-abundance, spectrally distinct phases. Use over-specify-then-prune-on-SAM.
- **SAM angle interpretation** (verify degrees vs radians in `hsi_calc_sam` first): <3–5° collapsed/redundant; ~5.7° classic match tolerance; 6–17° distinguishable/keep; 17–40° clearly different; 40–90° unrelated.
- Endmember convention: `list($spectra, $locations)` where `$locations` is a named integer vector of terra cell numbers. This is being formalized as an **S3 class** (constructor, validator, `[` semantics, print method) — S3, not S4/S7. A spectral-metadata sidecar (wavelengths, FWHM, sensor identity) travels *alongside* the SpatRaster, never wrapping it; the transformation pipeline itself is never wrapped in a custom class.
- Abundance estimation is nnls-only (`nnls`).
- **unmixR is never a dependency of HSItools** (decided 2026-07-06). It is GitHub-only (no CRAN release) and endmember *search* is not core functionality. Search (N-FINDR/VCA via unmixR) lives in zarowka templates, which may use `Remotes:` freely. The `hsi_endmembers` class is a **pure container** — spectra + locations, no opinion on origin; any producer (unmixR output, manual ROI picks, spectral library) feeds the same constructor. If search ever enters core, it is a clean-room implementation, never an import; see §10 for its unresolved naming.
- **Masking precedes unmixing — always.** Background/tray pixels silently corrupt indices, stats, and especially endmember search (N-FINDR will pick the tray as a vertex). Masking is material-agnostic by construction: the user supplies the rule (band/index threshold) or a SpatVector; the function only applies it. Mind that `terra::mask()` can materialize (see bind-first caution above).

---

## 8. hsical interop contract

As of contract v3.0 (2026-07-11), **the HSItools metadata sidecar is the scan record** and hsical owns no schema, no YAML machinery, and no reader of its own. Rules:

- **Single schema, owned by HSItools.** Flat YAML (no nested groups), 32 keys, `schema_version` 1.1.0, only `name` required. Normative definition lives in `hsi_create_metadata()` / `validate_hsi_metadata()`; the human-readable map (field → GUI source) lives in `hsi-interface-contract.md`. If the schema changes: contract document first, then HSItools, then hsical's form — in that order.
- hsical maps GUI fields onto `hsi_create_metadata()` arguments and calls `hsi_write_metadata()` / `hsi_read_metadata()` directly. If hsical code ever contains `yaml::` or a field list of its own, the design has been violated.
- No `camera` field — hsical's camera selector writes `sensor_type`. No sample/location fields, no `notes`, no `saturation_ratio`, no test-scan positions, no derived `scan_length` (all deliberately rejected; reasons in contract v3.0).
- One sidecar per capture, beside the data; the master CSV is dead — aggregate views read sidecars back via `hsi_read_metadata()`.
- Unchanged protocol invariants: ET_white mandatory per session (ET_dark = ET_white); motor positions logged, never scan length; manual `session_id` groups scans sharing a white reference; naming `PREFIX_CC-SS_TIMESTAMP`, zero-padded; saturation screening happens in R, not the app.
- hsical never processes spectral data, never calculates reflectance, never co-registers.

---

## 9. Project and release conventions

- **pkgdown dev/stable split**: dev branch publishes to `/dev/` via `development: mode: auto` + `.9000` version suffix; drop the suffix on release merge to main.
- **References**: Kruse 1993 (SAM), Nascimento & Bioucas-Dias 2005 (VCA), Winter 1999 (N-FINDR), Gadallah 2000 & Rogaß 2011 (destriping), Butz 2015 (RABA workflow). Verify any DOI before adding to `references.bib`.
- **Iterate one thing at a time**: propose one change, let tests run, then continue. The user runs tests after each change and catches errors actively.
- **Summaries for new chats**: when asked to summarize for a new chat, always write a `.md` file with the current date (YYYY-MM-DD) in both the file name and the file header.
- Verify version-dependent behaviour (e.g. `terra::spatSample(cells = TRUE)` column name) against the installed package version before writing test assertions.
- **CI must stay green per iteration**: r-lib `check-standard` matrix (Ubuntu devel/release/oldrel-1, Windows release, macOS release) on push/PR, `upload-snapshots: true`. New workflows (coverage, pkgdown) are added one at a time, never batched.
- **Metadata sidecars must carry a `schema_version` field** from the first sidecar written in the wild. The same versioning question applies to the hsical interop contract.
- **File/roadmap decoupling (rule):** this file (CLAUDE.md, canonical; mirrored to the claude.ai skill at milestone boundaries) carries only durable conventions and constraints. Milestone numbers, sequencing, feature backlogs, and session state live exclusively in dated documents (`YYYY-MM-DD_*.md` — roadmap, audits, handoffs). Never write a version-number milestone into this file; never treat a handoff as a source of durable convention. Handoffs execute decisions already made — items a handoff marks settled stay settled; items marked ⏳ belong to Maury. As of 2026-07-18 all dated documents live in the **private `hsi_development` repo** (one folder per package: `hsitools/`, `zarowka/`, `hsical/`), never inside the package repos; the `dev-notes/` `.gitignore` entries in the package repos remain as a safety net. Changelog citations of `dev-notes/...` paths predate the move and now resolve under `hsi_development/hsitools/`.

---

## 10. Open design questions — DO NOT treat as settled

These are deliberately unresolved. Ask before implementing; never lock them unilaterally:

- **`hsi_bind_sensors()`** — four open decisions: final function name, default `cut` behaviour, whether to offer an offset option, wavelength source. The hard-cut + median-ratio-gain + no-blending approach *is* agreed; the four details are not.
- **splib07 spectral library resampling** — method choice between `approx()` and Gaussian SRF convolution weighted by FWHM; input model (folder glob vs chapter directory query).
- **`hsi_calc_sam` units** — verify whether it returns degrees or radians before writing any threshold logic or tests.
- **`terra::spatSample(cells = TRUE)` column name** — verify against the installed terra version before writing test assertions. (terra 1.9-25/34/46 all changed `spatSample` behaviour — re-verify, do not assume the 2026-07 answer holds.)
- **`hsi_tiled()` fate** — measurement left it without a use case (2–5× slower than direct on both narrow and wide rasters; ~99 % of its cost is the serial VRT→TIFF mosaic read-back). Retire it, or keep it documented as legacy? Related: terra 1.9-25 added its own `tile_apply()` for parallelization, which may cover any remaining need; and whether a shipped product may be a VRT at all is unresolved (if the mosaic cost is read-side, a VRT defers rather than removes it — a ~2 h probe would settle it, and is moot if the function is retired).
- **terra native GCP support** — terra 1.9-46 added `has.geoloc`/`geoloc` and better GCP handling in `rast()`. The co-registration toolchain currently embeds GCPs into a VRT by hand and warps via `sf::gdal_utils()`; whether to migrate is unexamined.
- **`terra::terraOptions(memmax = )`** — on the windowed reflectance step, raising it from the 16 GB default to 64 GB measured 5.8× (647 → 112 s, GKUT VNIR), and flat from there: 336 GB on a 383 GB machine changed nothing, because peak working set was 19 GB. Thread count did not order the results at 16, 32 or 64. On the postprocess chain the same change measured ~13 % (bigger blocks, fewer per-block closure ships). Earlier it also masked a correctness bug by keeping crops in memory; with `terra::window()` that path is gone and this is purely a speed knob. Still unadopted: machine-specific, and untested for interaction with concurrent `terra::app()` workers.
- **Lawson & Hanson 1974 DOI** — verify before adding to `references.bib`.
- **S3 endmember class** — S3 (not S4/S7) is decided; the concrete constructor/validator/`[`/print design is in progress, endmember class first, metadata sidecar second.
- **`wavelength_position` `arg`/`call` threading** — it is exported *and* the internal workhorse behind every `hsi_calc_*` index; its abort names `{.arg wavelength}`, which callers of `hsi_calc_*` never typed. Recommendation: thread. Not confirmed.
- **`cli_alert_info()` at `hsi_check_gcp.R:100`** — migrate to `cli::cli_inform()` or bless `cli_alert_*` in §3.3a. One line either way; Maury's pick.
- **Wavelength units on ingest** — ENVI headers are often µm; house convention is nm. Normalize-on-ingest vs. explicit units field in the metadata sidecar.
- **In-core search function naming/signature** (if/when search ever enters core) — `hsi_find_endmembers()` vs `hsi_search_endmembers()` vs method-as-argument vs one-function-per-algorithm. Must not be grandfathered in from a zarowka template name. Scheduling lives in the roadmap, not here.

---

## 11. Quick pre-flight checklist

Before proposing any HSItools/zarowka code, confirm:

1. Approach discussed first? (No code before design agreement.)
2. Function atomic, simplest possible IO, lean signature — and sensor-, manufacturer-, and material-agnostic (no Specim/Lumo/sediment/wavelength-range assumptions)?
3. Argument order matches §3.1; validation block with `check_*` + `cli::cli_abort()` only, umbrella `hsitools_error` class per §3.3, correct call attribution (internal helpers thread `call`, exported functions don't)?
4. Return Shape A or B; `# Validate inputs` / `# Build write options` / `# Write to file` / `# Return result` verbatim?
5. `|>`, `\(i)`, `purrr` (no loops/apply), `::` everywhere, no `rlang::` messaging?
6. Works lazily on larger-than-memory data; no premature `terra::subset`/`mask` materialization?
7. Roxygen matches §4 verbatim strings and tag order; `@export` last?
8. Tests follow §5: sections, naming, fixture chain, 517.58–772.19 nm constraint, withr tempfiles, one behaviour per test, snapshot layer for cli errors?
9. Touching anything in §10? Stop and ask before deciding.

---

## Changelog

- **1.11.0 (2026-09-10)** — Saturation doctrine settled, and both questions opened the same morning closed (design Fable + Maury; records in `hsi_development/zarowka/2026-09-09_window-reflectance-gkut-validation-opus.md` and `2026-09-09_memmax-window-probes-opus.md`). `hsi_check_saturation()` **promoted from zarowka to HSItools** (0.5.3.9003), bringing the new `HSI Diagnostics` family in §4.6; zarowka now calls it through the package namespace and requires that version. New §6.6 records the physics and the composition rule: a clipped reading carries no specimen information (a sixty-band run measured flat at the ceiling in DN and climbing 0.2565 to 0.3259 in reflectance, matching a linear chord within 0.002, i.e. pure white-reference shape); the threshold is instrument knowledge that normally sits *below* the datatype ceiling because response compresses first (unflagged neighbours at 64824 and 64450); masking is whole-pixel via the collapsed screen; and it happens on the raw pixel grid, after calibration and before co-registration. §10 accordingly **loses** both entries added in 1.10.0: reflectance keeps no saturation semantics (the threshold cannot be inferred and the primitive already exists separately), and the `hsi_smooth_savgol()` any-NA question dissolves rather than resolving, since whole-pixel masking makes the existing rule correct.
- **1.10.0 (2026-09-10)** — The saturation/NoData collision closed on the template side (design and orchestration Fable + Maury; implementation, runs and validation by Opus subagents. Problem statement in `hsi_development/hsitools/2026-09-09_handoff-saturation-nodata-open.md`; records in `hsi_development/zarowka/2026-09-09_window-reflectance-gkut-validation-opus.md` and `2026-09-09_memmax-window-probes-opus.md`). **`terra::window()` replaces `terra::crop()` for cutting a raw capture** — new §2 anti-pattern row and a rewritten crop gotcha. A materialised crop writes the copy in the source integer datatype and reserves the datatype maximum as NoData, so genuine saturated readings return as `NA`; on a GKUT VNIR transect that silently destroyed 24,816 readings, and whether it happened at all depended on terra's memory budget. The windowed product is bit-identical to the in-memory-crop baseline with zero `NA`, and the 24,816 cells it preserves match the raw window exactly. `hsi_calc_reflectance()` gained an `@details` paragraph documenting the trap, the `terra::window()` recommendation, and the float-datatype fallback for physical subsets. Nine measured runs of the VNIR reflectance step separated the effects: `memmax` alone accounts for the whole window-versus-crop timing gap, thread count does not order the results at 16/32/64, the setting is flat from 64 GB up, and the documented float crop is correct but ~25× slower than windowing (serial read-back of terra's default block layout, not the write). §10's `memmax` entry rewritten accordingly — it is no longer a correctness knob — and gains the two questions the 2026-09-09 handoff parked there: reflectance saturation semantics, and the `hsi_smooth_savgol()` any-NA policy.
- **1.9.0 (2026-08-19)** — Parallelism conventions, from the 2026-08-18/19 arc (design/orchestration Fable + Maury, implementation and validation by Opus subagents; full record in `hsi_development/hsitools/2026-08-19_handoff-parallelism-closeout-fable.md`). §3.10 rewritten: `terra::app(cores = )` is the backend for per-pixel spectral operations, with the **mandatory unconditional minimal-environment rebind** — an inline lambda carries its call frame, so the SpatRaster (~40 MB serialized, re-shipped per write block) rides to every worker and the naive pass-through measured **5.5× slower than serial**; `data = list()` is valid and load-bearing; explicit `pkg::` namespacing in the body becomes load-bearing under `parent = baseenv()`. `cores` landed on `hsi_smooth_savgol`, `hsi_remove_continuum`, `hsi_calc_raba`, `hsi_calc_remp`; chain 64 → 4.8 min (GKUT SWIR), outputs bit-identical to serial and to the 2026-05 `hsi_tiled()` products. `mirai` demoted from "the parallel backend" to custom orchestration only; `hsi_tiled()` measured 2–5× slower than direct on both raster shapes (~99 % of cost in the serial VRT mosaic read-back, **not** `makeTiles()`) — fate moved to §10. §2 gains the `cores`-vs-`threads` distinction (built-in character funs ignore `cores`; a dead `cores` was removed from `hsi_smooth_median()`) and the `terra::crop()` NoData trap. New §5.3a: tests for functions that subset before computing must target bands **inside** the window (a hardcoded index gives a vacuous always-green test), plus the standard three-test set for `cores`. terra performance floor recorded: ≥ 1.9-46 TBB-threads the built-in focal statistics (median 3.9× faster, bit-identical), templates set `terraOptions(threads = )` to the core count. §10 gains four entries (`hsi_tiled` fate incl. terra's own `tile_apply`, terra native GCP support, `memmax` tuning, `spatSample` re-verification).
- **1.8.2 (2026-07-18)** — Dev-notes centralized into the **private `hsi_development` repo** (decision Maury + Fable, 2026-07-18), one folder per package (`hsitools/`, `zarowka/`, `hsical/`), after the day's accidental push of `dev-notes/` to the public HSItools repo (commit `847ae9a`, reverted in `87fb297`). §9 file/roadmap decoupling rule updated accordingly: dated documents never live inside the package repos; their `dev-notes/` `.gitignore` entries remain as a safety net; pre-move changelog citations of `dev-notes/...` paths resolve under `hsi_development/hsitools/`. Companion corrections applied to zarowka's and hsical's CLAUDE.md (scratchpad wording, interface-contract location — no absolute filesystem paths anywhere by policy). No convention content changed otherwise; §10 untouched.
- **1.8.1 (2026-07-18)** — §7 gains the masking-before-MNF safety note, closing D2 of the hsi_mask close-out (source + ground-truth probes by Opus, design Fable+Maury; evidence in `dev-notes/2026-07-18_handoff-mask-closeout-opus.md` and `..._handoff-d2-mnf-mask-probe-opus.md`). Seam-crossing pairs (the existing PCA-over-MNF bullet) remain toxic; mask holes are benign — the two cases are mechanically different (valid-valid poisoned pairs vs. dropped NA pairs). No other sections touched.
- **1.8.0 (2026-07-14)** — The `...` sink fix (design Fable+Maury, probes Opus/Fable, sweep Sonnet+Fable, all 2026-07-14; record in `dev-notes/2026-07-14_*dots*` in both repos). New `check_dots_write()` helper in `utils-checks.R` and its call blessed as canonical: guarded-write Shape A functions end the validation block with `wopt_user <- rlang::list2(...)` + `check_dots_write(wopt_user, filename)`, aborting when `...` is non-empty and `filename == ""` — previously such arguments (typos, removed args like `hsi_calc_abundance(method =)`) were silently discarded, which defeated zarowka's test suite for three weeks. §3.2/§3.4/§3.5 updated; §3.4 snippet now shows `wopt_user` captured in the validation block. **Exemption recorded in §3.2:** functions passing `filename`/`wopt` directly into `terra::app()`/`focal()`/`predict()` are excluded — terra validates `wopt` unconditionally and honours valid options in-memory (probed, terra 1.9.34), so their `...` is live and the check would break working calls. Swept: 7 HSItools + 2 zarowka functions; 8 HSItools functions exempt. Anti-pattern row added; helper inventory updated. `test-hsi_rcv.R` renamed `test-hsi_calc_rcv.R` (§3.8 mirror rule). §10 untouched.
- **1.7.0 (2026-07-13)** — Migrated from claude.ai skill to repo-root `CLAUDE.md` as the canonical source (decision Maury + Fable, 2026-07-13); version header and changelog now live here, and the skill becomes a mirror refreshed at milestone boundaries. New unnumbered **Repo map and mechanics** section merged from the Claude Code auto-generated CLAUDE.md, with two corrections: package description rewritten material/sensor/manufacturer-agnostic (the generated text had baked in core-scanning framing, violating §1) and the formatter guidance corrected to air (the generated text inferred hand-formatting from a missing config). Genuinely new captures retained from the generated file: `@family`-as-navigation, `wavelength_position()` as the shared wavelength primitive, `hsi_coregister()` file-source requirement, mirai worker self-containment, `spectral_indices` column→argument mapping, fixture layout, Windows CI vignette skip, schema-version bump-together rule, and the as-built articulation of the two temp-file lifetimes (normative rule unchanged in §3.6). §0 rule 7 rewritten conditionally (executing sessions run tests and show output per change; non-executing sessions hand off patches); new rule 8 (git modifications belong to Maury; read-only inspection permitted) and rule 9 (no milestone state or session notes in this file; never edit unprompted). §9 decoupling rule renamed file/roadmap and now names this file. §10 untouched — all open questions remain open.
- **1.6.0 (2026-07-11)** — Post-0.5.3-release consolidation. §8 rewritten for contract v3.0: the flat HSItools sidecar (32 keys, schema 1.1.0) is the scan record; hsical is schema-less and wraps the trio; deliberate field rejections recorded. §0 gains rule 7 (execution-environment): model sessions edit source and hand off patches, Maury runs all R verification locally. §10 unchanged.
- **1.5.0 (2026-07-10)** — Guardrails Phase 2 closure (decisions 2026-07-09 by Fable+Maury, sweep 2026-07-10 by Sonnet): §3.3 `hsitools_warning` umbrella promoted from proposed to settled convention (every `cli_warn()` site, hardwired, no taxonomy); corresponding §10 question dropped. §5.6 gains the class-assertion discipline: class assertions ride on existing message/behaviour assertions, never as a test's sole content, never bolted onto bare `expect_error()`. `check_spatraster_list()` joined `utils-checks.R` — no skill edit needed; §3.3's generic `check_*` rules already cover it. All other §10 questions (incl. `cli_alert_info`, `wavelength_position` threading) remain open.
- **1.4.0 (2026-07-08)** — Durable conventions from the metadata-trio sessions (design 2026-07-07/08, execution by Sonnet 2026-07-08): §3.3 rlang type-check carve-out (`rlang::check_string()` et al. sanctioned for scalar validation, errors stay rlang-classed, never wrapped or duplicated); new §3.11 S3 serialization pattern (readers assemble via `structure()` + shared validator, never the constructor, preserving on-disk provenance such as `schema_version`); §2.0 anti-pattern row for `$` partial matching on deserialized data (spell full names); §5.6 gains the rlang-classed assertion consequence and the volatile-path snapshot rule (`transform` with `fixed = TRUE` redacting runtime paths to stable placeholders). §10 untouched — `hsitools_warning`, hsical YAML reader, and all other open questions remain open.
- **1.3.0 (2026-07-06)** — Guardrails conventions from the 2026-07-06 audit/planning session: new §3.3 umbrella condition class (`hsitools_error`, hardwired in helpers, no category taxonomy) and call-attribution rule (internal helpers thread `arg`/`call`; exported functions never do); `check_one_of()` replaces `match.arg()` in §3.2 and the anti-pattern table; new anti-pattern row banning aborts inside purrr lambdas (collect-all-offenders pattern). §7: unmixR is never a dependency, `hsi_endmembers` is a pure container, masking-precedes-unmixing constraint added. §9: CI-green-per-iteration, `schema_version` requirement, and an explicit **skill/roadmap decoupling rule** — milestones and sequencing live only in dated roadmap/handoff documents, never in this skill. §10 gains five open questions (warning umbrella, `wavelength_position` threading, `cli_alert_info`, wavelength units on ingest, in-core search naming). Written by Fable ahead of planned unavailability; the paired execution handoff is `2026-07-06_handoff-guardrails-opus.md`.
- **1.2.0 (2026-07-02)** — §5 rewritten after audit against r-pkgs (2e) and testthat 3e: one-behaviour-per-test consolidation (target 8–12, not 12–16); `withr::local_tempfile()` replaces manual `unlink()` (removes contradiction with §3.6); two-layer error testing with `expect_snapshot(error = TRUE)` for cli messages; S3 class testing section added ahead of the endmember class; helper-*.R and hermeticity rules added.
- **1.1.0 (2026-07-02)** — Added §0 working principles, §2.0 anti-pattern table, §10 open design questions; made sensor/manufacturer/material agnosticism explicit and imperative; marked param and family catalogues open-ended.
- **1.0.0 (2026-07-02)** — Initial consolidation of CODING/TESTING/ROXYGEN guidelines, interface contract, sensor-fusion notes, and accumulated restraints.