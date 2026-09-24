---
paths:
  - "tests/**/*"
---

# Testing standards (§5)

Loaded when Claude reads anything under `tests/`.

## 5. Testing standards (testthat 3e)

Calibrated against r-pkgs (2e) test design and testthat 3e practice (2026-07-02 audit). Guiding principles: test the **external interface** only; test **each behaviour in one and only one test** (multiple expectations per test are fine — one behaviour is not one expectation); tests are **hermetic** (self-contained setup and teardown; a fresh session + `devtools::load_all()` must be able to run any single test).

### 5.1 Structure

- One test file per exported function: `test-<function_name>.R`, mirroring `R/`.
- File opens with a comment block (what the function does, key contracts). **No code outside `test_that()` blocks** in a `test-*.R` file.
- Fixtures live in helper files, auto-loaded by testthat and `devtools::load_all()`: shared fixtures in `tests/testthat/helper-fixtures.R`, fixtures used by a single test file in `tests/testthat/helper-<name>.R` or built inside the `test_that()` block that needs them. Fixtures are read-only — never mutate one inside a test; derive per-test copies via `terra::setValues()` / `terra::subset()`. Test files that still load fixtures under a top-level `## Setup ----` predate this rule; move a file's setup into helpers when it next gets real test work.
- Never `source()` inside `tests/testthat/`. Shared expectation helpers live in `tests/testthat/helper-*.R`.
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
- Use the most specific expectation available, because its failure message shows the offending values: `expect_all_true(x >= 0 & x <= 1)` not `expect_true(all(...))`, `expect_all_equal()` for "every element equals", `expect_type()` / `expect_s4_class()` for types. `expect_true()` / `expect_false()` only where no specific expectation exists (`expect_true(file.exists(temp_file))`, §5.5). `expect_all_true()` / `expect_all_equal()` need testthat ≥ 3.3.0.

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

Do **not** test: internal `check_*` helpers directly (validated once in isolation / indirectly through public functions), stub plot functions, or the `cores` argument. Deprecated functions and arguments (§3.12) get no new behaviour tests — exactly one `expect_snapshot(. <- hsi_fn(...))` pinning the deprecation warning; existing tests that exercise them stay until removal, silenced with `withr::local_options(lifecycle_verbosity = "quiet")` at the top of each affected `test_that()` block. Parameterized tests / shared helpers for function families; keep individual tests focused on unique logic. Target **~8–12 tests** per raster-transforming function, **6–10** for tibble-returning extraction functions (counts assume consolidated behaviours per §5.3; more only when the function has genuinely more contracts). `Suggests` packages guarded with skip helpers for CRAN compliance. Prune suites to reflect current function surfaces — dead tests are debt; testthat auto-deletes dangling snapshots, but guard conditional/skipped snapshot tests with `announce_snapshot_file()`.
