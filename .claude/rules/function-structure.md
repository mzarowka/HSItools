---
paths:
  - "R/**/*.R"
---

# Function structure, continued (§3.5–3.12)

Loaded when Claude reads package source under `R/`. §3.1–3.4 (argument order, validation block,
condition classes, messaging, return shapes) are in `CLAUDE.md` and always loaded.

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

**`terra::app(cores = )` is the backend for per-pixel spectral operations** (settled 2026-08-19, after measurement on two production rasters of different shape). Any exported function that hands a **user-supplied R function** to `terra::app()` takes `cores` at §3.1 slot 9, validated with `check_numeric(cores, len = 1, positive = TRUE)`. Current holders: `hsi_smooth_savgol()`, `hsi_remove_continuum()`, `hsi_calc_raba()`, `hsi_calc_remp()`. Built-in character funs never get it (§2 terra gotchas).

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

`data = list()` is valid and **load-bearing** when the body references nothing from the frame (`hsi_calc_raba()`): its job is removing the fat environment, not adding to it — never "clean it up". `parent = baseenv()` suffices because `::` lives in base and namespaces serialize by reference; this makes explicit `pkg::` namespacing in the body load-bearing too, not just house style. Applied unconditionally, including at `cores = 1`, so there is one code path (§1, design philosophy). Verified: `tryCatch` handlers survive the trip and fire on workers; outputs are bit-identical to serial.

**`mirai` remains available for genuinely custom orchestration**, but is no longer the default answer. `hsi_tiled()` pattern (unchanged, fate open — §10): `mirai::mirai_map()` with `.args = list(fun = fun)` for explicit argument passing (never rely on environment scoping); `carrier::crate()` for worker environment isolation; `terra::makeTiles()` with corrected tile-size conversion; VRT-based mosaic; `BIGTIFF=YES` hardcoded for mosaic intermediates. Measured 2–5× **slower** than a direct call for per-pixel ops on both narrow and wide rasters, with ~99 % of the cost in the serial VRT→TIFF mosaic read-back, not in `makeTiles()`.

**terra version floor (performance, not correctness):** from terra 1.9-46 the built-in focal statistics `max`/`min`/`median`/`modal`/`sd` are TBB-threaded (previously only `sum`/`mean`), which makes `hsi_smooth_median()` ~3.9× faster with no code change; values are bit-identical either way. Templates set `terra::terraOptions(threads = )` to the core count, since terra's own default caps at 16.

### 3.11 S3 serialization — readers bypass the constructor

Functions that deserialize an S3 object from disk assemble it via `structure(raw, class = "...")` and then run the shared validator — **never** through the constructor. Constructors stamp provenance fields (e.g. `schema_version`); re-running one on read would overwrite what the file actually says, defeating any schema gate. Pattern established by `hsi_read_metadata()` (2026-07-08).

### 3.12 Deprecation (tidyverse lifecycle workflow)

Exported functions and arguments are deprecated before they are removed, never removed outright. The first deprecation in a package starts with `usethis::use_lifecycle()`. Each deprecation is one change covering all five steps:

1. **Version.** Read `Version` from `DESCRIPTION`; the deprecation version is the next minor release (`MAJOR.MINOR.PATCH.9xxx` → `MAJOR.(MINOR+1).0`).
2. **Warning**, first thing in the body — ahead of the validation block:

   ```r
   # Deprecated function
   lifecycle::deprecate_warn("X.Y.0", "hsi_old()", "hsi_new()")

   # Deprecated argument: default becomes lifecycle::deprecated()
   if (lifecycle::is_present(old_arg)) {
     lifecycle::deprecate_warn("X.Y.0", "hsi_fn(old_arg)", "hsi_fn(new_arg)")
   }
   ```
3. **Documentation.** Function: `@description` opens with `` `r lifecycle::badge("deprecated")` `` and a sentence naming the replacement; `@examples` are replaced by 2–3 `# Old:` / `# New:` migration pairs. Argument: the `@param` string starts with `` `r lifecycle::badge("deprecated")` ``. Re-document.
4. **Tests** per §5.8.
5. **NEWS** bullet per §9 — `` `hsi_old()` is deprecated. Use `hsi_new()` instead. ``
