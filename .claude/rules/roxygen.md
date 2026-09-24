---
paths:
  - "R/**/*.R"
  - "_pkgdown.yml"
---

# Roxygen documentation rules (§4)

Loaded when Claude reads package source under `R/` or `_pkgdown.yml`.

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

### 4.8 pkgdown reference index

In packages with a `_pkgdown.yml`: whenever a new exported documentation topic is added, add it to the reference index in `_pkgdown.yml` in the same change, then run `pkgdown::check_pkgdown()` — an unlisted topic fails the site build.
