# HSItools (development version)

-   `hsi_coregister()` now errors when `x` is a window, layer subset or combination of files. It warps the file on disk, so these inputs previously produced silently misregistered output.

-   `hsi_coregister()` now returns a readable raster; previously the returned object pointed at a deleted temporary file. It also warps directly to `filename` without an intermediate copy.

# HSItools 0.5.3

-   Breaking: `hsi_bind_layers()` is no longer exported (non-functional stub; design pending).

-   Breaking: removed `hsi_merge_rasters`; use `hsi_bind_rows` instead.

-   Added capture metadata sidecars: `hsi_create_metadata()`, `hsi_write_metadata()`, `hsi_read_metadata()`. One flat YAML file per capture, 32-field schema (v1.1.0), only `name` required. Reader validates fields and the schema version on read.

-   All package errors and warnings now carry the `hsitools_error` / `hsitools_warning` condition classes, so they can be caught and distinguished from {terra}/GDAL or base R conditions programmatically.

-   Internal validation helpers (`check_*` family) unified; list inputs now report all offending elements in a single error instead of failing on the first.

-   Fixed `hsi_read_metadata()` pointing at the wrong argument name in filename validation errors.

-   Added `hsi_tiled()` for parallel tiled processing of larger-than-memory rasters ({mirai} workers, VRT mosaic, automatic BIGTIFF for outputs over 4 GB).

-   `hsi_calc_reflectance` now uses the matched-dark formula with independent white/target integration times.

-   Added spatial calibration and co-registration toolchain: `hsi_calibration_from_scale`, `hsi_set_extent`, `hsi_find_extent`, `hsi_pixels_to_units`, `hsi_drop_crs`, `hsi_check_gcp`, `hsi_match_gcp`, `hsi_coregister`.

-   Added MNF: `hsi_calc_mnf` / `hsi_apply_mnf`.

-   Continuous integration on five platforms (Linux devel/release/oldrel, macOS, Windows) via GitHub Actions, including Quarto vignette builds (skipped on Windows CI due to an upstream quarto limitation; verified on the other platforms).

# HSItools 0.4.0

-   Major functions now live in separate files.

-   Created first vignette "Basic HSItools workflow".

-   Fixed and improved RABD calculation speed with `hsi_calc_rabd`

-   Added `hsi_calc_rsd` for calculation of reflectance standard deviation per pixel.

-   Added `hsi_calc_rcv` for calculation of reflectance coefficient of variation per pixel.

-   Added `hsi_calc_rmedian` for calculation of median reflectance per pixel.

-   `hsi_calc_reflectance` gained `in_memory` argument to process everything in memory, if it is available. This needs user's control and knowledge of their OS and hardware.

-   Plotting got split into separate files, too.

# HSItools 0.3.0

-   Ditched the shiny app.

-   Dropped heuristics for file naming, hardcoded paths etc.

-   Most SpatRasters now use default names from {terra}.

-   Reflectance calculation is no longer based on aggregate / disaggregate functions and resampling.

-   Most functions gain *hsi\_* prefix.

-   Unified arguments where possible.

-   Introduce tests for some functions.

-   `hsi_smooth_savgol` now uses {[gsignal](https://CRAN.R-project.org/package=gsignal "https://CRAN.R-project.org/package=gsignal")}

# HSItools 0.2.0

-   Last release with a shiny app.

# HSItools 0.1.0

-   Initial GitHub.
