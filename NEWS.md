# HSItools (development version)

# HSItools 0.5.0

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
