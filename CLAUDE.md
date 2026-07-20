# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Your instructions
You are situated inside of an R package source directory. The subdirectory R/ contains source files. The subdirectory tests/ contains test infrastructure (currently minimal - only tests/testthat.R exists as a test harness).

Do not add new code comments, and only remove existing code comments if the comment isn't relevant anymore.

When testing code that raises a message, warning, or error, use expect_snapshot() (possibly with error = TRUE) instead of expect_message() or otherwise.

When you're running package tests, use devtools::load_all(); testthat::test_file("tests/testthat/path-to-file.R"). If you encounter namespacing issues, don't delete tests that otherwise should work, and instead ask me what to do.

Notably, do not comment your code besides roxygen comments.

## Package Overview

HSItools is an R package for processing and visualizing hyperspectral core scanning data. The package provides tools for normalization, filtering, spectral calculations, and visualization of hyperspectral imaging (HSI) data from geological core samples.

## Development Commands

### Package Installation and Dependencies
```r
# Install from GitHub (development version)
pak::pak("mzarowka/HSItools")

# Install dependencies manually if needed
source("install.R")
```

### R Package Development Commands
```r
# Load and test package during development
devtools::load_all()

# Build documentation
devtools::document()

# Check package
devtools::check()

# Install package locally
devtools::install()
```

### Testing
```r
# Run all tests
devtools::test()

# Run a single test file (when test files exist)
devtools::load_all()
testthat::test_file("tests/testthat/test-example.R")
```

### GitHub Actions
- R-CMD-check runs on push/PR to main/master branches
- Tests on multiple R versions (devel, release, oldrel-1) and OS (macOS, Windows, Ubuntu)
- pkgdown workflow for documentation site generation

## Core Architecture

### Main Workflow Components

1. **Interactive Shiny App** (`R/app.R`, `R/simpleShiny.R`, `R/app_helpers.R`)
   - Primary entry point via `run_core()`
   - Allows users to select analysis options and interact with core images
   - Handles file selection, ROI drawing, and parameter configuration

2. **Data Processing Pipeline** (`R/workflow.R`)
   - `get_reflectance()` - Core function for extracting reflectance from raw HSI data
   - Handles normalization, integration time corrections, and extent cropping
   - Processes capture, dark reference, and white reference files
   - `standard_workflow()` - High-level function that orchestrates full analysis pipeline
   - `prepare_core()` - Alternative workflow function (see README for updated pattern)

3. **Spectral Calculations** (`R/spectral_calculations.R`)
   - Implements various spectral indices and transformations
   - Key functions: `calculate_*` family including:
     - `calculate_rabd()` - Relative Absorption Band Depth (supports "max", "mid", "strict" types)
     - `calculate_raba()` - Relative Absorption Band Area
     - `calculate_band_ratio()`, `calculate_band_difference()`, `calculate_ndi()` - Band math
     - `calculate_rmean()` - Mean reflectance across all bands
     - `calculate_lambdaremp()` - Red-edge minimum point wavelength
     - `calculate_derivative()` - Spectral derivatives (central, forward, backward methods)

4. **Filtering and Normalization** (`R/filters.R`, `R/normalization.R`)
   - Savitzky-Golay and median filtering
   - Continuum removal and spectral normalization
   - Reference raster creation and normalization

5. **Visualization** (`R/plotting.R`, `R/nickViz.R`)
   - `R/plotting.R`: RGB composite generation, spectral profile/series plotting
   - `R/nickViz.R`: Advanced visualization functions:
     - `plotSpectralDashboard()` - Comprehensive multi-panel visualization combining core photos, heatmaps, and line plots
     - `plotHeatmap()` - Depth-indexed heatmap visualization
     - `plotVerticalIndex()` - Downcore index plots with smoothing
     - `getColorsByIndex()` - Color palette mapping for different spectral indices
   - Supports PNG export and automated dashboard generation

6. **Data Extraction** (`R/extractors.R`)
   - `extract_spectral_series()` - Extract downcore spectral data, supports depth calibration
   - `extract_spectral_profile()` - Average spectral profile from ROI
   - `extract_spectral_indices()` - Extract calculated indices from spatial data
   - Spatial to spectral coordinate conversion with calibration support

### Data Structure and Workflow

**Input Data:**
- Raw HSI data in `/capture/` subdirectory with `.raw` or `.tif` files
- Three types of captures required: sample, dark reference (DARK), white reference (WHITE)
- File naming: Files are identified by patterns like "WHITE", "DARK" in filenames

**Core Object (from `run_core()`):**
- `$directory` - Base directory path
- `$rasterPaths` - Paths to capture, darkref, whiteref files
- `$layers` - Selected wavelength layers
- `$cropImage` - Extent for cropping (or NULL for full image)
- `$analysisRegions` - SpatVector of ROI polygons
- `$distances` - Calibration info (`$pixelRatio`, `$startCore`, `$endCore`, etc.)
- `$analysisOptions` - Processing settings (normalize, integration, proxies)

**Output Data:**
- `/products/` - Reflectance rasters, spectral indices (written as .tif files)
- `/photos/` - RGB, CIR, NIR images and ROI visualizations (PNG files)
- `HSItools_core.rds` - Saved core configuration object
- CSV files - Extracted spectral series and profiles

### Key Dependencies

Core packages (see DESCRIPTION for complete list):
- `terra` - Raster data handling and spatial operations (primary raster backend)
- `shiny` + `shinyFiles` + `DT` + `shinycssloaders` - Interactive web application framework
- `ggplot2` + `tidyterra` - Plotting and visualization
- `signal` + `prospectr` - Signal processing and spectral smoothing
- `sf` - Spatial data handling for ROIs
- `dplyr` + `tidyr` + `purrr` + `tibble` - Data manipulation (tidyverse)
- `magick` - Image processing for dashboard creation
- `egg` - Plot arrangement for multi-panel figures
- `fs` - Cross-platform file system operations

### File Organization

- `R/` - All R source code organized by functionality:
  - `app.R` - Main Shiny application (1379 lines)
  - `workflow.R` - Core processing pipeline (`get_reflectance()`, `standard_workflow()`)
  - `spectral_calculations.R` - Spectral index calculations
  - `extractors.R` - Data extraction utilities
  - `plotting.R` - Basic plotting functions
  - `nickViz.R` - Advanced visualization and dashboard creation
  - `filters.R`, `normalization.R` - Signal processing
  - `app_helpers.R`, `utils.R` - Helper functions
  - `data.R` - Package data documentation
- `data/` - Package data (proxies.rda)
- `data-raw/` - Scripts for generating package data
- `inst/extdata/` - Example data (CORE_XYZ/)
- `man/` - Auto-generated documentation (roxygen2)
- `tests/` - Test infrastructure (currently minimal, only testthat.R harness exists)
- `.github/workflows/` - CI/CD (R-CMD-check, pkgdown)

### Example Workflows

**Interactive Workflow (Shiny-based):**
```r
library(HSItools)

# Run interactive Shiny app to configure analysis
core <- run_core()  # Creates HSItools_core.rds automatically

# Process reflectance using saved configuration
reflectance <- get_reflectance(core)

# Create RGB visualizations
rgb_preview <- stretch_raster_full(reflectance, type = "RGB", write = TRUE)
```

**Automated Workflow (standard_workflow):**
```r
library(HSItools)

# Load previously saved core configuration
core <- readRDS("path/to/HSItools_core.rds")

# Run complete automated pipeline
# This creates all images, indices, and dashboards
standard_workflow(core, verbose = TRUE, smooth.win = NA)
```

**Updated Pattern (from README):**
```r
library(HSItools)

# Run app and prepare core
core <- run_core()
reflectance <- core |> prepare_core()

# Create visualizations
rgb_preview <- reflectance |> stretch_raster_full(extension = "tif", write = TRUE)
rgb_plot <- reflectance |> plot_raster_rgb()
```

### Important Implementation Notes

**Shiny App Architecture:**
- Uses reactive values extensively for state management
- Handles file selection through `shinyFiles` package
- Supports both directory selection (for raw data) and file selection (for existing reflectance)
- Implements custom brush selection for ROI drawing
- Auto-saves configuration to `HSItools_core.rds` when `autoSave = TRUE`

**Raster Processing:**
- Always use `terra::rast(..., noflip = TRUE)` when reading rasters to maintain proper orientation
- Window operations (`terra::window()`) are used extensively for memory-efficient ROI processing
- Always reset window with `terra::window(raster) <- NULL` after operations
- Temporary files are written to `/products/` and cleaned up systematically

**Depth Calibration:**
- `pixel_to_distance()` converts between pixel and depth coordinates
- Calibration info stored in `core$distances` with `$pixelRatio`, `$startCore`, `$endCore`
- Used by `extract_spectral_series()` and visualization functions

**File Naming Conventions:**
- Reflectance files: `REFLECTANCE_*.tif`
- Indices: Uppercase names like `RABD660670_max_*.tif`, `R570R630_*.tif`
- Images: `fullImage_RGB.png`, `roi{n}_rgb.png`, etc.
- Dashboards: `roi{n}-dashboard.pdf`, `roi{n}-{index}-dashboard.pdf`
