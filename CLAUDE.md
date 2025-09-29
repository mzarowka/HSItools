# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Your instructions
You are situated inside of an R package source directory. The subdirectory R/ contains source files. The subdirectory tests/testthat/ contains corresponding tests. e.g. R/task.R is tested primarily in tests/testthat/test-task.R.

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

3. **Spectral Calculations** (`R/spectral_calculations.R`)
   - Implements various spectral indices and transformations
   - Functions include: band ratios, derivatives, normalized difference indices
   - Key functions: `calculate_*` family (ndi, raba, rabd, rmean, etc.)

4. **Filtering and Normalization** (`R/filters.R`, `R/normalization.R`)
   - Savitzky-Golay and median filtering
   - Continuum removal and spectral normalization
   - Reference raster creation and normalization

5. **Visualization** (`R/plotting.R`, `R/nickViz.R`)
   - RGB composite generation and plotting
   - Spectral profile and series plotting
   - Raster overlay and proxy visualizations

6. **Data Extraction** (`R/extractors.R`)
   - ROI-based spectral data extraction
   - Profile and series extraction from core data
   - Spatial to spectral coordinate conversion

### Data Structure

- Raw HSI data expected in `/capture/` subdirectory with `.raw` or `.tif` files
- Three types of captures: sample, dark reference, white reference
- Output data includes reflectance rasters and extracted spectral data
- Supports various raster formats via `terra` package

### Key Dependencies

Core packages:
- `terra` - Raster data handling and spatial operations
- `shiny` - Interactive web application framework
- `ggplot2` + `tidyterra` - Plotting and visualization
- `signal` + `prospectr` - Signal processing for spectral data
- `sf` - Spatial data handling for ROIs

### File Organization

- `R/` - All R source code organized by functionality
- `data/` - Package data (proxies.rda)
- `data-raw/` - Scripts for generating package data
- `inst/extdata/` - Example data and test files
- `man/` - Auto-generated documentation (roxygen2)
- No formal testing structure exists yet

### Example Workflow
```r
# Standard workflow pattern
core <- run_core()  # Interactive session
reflectance <- get_reflectance(core)  # Process raw data
rgb_preview <- stretch_raster_full(reflectance, write = TRUE)  # Create outputs
```
