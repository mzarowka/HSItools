#' Perform Principal Component Analysis on a hyperspectral SpatRaster
#'
#' @family Spectral transformations
#'
#' @param raster terra SpatRaster to transform.
#' @param n_components number of principal components to retain (default is 3).
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param sample_size number of sample pixels to use for PCA calculation (default is 10000).
#' @param use_sampling logical, whether to use sampling (TRUE) or analyze entire extent (FALSE).
#' @param scale logical, whether to scale the data before PCA (default is FALSE).
#' @param center logical, whether to center the data before PCA (default is TRUE).
#' @param top_n_bands number of top contributing bands to retain for each PC (default is 5).
#' @param include_80pct logical, whether to also identify bands contributing to 80% of variance (default is TRUE).
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products folder, provide full path and ext to override.
#'
#' @return A list containing the PCA transformed SpatRaster, importance of components, and loadings information.
#' @export
#'
#' @description
#' Performs Principal Component Analysis (PCA) on a hyperspectral SpatRaster using terra's prcomp function.
#' This helps identify the most important wavelengths/bands and reduces dimensionality before clustering.
#' 
#' By default, the function samples the data to improve efficiency with large datasets, but
#' can be configured to use the entire extent by setting use_sampling = FALSE.
#'
calculate_pca <- function(
  raster,
  n_components = 3,
  extent = NULL,
  sample_size = 10000,
  use_sampling = TRUE,
  scale = FALSE,
  center = TRUE,
  top_n_bands = 5,
  include_80pct = TRUE,
  ext = NULL,
  filename = NULL) {

# Check if correct class is supplied
if (!inherits(raster, what = "SpatRaster")) {
  rlang::abort(message = "Supplied data is not a terra SpatRaster.")
}

# Raster source directory
raster_src <- raster |>
  terra::sources() |>
  fs::path_dir()

# Raster source name
raster_name <- raster |>
  terra::sources() |>
  fs::path_file() |>
  fs::path_ext_remove()

# Check type of filename
if (is.null(filename) == TRUE) {
  filename <- paste0(raster_src, "/PCA_", raster_name, ".tif")
} else {
  filename <- fs::path(filename, ext = ext)
}

# Set up extent window if provided
if (is.null(extent)) {
  # Set window of interest
  terra::window(raster) <- terra::ext(raster)
} else {
  # Set window of interest
  terra::window(raster) <- terra::ext(extent)
}

# Named list with write options
wopts <- list(steps = terra::ncell(raster) * terra::nlyr(raster))

# Store original band names for reference
orig_names <- names(raster)

# Store a raster with sanitized names to avoid warnings
# We'll use make.names to match how terra sanitizes names
sanitized_names <- make.names(names(raster), unique = TRUE)
raster_clean <- raster
names(raster_clean) <- sanitized_names

# Create temporary raster to avoid confusion
temp_raster <- raster_clean

# Decide whether to use sampling or full extent
if (use_sampling && terra::ncell(temp_raster) > sample_size) {
  cli::cli_alert_info("Sampling {sample_size} pixels for PCA calculation")
  
  # Sample the data for PCA
  sample_cells <- sample(1:terra::ncell(temp_raster), sample_size)
  sample_data <- terra::extract(temp_raster, sample_cells)
  
  # Remove NA values
  sample_data <- sample_data[complete.cases(sample_data[, -1]), ]
  
  # Perform PCA on the sample
  pca_model <- terra::prcomp(sample_data[, -1], scale = scale, center = center)
  
} else {
  # Perform PCA on the full raster extent with clean names
  cli::cli_alert_info("Performing PCA on all pixels within the extent")
  pca_model <- terra::prcomp(temp_raster, scale = scale, center = center)
}

# Extract PCA information
rotation <- pca_model$rotation

# Use predict with the same raster that has sanitized names
cli::cli_alert_info("Applying PCA transformation to the raster")

# Apply PCA transformation
pca_raster <- terra::predict(temp_raster, pca_model, filename = filename, 
                             na.rm = TRUE, overwrite = TRUE, wopt = wopts)

# Limit to requested number of components if needed
if (n_components < terra::nlyr(pca_raster)) {
  pca_raster <- terra::subset(pca_raster, 1:n_components)
}

# Set meaningful names for the PCA components
names(pca_raster) <- paste0("PC", 1:terra::nlyr(pca_raster))

# Reset window
terra::window(raster) <- NULL

# Map original band names to sanitized names
name_mapping <- data.frame(
  original_name = orig_names,
  sanitized_name = sanitized_names,
  stringsAsFactors = FALSE
)

# Get rotation matrix with row names being the sanitized names
# This ensures we can properly map back to original names
loadings <- pca_model$rotation

# Find which original bands contribute most to each PC
# First, create a detailed loadings analysis data frame
loadings_analysis <- purrr::map(1:min(n_components, ncol(loadings)), function(i) {
  # Get absolute loadings for this PC
  abs_loadings <- abs(loadings[, i])
  
  # Sort loadings in descending order
  sorted_indices <- order(abs_loadings, decreasing = TRUE)
  
  # Get the top n bands specified by user
  top_indices <- sorted_indices[1:min(top_n_bands, length(sorted_indices))]
  
  # Calculate cumulative contribution
  cum_contribution <- cumsum(abs_loadings[sorted_indices]) / sum(abs_loadings)
  
  # Find bands that contribute to 80% of the loadings
  if (include_80pct) {
    threshold_idx <- which(cum_contribution >= 0.8)[1]
    if (!is.na(threshold_idx)) {
      indices_80percent <- sorted_indices[1:min(threshold_idx, length(sorted_indices))]
      # Add any indices from the 80% threshold that aren't already in top_indices
      extra_indices <- setdiff(indices_80percent, top_indices)
      if (length(extra_indices) > 0) {
        # If there are additional indices needed to reach 80%, include them
        combined_indices <- c(top_indices, extra_indices)
        # Re-sort by importance
        combined_indices <- combined_indices[order(abs_loadings[combined_indices], decreasing = TRUE)]
        top_indices <- combined_indices
      }
    }
  }
  
  # Store information for this PC
  pc_data <- tibble::tibble(
    PC = paste0("PC", i),
    Rank = 1:length(top_indices),
    SanitizedBand = rownames(loadings)[top_indices],
    Loading = loadings[top_indices, i],
    AbsLoading = abs_loadings[top_indices],
    ContributionPct = abs_loadings[top_indices] / sum(abs_loadings) * 100,
    CumulativePct = cumsum(abs_loadings[top_indices]) / sum(abs_loadings) * 100
  )
  
  # Map back to original band names using purrr
  pc_data <- pc_data |>
    dplyr::mutate(
      OriginalBand = purrr::map_chr(SanitizedBand, function(sb) {
        ob <- name_mapping$original_name[name_mapping$sanitized_name == sb]
        if(length(ob) == 0) return(sb)
        return(ob)
      })
    )
  
  return(pc_data)
})

# Combine all PC analyses using purrr
detailed_loadings <- purrr::list_rbind(loadings_analysis)

# Create a simplified important bands summary (top band only for compatibility)
important_bands <- tibble::tibble(
  PC = paste0("PC", 1:min(n_components, ncol(loadings)))
) |>
dplyr::mutate(
  Most_Important_Band = purrr::map_chr(PC, function(pc) {
    detailed_loadings |>
      dplyr::filter(PC == pc, Rank == 1) |>
      dplyr::pull(OriginalBand) |>
      as.character()
  }),
  Most_Important_Value = purrr::map_dbl(PC, function(pc) {
    detailed_loadings |>
      dplyr::filter(PC == pc, Rank == 1) |>
      dplyr::pull(Loading)
  })
)

# Create summary of explained variance
importance <- summary(pca_model)$importance
variance_summary <- tibble::tibble(
  Component = paste0("PC", 1:ncol(importance)),
  StandardDeviation = importance[1, ],
  ProportionOfVariance = importance[2, ],
  CumulativeVariance = importance[3, ],
  stringsAsFactors = FALSE
)

# Return results as a list
return(list(
  pca_raster = pca_raster,
  importance = variance_summary[1:min(n_components, nrow(variance_summary)), ],
  loadings = loadings[, 1:min(n_components, ncol(loadings)), drop = FALSE],
  band_names = orig_names,
  name_mapping = name_mapping,
  important_bands = important_bands,
  detailed_loadings = detailed_loadings,
  model = pca_model
))
}