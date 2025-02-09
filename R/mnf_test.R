# Function to calculate MNF transformation
calculate_mnf <- function(hyperspectral_raster, num_components = NULL) {
  # Convert raster to matrix for calculations
  raster_matrix <- values(hyperspectral_raster)
  
  # Calculate noise statistics using purrr
  # Create sequence of band indices
  band_indices <- seq_len(nlyr(hyperspectral_raster))
  
  # Function to calculate noise for a single band
  calculate_band_noise <- function(band_idx) {
    band <- hyperspectral_raster[[band_idx]]
    shifted_right <- shift(band, dx = 1)
    shifted_down <- shift(band, dy = 1)
    noise_right <- (band - shifted_right) / sqrt(2)
    noise_down <- (band - shifted_down) / sqrt(2)
    noise_combined <- c(values(noise_right), values(noise_down))
    noise_combined <- noise_combined[!is.na(noise_combined)]
    var(noise_combined, na.rm = TRUE)
  }
  
  # Calculate noise variance for each band using map
  noise_variances <- band_indices %>%
    map_dbl(calculate_band_noise)
  
  # Create noise covariance matrix
  noise_cov <- diag(noise_variances)
  
  # Calculate signal covariance
  signal_cov <- cov(raster_matrix, use = "complete.obs")
  
  # Perform eigenvalue decomposition of noise-whitened covariance matrix
  noise_whitened <- solve(chol(noise_cov)) %*% signal_cov %*% solve(t(chol(noise_cov)))
  eigen_decomp <- eigen(noise_whitened)
  
  # Sort eigenvalues and eigenvectors
  sorted_indices <- order(eigen_decomp$values, decreasing = TRUE)
  eigenvalues <- eigen_decomp$values[sorted_indices]
  eigenvectors <- eigen_decomp$vectors[, sorted_indices]
  
  # If num_components not specified, use Kaiser criterion
  if(is.null(num_components)) {
    num_components <- sum(eigenvalues > 1)
  }
  
  # Calculate MNF components
  mnf_transform <- raster_matrix %*% solve(t(chol(noise_cov))) %*% 
                   eigenvectors[, 1:num_components]
  
  # Convert back to raster
  mnf_raster <- hyperspectral_raster[[1:num_components]]
  values(mnf_raster) <- mnf_transform
  
  # Create results list
  results <- list(
    mnf_raster = mnf_raster,
    eigenvalues = eigenvalues,
    eigenvectors = eigenvectors,
    noise_cov = noise_cov,
    signal_cov = signal_cov,
    num_components = num_components
  )
  
  return(results)
}

# Function to plot MNF results using tidyverse
plot_mnf_results <- function(mnf_results) {
  # Plot eigenvalues using tidyverse
  eigenvalue_plot <- tibble(
    Component = seq_along(mnf_results$eigenvalues),
    Eigenvalue = mnf_results$eigenvalues
  ) %>%
    ggplot(aes(x = Component, y = Eigenvalue)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "MNF Eigenvalues",
         x = "Component Number",
         y = "Eigenvalue")
  
  print(eigenvalue_plot)
  
  # Plot first three MNF components if available
  if(nlyr(mnf_results$mnf_raster) >= 3) {
    plotRGB(mnf_results$mnf_raster[[1:3]], 
            r = 1, g = 2, b = 3, 
            stretch = "lin")
  }
}

# Add a new function to analyze component contributions
analyze_components <- function(mnf_results) {
  tibble(
    Component = seq_along(mnf_results$eigenvalues),
    Eigenvalue = mnf_results$eigenvalues,
    Variance_Explained = mnf_results$eigenvalues / sum(mnf_results$eigenvalues) * 100,
    Cumulative_Variance = cumsum(Variance_Explained)
  ) %>%
    mutate(
      Significant = Eigenvalue > 1,
      Quality = case_when(
        Eigenvalue > mean(Eigenvalue) + 2*sd(Eigenvalue) ~ "High",
        Eigenvalue > mean(Eigenvalue) ~ "Medium",
        TRUE ~ "Low"
      )
    )
}

# Example usage:
rast_file <- "D:/HSItools_hsitools_2025-02-04_12-10-28/products/REFLECTANCE_HSItools_hsitools_2025-02-04_12-10-28_ROI_2.tif"
hyperspectral_img <- rast(rast_file)
# 
# # Calculate MNF
mnf_results <- calculate_mnf(hyperspectral_img)
# 
# # Plot results
# plot_mnf_results(mnf_results)
# 
# # Analyze components
# component_analysis <- analyze_components(mnf_results)
# print(component_analysis)