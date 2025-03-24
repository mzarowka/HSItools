#' Perform k-means clustering on a SpatRaster
#'
#' @family Spectral classifications
#'
#' @param raster terra SpatRaster to cluster, often PCA-transformed data.
#' @param n_clusters number of clusters to create.
#' @param iter.max maximum number of iterations for k-means algorithm.
#' @param nstart number of random starting configurations.
#' @param sample_size number of sample pixels to use for initial clustering.
#' @param use_sampling logical, whether to use sampling (TRUE) or analyze entire extent (FALSE).
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension.
#' @param filename NULL (default) to write automatically into products folder, provide full path and ext to override.
#'
#' @return A list containing the clustered SpatRaster and k-means model information.
#' @export
#'
#' @description
#' Performs k-means clustering on a SpatRaster, typically after PCA transformation.
#' This is useful for segmenting hyperspectral core data into regions with similar spectral characteristics.
#'
calculate_kmeans <- function(
  raster,
  n_clusters = 5,
  iter.max = 100,
  nstart = 25,
  sample_size = 10000,
  use_sampling = TRUE,
  extent = NULL,
  ext = NULL,
  filename = NULL
) {
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
    filename <- paste0(
      raster_src,
      "/KMEANS_",
      n_clusters,
      "clusters_",
      raster_name,
      ".tif"
    )
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
  sanitized_names <- make.names(names(raster), unique = TRUE)
  raster_clean <- raster
  names(raster_clean) <- sanitized_names

  # Decide whether to use sampling or analyze entire extent
  if (use_sampling && terra::ncell(raster_clean) > sample_size) {
    cli::cli_alert_info(
      "Sampling {sample_size} pixels for initial k-means clustering"
    )

    # Extract a sample of values
    sample_cells <- sample(
      1:terra::ncell(raster_clean),
      min(sample_size, terra::ncell(raster_clean))
    )
    sample_values <- terra::extract(raster_clean, sample_cells)

    # Remove rows with NA values
    sample_values <- sample_values[complete.cases(sample_values), ]

    # Drop ID column for clustering
    sample_matrix <- as.matrix(sample_values[, -1])
  } else {
    # Extract all values in the current window/extent
    cli::cli_alert_info(
      "Using all pixels within the extent for k-means clustering"
    )

    # Extract all values and drop cells with NAs
    all_values <- terra::values(raster_clean)
    complete_rows <- complete.cases(all_values)
    sample_matrix <- all_values[complete_rows, ]
  }

  # Check if we have enough data for clustering
  if (nrow(sample_matrix) < n_clusters) {
    rlang::abort(paste0(
      "Not enough complete data points (",
      nrow(sample_matrix),
      ") for ",
      n_clusters,
      " clusters. Try a smaller n_clusters value."
    ))
  }

  # Perform k-means clustering
  cli::cli_alert_info(
    "Performing k-means clustering with {n_clusters} clusters"
  )
  kmeans_model <- stats::kmeans(
    sample_matrix,
    centers = n_clusters,
    iter.max = iter.max,
    nstart = nstart
  )

  # Create a function to apply the clustering to each pixel
  kmeans_fun <- function(x) {
    if (any(is.na(x))) return(NA)

    # Calculate distances to each cluster center
    dists <- apply(kmeans_model$centers, 1, function(center) {
      sqrt(sum((x - center)^2))
    })

    # Return cluster with minimum distance
    which.min(dists)
  }

  # Apply clustering function to the raster with cleaned names
  cli::cli_alert_info("Applying clustering to full raster")
  clustered_raster <- terra::app(
    raster_clean,
    fun = kmeans_fun,
    filename = filename,
    overwrite = TRUE,
    wopt = wopts
  )

  # Set layer name
  names(clustered_raster) <- "cluster"

  # Reset window
  terra::window(raster) <- NULL

  # Calculate cluster statistics
  cluster_sizes <- kmeans_model$size
  cluster_props <- cluster_sizes / sum(cluster_sizes) * 100

  # Create a data frame with cluster centers mapped back to original names
  centers_df <- as.data.frame(kmeans_model$centers)
  colnames(centers_df) <- sanitized_names

  # Create name mapping for reference
  name_mapping <- data.frame(
    original_name = orig_names,
    sanitized_name = sanitized_names,
    stringsAsFactors = FALSE
  )

  # Calculate the importance of each band for differentiating clusters
  # by measuring variance of cluster centers for each band
  band_importance <- tibble::tibble(
    SanitizedBand = sanitized_names,
    Variance = apply(kmeans_model$centers, 2, var),
    RelativeImportance = apply(kmeans_model$centers, 2, var) /
      sum(apply(kmeans_model$centers, 2, var)) *
      100
  ) |>
    dplyr::arrange(desc(Variance)) |>
    dplyr::mutate(
      OriginalBand = purrr::map_chr(SanitizedBand, function(sb) {
        ob <- name_mapping$original_name[name_mapping$sanitized_name == sb]
        if (length(ob) == 0) return(sb)
        return(ob)
      })
    )

  # Prepare cluster statistics
  cluster_stats <- tibble::tibble(
    Cluster = 1:n_clusters,
    Size = cluster_sizes,
    Proportion = cluster_props
  )

  # Return results
  return(list(
    cluster_raster = clustered_raster,
    centers = kmeans_model$centers,
    centers_df = centers_df,
    cluster_stats = cluster_stats,
    band_importance = band_importance,
    name_mapping = name_mapping,
    model = kmeans_model
  ))
}

#' Find optimal number of clusters for k-means clustering of hyperspectral data
#'
#' @family Spectral classifications
#'
#' @param raster terra SpatRaster to analyze.
#' @param max_clusters maximum number of clusters to evaluate.
#' @param min_clusters minimum number of clusters to evaluate (default is 2).
#' @param methods vector of methods to use: "elbow", "silhouette", "gap", "ch_index", "db_index".
#' @param sample_size number of sample pixels to use (default is 10000).
#' @param nstart number of random starts for k-means (default is 25).
#' @param iter.max maximum iterations for k-means (default is 100).
#' @param plot logical, whether to create plots (default is TRUE).
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param ext character, a graphic format extension for saving plots.
#' @param filename NULL (default) for no file output, or specify to save plots.
#'
#' @return A list containing the optimal number of clusters according to different methods,
#'         metrics for each evaluated number of clusters, and ggplot objects.
#' @export
#'
#' @description
#' Determines the optimal number of clusters for k-means clustering using multiple methods:
#' 1. Elbow method (Within-cluster Sum of Squares)
#' 2. Silhouette method (Average silhouette width)
#' 3. Gap statistic (Comparison to null reference)
#' 4. Calinski-Harabasz index (Variance ratio criterion)
#' 5. Davies-Bouldin index (Average similarity between clusters)
#'
find_optimal_clusters <- function(
  raster,
  max_clusters = 10,
  min_clusters = 2,
  methods = c("elbow", "silhouette", "ch_index", "db_index"),
  sample_size = 10000,
  nstart = 25,
  iter.max = 100,
  plot = TRUE,
  extent = NULL,
  ext = "png",
  filename = NULL
) {
  # Check if correct class is supplied
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Validate methods
  valid_methods <- c("elbow", "silhouette", "gap", "ch_index", "db_index")
  methods <- match.arg(methods, valid_methods, several.ok = TRUE)

  # Raster source directory for filenames if needed
  if (!is.null(filename)) {
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()
  }

  # Set up extent window if provided
  if (is.null(extent)) {
    # Set window of interest
    terra::window(raster) <- terra::ext(raster)
  } else {
    # Set window of interest
    terra::window(raster) <- terra::ext(extent)
  }

  # Clean band names
  sanitized_names <- make.names(names(raster), unique = TRUE)
  raster_clean <- raster
  names(raster_clean) <- sanitized_names

  # Sample the data
  cli::cli_alert_info("Sampling {sample_size} pixels for cluster analysis")

  # Extract a sample of values
  sample_cells <- sample(
    1:terra::ncell(raster_clean),
    min(sample_size, terra::ncell(raster_clean))
  )
  sample_values <- terra::extract(raster_clean, sample_cells)

  # Remove rows with NA values
  sample_values <- sample_values[complete.cases(sample_values), ]

  # Drop ID column for clustering
  sample_matrix <- as.matrix(sample_values[, -1])

  # Check if we have enough data
  if (nrow(sample_matrix) < max_clusters * 10) {
    cli::cli_alert_warning(
      "Sample size may be too small for reliable cluster analysis. Consider reducing max_clusters."
    )
  }

  # Initialize results storage
  results <- list(
    metrics = data.frame(
      k = min_clusters:max_clusters,
      wss = NA_real_,
      silhouette = NA_real_,
      gap = NA_real_,
      ch_index = NA_real_,
      db_index = NA_real_
    ),
    optimal = list(),
    plots = list()
  )

  # Run k-means for each k value
  cli::cli_alert_info(
    "Evaluating clusters from {min_clusters} to {max_clusters}"
  )

  # Within-cluster Sum of Squares (Elbow method)
  if ("elbow" %in% methods) {
    cli::cli_alert_info(
      "Computing Within-cluster Sum of Squares (Elbow method)"
    )

    for (k in min_clusters:max_clusters) {
      km <- stats::kmeans(
        sample_matrix,
        centers = k,
        nstart = nstart,
        iter.max = iter.max
      )
      results$metrics$wss[results$metrics$k == k] <- km$tot.withinss
    }

    # Find optimal k using Elbow method
    # Calculate the second derivative
    wss_diff <- diff(results$metrics$wss)
    wss_diff2 <- diff(wss_diff)

    # The elbow is where the second derivative is maximum (where the curve bends the most)
    optimal_k_elbow <- min_clusters + which.max(abs(wss_diff2))

    # For safety, calculate alternative elbow using the "knee" method
    # The knee is where the ratio of the relative decrease in WSS to the previous WSS is maximized
    wss_ratio <- -diff(results$metrics$wss) /
      results$metrics$wss[-length(results$metrics$wss)]
    alternative_k_elbow <- min_clusters + which.max(wss_ratio)

    results$optimal$elbow <- optimal_k_elbow
    results$optimal$alternative_elbow <- alternative_k_elbow

    # Create elbow plot
    if (plot) {
      p_elbow <- ggplot2::ggplot(
        results$metrics,
        ggplot2::aes(x = k, y = wss)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_vline(
          xintercept = optimal_k_elbow,
          linetype = "dashed",
          color = "red"
        ) +
        ggplot2::geom_vline(
          xintercept = alternative_k_elbow,
          linetype = "dotted",
          color = "blue"
        ) +
        ggplot2::labs(
          title = "Elbow Method for Optimal k",
          subtitle = paste(
            "Suggested k:",
            optimal_k_elbow,
            "(red), Alternative:",
            alternative_k_elbow,
            "(blue)"
          ),
          x = "Number of Clusters (k)",
          y = "Within-cluster Sum of Squares"
        ) +
        ggplot2::theme_minimal()

      results$plots$elbow <- p_elbow

      if (!is.null(filename)) {
        elbow_file <- paste0(
          raster_src,
          "/OPTIMAL_CLUSTERS_ELBOW_",
          raster_name,
          ".",
          ext
        )
        ggplot2::ggsave(elbow_file, p_elbow, width = 8, height = 6)
      }
    }
  }

  # Silhouette method
  if ("silhouette" %in% methods) {
    cli::cli_alert_info("Computing Silhouette scores")

    # Check if the cluster package is available
    if (!requireNamespace("cluster", quietly = TRUE)) {
      cli::cli_alert_warning(
        "Package 'cluster' not available. Skipping silhouette analysis."
      )
    } else {
      for (k in min_clusters:max_clusters) {
        # Skip k=1 as silhouette is not defined for it
        if (k == 1) next

        km <- stats::kmeans(
          sample_matrix,
          centers = k,
          nstart = nstart,
          iter.max = iter.max
        )
        sil <- cluster::silhouette(km$cluster, dist(sample_matrix))
        results$metrics$silhouette[results$metrics$k == k] <- mean(sil[, 3])
      }

      # Find optimal k using Silhouette method
      optimal_k_silhouette <- results$metrics$k[which.max(
        results$metrics$silhouette
      )]
      results$optimal$silhouette <- optimal_k_silhouette

      # Create silhouette plot
      if (plot) {
        p_silhouette <- ggplot2::ggplot(
          results$metrics,
          ggplot2::aes(x = k, y = silhouette)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::geom_vline(
            xintercept = optimal_k_silhouette,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::labs(
            title = "Silhouette Method for Optimal k",
            subtitle = paste("Suggested k:", optimal_k_silhouette),
            x = "Number of Clusters (k)",
            y = "Average Silhouette Width"
          ) +
          ggplot2::theme_minimal()

        results$plots$silhouette <- p_silhouette

        if (!is.null(filename)) {
          silhouette_file <- paste0(
            raster_src,
            "/OPTIMAL_CLUSTERS_SILHOUETTE_",
            raster_name,
            ".",
            ext
          )
          ggplot2::ggsave(silhouette_file, p_silhouette, width = 8, height = 6)
        }
      }
    }
  }

  # Gap statistic method
  if ("gap" %in% methods) {
    cli::cli_alert_info("Computing Gap statistic (this may take a while)")

    # Check if the cluster package is available
    if (!requireNamespace("cluster", quietly = TRUE)) {
      cli::cli_alert_warning(
        "Package 'cluster' not available. Skipping gap statistic."
      )
    } else {
      # Reduce computations for gap statistic if sample is large
      if (nrow(sample_matrix) > 1000) {
        gap_sample <- sample_matrix[sample(1:nrow(sample_matrix), 1000), ]
      } else {
        gap_sample <- sample_matrix
      }

      # Calculate gap statistic
      gap_stat <- cluster::clusGap(
        gap_sample,
        FUN = stats::kmeans,
        nstart = nstart,
        K.max = max_clusters,
        B = 10 # Reduced for speed; increase for better estimates
      )

      # Extract gap values
      for (k in min_clusters:max_clusters) {
        results$metrics$gap[results$metrics$k == k] <- gap_stat$Tab[k, "gap"]
      }

      # Find optimal k using Gap statistic
      optimal_k_gap <- cluster::maxSE(
        gap_stat$Tab[, "gap"],
        gap_stat$Tab[, "SE.sim"]
      )
      results$optimal$gap <- optimal_k_gap

      # Create gap plot
      if (plot) {
        p_gap <- ggplot2::ggplot(
          results$metrics,
          ggplot2::aes(x = k, y = gap)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::geom_vline(
            xintercept = optimal_k_gap,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::labs(
            title = "Gap Statistic Method for Optimal k",
            subtitle = paste("Suggested k:", optimal_k_gap),
            x = "Number of Clusters (k)",
            y = "Gap Statistic"
          ) +
          ggplot2::theme_minimal()

        results$plots$gap <- p_gap

        if (!is.null(filename)) {
          gap_file <- paste0(
            raster_src,
            "/OPTIMAL_CLUSTERS_GAP_",
            raster_name,
            ".",
            ext
          )
          ggplot2::ggsave(gap_file, p_gap, width = 8, height = 6)
        }
      }
    }
  }

  # Calinski-Harabasz Index
  if ("ch_index" %in% methods) {
    cli::cli_alert_info("Computing Calinski-Harabasz Index")

    # function to calculate CH index
    calc_ch_index <- function(km, data) {
      k <- length(km$size)
      n <- nrow(data)

      # Skip if only one cluster
      if (k <= 1) return(NA_real_)

      # Between-cluster sum of squares
      between_ss <- km$betweenss

      # Within-cluster sum of squares
      within_ss <- km$tot.withinss

      # CH index formula
      ch <- (between_ss / (k - 1)) / (within_ss / (n - k))
      return(ch)
    }

    for (k in min_clusters:max_clusters) {
      km <- stats::kmeans(
        sample_matrix,
        centers = k,
        nstart = nstart,
        iter.max = iter.max
      )
      results$metrics$ch_index[results$metrics$k == k] <- calc_ch_index(
        km,
        sample_matrix
      )
    }

    # Find optimal k using CH index
    optimal_k_ch <- results$metrics$k[which.max(results$metrics$ch_index)]
    results$optimal$ch_index <- optimal_k_ch

    # Create CH index plot
    if (plot) {
      p_ch <- ggplot2::ggplot(
        results$metrics,
        ggplot2::aes(x = k, y = ch_index)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_vline(
          xintercept = optimal_k_ch,
          linetype = "dashed",
          color = "red"
        ) +
        ggplot2::labs(
          title = "Calinski-Harabasz Index for Optimal k",
          subtitle = paste("Suggested k:", optimal_k_ch),
          x = "Number of Clusters (k)",
          y = "Calinski-Harabasz Index (Higher is Better)"
        ) +
        ggplot2::theme_minimal()

      results$plots$ch_index <- p_ch

      if (!is.null(filename)) {
        ch_file <- paste0(
          raster_src,
          "/OPTIMAL_CLUSTERS_CH_",
          raster_name,
          ".",
          ext
        )
        ggplot2::ggsave(ch_file, p_ch, width = 8, height = 6)
      }
    }
  }

  # Davies-Bouldin Index
  if ("db_index" %in% methods) {
    cli::cli_alert_info("Computing Davies-Bouldin Index")

    # Function to calculate DB index
    calc_db_index <- function(km, data) {
      k <- length(km$size)

      # Skip if only one cluster
      if (k <= 1) return(NA_real_)

      # Calculate cluster dispersions (average distance within clusters)
      cluster_disp <- numeric(k)
      for (i in 1:k) {
        cluster_points <- data[km$cluster == i, , drop = FALSE]
        if (nrow(cluster_points) <= 1) {
          cluster_disp[i] <- 0
        } else {
          cluster_disp[i] <- mean(dist(cluster_points))
        }
      }

      # Calculate distances between cluster centers
      center_dist <- as.matrix(dist(km$centers))

      # Calculate DB index components
      db_components <- numeric(k)
      for (i in 1:k) {
        if (cluster_disp[i] == 0) next

        max_ratio <- 0
        for (j in 1:k) {
          if (i == j | cluster_disp[j] == 0) next

          # Skip if centers are identical
          if (center_dist[i, j] == 0) next

          ratio <- (cluster_disp[i] + cluster_disp[j]) / center_dist[i, j]
          max_ratio <- max(max_ratio, ratio)
        }
        db_components[i] <- max_ratio
      }

      # Average over all clusters
      db_index <- mean(db_components, na.rm = TRUE)
      return(db_index)
    }

    for (k in min_clusters:max_clusters) {
      km <- stats::kmeans(
        sample_matrix,
        centers = k,
        nstart = nstart,
        iter.max = iter.max
      )
      results$metrics$db_index[results$metrics$k == k] <- calc_db_index(
        km,
        sample_matrix
      )
    }

    # Find optimal k using DB index (smaller is better)
    optimal_k_db <- results$metrics$k[which.min(results$metrics$db_index)]
    results$optimal$db_index <- optimal_k_db

    # Create DB index plot
    if (plot) {
      p_db <- ggplot2::ggplot(
        results$metrics,
        ggplot2::aes(x = k, y = db_index)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_vline(
          xintercept = optimal_k_db,
          linetype = "dashed",
          color = "red"
        ) +
        ggplot2::labs(
          title = "Davies-Bouldin Index for Optimal k",
          subtitle = paste("Suggested k:", optimal_k_db),
          x = "Number of Clusters (k)",
          y = "Davies-Bouldin Index (Lower is Better)"
        ) +
        ggplot2::theme_minimal()

      results$plots$db_index <- p_db

      if (!is.null(filename)) {
        db_file <- paste0(
          raster_src,
          "/OPTIMAL_CLUSTERS_DB_",
          raster_name,
          ".",
          ext
        )
        ggplot2::ggsave(db_file, p_db, width = 8, height = 6)
      }
    }
  }

  # Create combined plot of all methods
  if (plot && length(methods) > 1) {
    # First, standardize each metric to 0-1 range for comparison
    metrics_std <- results$metrics
    std_columns <- character(0)

    # Process only the methods that were actually calculated
    for (m in methods) {
      col_name <- switch(
        m,
        "elbow" = "wss",
        "silhouette" = "silhouette",
        "gap" = "gap",
        "ch_index" = "ch_index",
        "db_index" = "db_index"
      )

      # Skip if method wasn't calculated
      if (all(is.na(metrics_std[[col_name]]))) next

      # Handle direction (some metrics higher is better, some lower is better)
      direction <- ifelse(m %in% c("silhouette", "gap", "ch_index"), 1, -1)

      # Standardize
      col_range <- range(metrics_std[[col_name]], na.rm = TRUE)
      if (diff(col_range) > 0) {
        if (direction == 1) {
          metrics_std[[paste0(col_name, "_std")]] <-
            (metrics_std[[col_name]] - col_range[1]) / diff(col_range)
        } else {
          metrics_std[[paste0(col_name, "_std")]] <-
            1 - (metrics_std[[col_name]] - col_range[1]) / diff(col_range)
        }
        # Track which standardized columns exist
        std_columns <- c(std_columns, paste0(col_name, "_std"))
      } else {
        metrics_std[[paste0(col_name, "_std")]] <- 0
        std_columns <- c(std_columns, paste0(col_name, "_std"))
      }
    }

    # Only proceed if we have standardized columns
    if (length(std_columns) > 0) {
      # Prepare data for plotting - only using columns that exist
      plot_data <- tidyr::pivot_longer(
        metrics_std,
        cols = std_columns,
        names_to = "Method",
        values_to = "Standardized_Value",
        values_drop_na = TRUE
      )

      # Create mapping for method labels
      method_labels <- c(
        "wss_std" = "Elbow",
        "silhouette_std" = "Silhouette",
        "gap_std" = "Gap",
        "ch_index_std" = "Calinski-Harabasz",
        "db_index_std" = "Davies-Bouldin"
      )

      # Apply mapping only for methods that exist
      method_factors <- method_labels[std_columns]

      # Map method names to more readable labels
      plot_data$Method <- factor(
        plot_data$Method,
        levels = names(method_factors),
        labels = method_factors
      )

      # Create combined plot
      p_combined <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = k, y = Standardized_Value, color = Method)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = "Comparison of Cluster Validation Methods",
          subtitle = "All metrics standardized (higher values suggest better clustering)",
          x = "Number of Clusters (k)",
          y = "Standardized Score (Higher is Better)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

      results$plots$combined <- p_combined

      if (!is.null(filename)) {
        combined_file <- paste0(
          raster_src,
          "/OPTIMAL_CLUSTERS_COMBINED_",
          raster_name,
          ".",
          ext
        )
        ggplot2::ggsave(combined_file, p_combined, width = 10, height = 6)
      }
    } else {
      cli::cli_alert_warning(
        "Not enough valid methods to create a combined plot"
      )
    }
  }

  # Calculate consensus optimal k
  if (length(results$optimal) > 0) {
    optimal_values <- unlist(results$optimal)
    consensus_k <- as.numeric(names(sort(
      table(optimal_values),
      decreasing = TRUE
    )[1]))
    results$optimal$consensus <- consensus_k

    cli::cli_alert_success(
      "Consensus optimal number of clusters: {consensus_k}"
    )
    for (method in names(results$optimal)[
      names(results$optimal) != "consensus"
    ]) {
      cli::cli_alert_info("{method}: {results$optimal[[method]]}")
    }
  }

  # Reset window
  terra::window(raster) <- NULL

  return(results)
}

#' Perform k-means clustering on multiple SpatRasters
#'
#' @family Spectral classifications
#'
#' @param raster_list a list of terra SpatRasters to cluster.
#' @param n_clusters number of clusters to create.
#' @param iter.max maximum number of iterations for k-means algorithm.
#' @param nstart number of random starting configurations.
#' @param sample_size number of sample pixels to use for initial clustering.
#' @param use_sampling logical, whether to use sampling (TRUE) or analyze entire extent (FALSE).
#' @param consistent_centroids logical, whether to force exactly the same centroids across all rasters.
#' @param variables vector of specific band/variable names to use, or NULL to use all.
#' @param extent an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.
#' @param filename_prefix prefix for output filenames, or NULL for no file output.
#' @param extension character, a graphic format extension.
#'
#' @return A list containing the clustered SpatRasters and k-means model information.
#' @export
#'
#' @description
#' Performs k-means clustering on multiple SpatRasters that may have different extents.
#' When consistent_centroids=TRUE, exactly the same cluster centroids are used for all rasters.
#' When consistent_centroids=FALSE, initial centroids are derived from samples of all rasters,
#' but the final centroids may vary slightly.
#'
calculate_kmeans_multiple <- function(
  raster_list,
  n_clusters = 5,
  iter.max = 100,
  nstart = 25,
  sample_size = 10000,
  use_sampling = TRUE,
  consistent_centroids = TRUE,
  variables = NULL,
  extent = NULL,
  filename_prefix = NULL,
  extension = "tif"
) {
  # Check input
  if (!is.list(raster_list) || length(raster_list) == 0) {
    rlang::abort("raster_list must be a non-empty list of SpatRasters")
  }

  if (!all(sapply(raster_list, function(r) inherits(r, "SpatRaster")))) {
    rlang::abort("All elements in raster_list must be terra SpatRasters")
  }

  # Set random seed for reproducibility
  set.seed(42)

  # Filter variables if specified
  if (!is.null(variables)) {
    raster_list <- purrr::map(raster_list, function(r) {
      if (all(variables %in% names(r))) {
        return(terra::subset(r, variables))
      } else {
        available_vars <- variables[variables %in% names(r)]
        if (length(available_vars) == 0) {
          rlang::abort(paste(
            "None of the specified variables found in raster:",
            names(r)
          ))
        }
        cli::cli_alert_warning(paste(
          "Only using variables:",
          paste(available_vars, collapse = ", ")
        ))
        return(terra::subset(r, available_vars))
      }
    })
  }

  # Apply extent if specified
  if (!is.null(extent)) {
    raster_list <- purrr::map(raster_list, function(r) {
      terra::crop(r, extent)
    })
  }

  # Sample data from all rasters for initial clustering
  if (use_sampling) {
    cli::cli_alert_info(
      "Sampling {sample_size} pixels across all rasters for initial clustering"
    )

    # Calculate how many samples to take from each raster
    samples_per_raster <- ceiling(sample_size / length(raster_list))

    # Sample from each raster
    sampled_values <- purrr::map(raster_list, function(r) {
      terra::spatSample(
        r,
        size = samples_per_raster,
        method = "random",
        na.rm = TRUE
      )
    })

    # Combine all samples
    combined_samples <- do.call(rbind, sampled_values)

    # Perform k-means on the combined sample
    cli::cli_alert_info("Performing initial k-means clustering on sampled data")
    kmeans_initial <- stats::kmeans(
      combined_samples,
      centers = n_clusters,
      iter.max = iter.max,
      nstart = nstart
    )

    initial_centers <- kmeans_initial$centers
  } else {
    initial_centers <- NULL
  }

  # Process each raster based on the strategy
  if (consistent_centroids && !is.null(initial_centers)) {
    # Force consistent centroids across all rasters
    cli::cli_alert_info("Using consistent centroids across all rasters")

    cluster_results <- purrr::map(seq_along(raster_list), function(i) {
      r <- raster_list[[i]]

      # Find closest centroid for each pixel
      cli::cli_alert_info("Processing raster {i}/{length(raster_list)}")

      # Create cluster raster using app() for memory efficiency
      cluster_raster <- terra::app(r, function(vals) {
        if (any(is.na(vals))) return(NA)

        # Calculate distance to each centroid
        dists <- apply(initial_centers, 1, function(cent) {
          sqrt(sum((vals - cent)^2))
        })

        # Return the index of the closest centroid
        return(which.min(dists))
      })

      # Set name
      names(cluster_raster) <- "cluster"

      # Write to file if specified
      if (!is.null(filename_prefix)) {
        outfile <- paste0(filename_prefix, "raster_", i, ".", extension)
        cli::cli_alert_info("Writing cluster raster to {outfile}")
        terra::writeRaster(cluster_raster, outfile, overwrite = TRUE)
      }

      # Store and return
      return(cluster_raster)
    })

    # Return with final centroids
    return(list(
      cluster_rasters = cluster_results,
      centers = initial_centers,
      centers_df = as.data.frame(initial_centers)
    ))
  } else {
    # Use terra's built-in kmeans with initial centers if available
    cli::cli_alert_info(
      "Using terra's kmeans with potentially varying centroids"
    )

    cluster_results <- purrr::map(seq_along(raster_list), function(i) {
      r <- raster_list[[i]]

      cli::cli_alert_info("Processing raster {i}/{length(raster_list)}")

      # Use initial centers if available, otherwise let terra pick them
      if (!is.null(initial_centers)) {
        cluster_raster <- terra::kmeans(r, centers = initial_centers)
      } else {
        cluster_raster <- terra::kmeans(r, centers = n_clusters)
      }

      # Write to file if specified
      if (!is.null(filename_prefix)) {
        outfile <- paste0(filename_prefix, "raster_", i, ".", extension)
        cli::cli_alert_info("Writing cluster raster to {outfile}")
        terra::writeRaster(cluster_raster, outfile, overwrite = TRUE)
      }

      return(cluster_raster)
    })

    # Extract centroids from the last clustering
    last_kmeans_attr <- attributes(cluster_results[[length(cluster_results)]])
    final_centers <- last_kmeans_attr$centers

    # Return results
    return(list(
      cluster_rasters = cluster_results,
      centers = final_centers,
      centers_df = as.data.frame(final_centers)
    ))
  }
}

#' Analyze cluster centroids to understand what each cluster represents
#'
#' @param centers_df data frame of cluster centroids
#' @param variable_names optional vector of variable names
#'
#' @return A tibble in long format with cluster centroids by variable
#' @export
analyze_cluster_centroids <- function(centers_df, variable_names = NULL) {
  # Add cluster ID if not present
  if (!"cluster" %in% names(centers_df)) {
    centers_df$cluster <- 1:nrow(centers_df)
  }

  # Rename columns if variable_names provided
  if (
    !is.null(variable_names) && length(variable_names) == ncol(centers_df) - 1
  ) {
    colnames(centers_df)[1:(ncol(centers_df) - 1)] <- variable_names
  }

  # Convert to long format for analysis
  centroids_long <- centers_df %>%
    tidyr::pivot_longer(
      cols = -cluster,
      names_to = "variable",
      values_to = "value"
    )

  return(centroids_long)
}

#' Find the most important variables for differentiating clusters
#'
#' @param centers_df data frame of cluster centroids
#' @param variable_names optional vector of variable names
#'
#' @return A tibble ranking variables by their importance in separating clusters
#' @export
rank_cluster_variables <- function(centers_df, variable_names = NULL) {
  # Apply variable names if provided
  if (
    !is.null(variable_names) && length(variable_names) == ncol(centers_df) - 1
  ) {
    colnames(centers_df)[1:(ncol(centers_df) - 1)] <- variable_names
  }

  # Get columns excluding the cluster ID
  data_cols <- setdiff(colnames(centers_df), "cluster")

  # Calculate variance of each variable across centroids
  var_importance <- tibble::tibble(
    variable = data_cols,
    variance = sapply(data_cols, function(col) var(centers_df[[col]])),
    relative_importance = NA_real_
  )

  # Calculate relative importance
  total_var <- sum(var_importance$variance)
  var_importance$relative_importance <- (var_importance$variance / total_var) *
    100

  # Sort by importance
  var_importance <- var_importance %>%
    dplyr::arrange(desc(relative_importance))

  return(var_importance)
}