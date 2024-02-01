#' Find position of selected spectra
#'
#' @param raster a terra SpatRaster.
#' @param spectra vector with choice of desired spectra.
#'
#' @return positions (indices) of desired spectra in SpatRaster
#' @export
#'
#' @description find index position of the nearest spectra (band) in the dataset.
#' Match for the lowest difference between integer band and actual SpatRaster band.
#' This will produce duplicates with multiple bands. Drop.
spectra_position <- function(raster, spectra) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Find index (position) of selected spectra by comparing choice and names
  spectraIndex <- purrr::map(spectra, \(x) which.min(abs(x - as.numeric(names(raster))))) |>
    # Get positions
    purrr::as_vector()

  # Create tibble with spectra of choice and respective position
  spectraIndex <- dplyr::tibble(
    spectra = spectra,
    position = spectraIndex) |>
    # Keep second observation if duplicates are present
    # From experience closer to desired product
    dplyr::slice_tail(by = .data$position)

  # Return values
  return(spectraIndex)
}

#' Subset SpatRaster by spectra
#'
#' @param raster a terra SpatRaster to be subset.
#' @param spectra_tbl a tibble with spectra positions from spectra_position.
#'
#' @return SpatRaster subset to contain only required spectral bands.
#' @export
#'
#' @description subset SpatRaster with spectra (bands) positions.
spectra_sub <- function(raster, spectra_tbl) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # Get spectra from tibble
  spectra <- dplyr::pull(spectra_tbl, 1)

  # Get positions from tibble
  position <- dplyr::pull(spectra_tbl, 2)

  # Subset raster by position
  raster <- terra::subset(raster, position)

  # Set raster names to match spectra
  names(raster) <- as.character(spectra)

  # Return raster
  return(raster)
}

#' Crop SpatRaster
#'
#' @param raster terra SpatRaster to be cropped.
#' @param type either data raster or reference raster.
#' @param roi Region Of Interest: cropping extent.
#' @param ... additional arguments.
#'
#' @return terra SpatRaster cropped to ROI.
#' @export
#'
#' @description Crop SpatRaster to large ROI (entire core)
#' For capture (core) SpatRaster use full extent
#' For reference (white and dark) SpatRaster use only x-direction.
raster_crop <- function(raster, type, roi, ...) {
  # Store additional parameters
  params <- rlang::list2(...)

  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # If cropping entire capture SpatRaster use entire large ROI
  if (type == "capture") {
    cli::cli_alert("Captured data")

    # Crop
    raster <- terra::crop(
      raster,
      roi,
      filename = paste0(params$path, "/products/", basename(params$path), "_cropped.tif"),
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))

    # If cropping reference SpatRaster use only xmin and xmax from large ROI
    # White reference SpatRaster
  } else if (type == "whiteref") {
    cli::cli_alert("White reference")

    # Crop
    raster <- terra::crop(
      raster,
      c(terra::xmin(roi),
        terra::xmax(roi),
        terra::ymin(raster),
        terra::ymax(raster)),
      filename = paste0(params$path, "/products/WHITEREF_", basename(params$path), "_cropped.tif"),
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))

    # Dark reference SpatRaster
  } else if (type == "darkref") {
    cli::cli_alert("Dark reference")

    # Crop
    raster <- terra::crop(
      raster,
      c(terra::xmin(roi),
        terra::xmax(roi),
        terra::ymin(raster),
        terra::ymax(raster)),
      filename = paste0(params$path, "/products/DARKREF_", basename(params$path), "_cropped.tif"),
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))
  }

  # Return raster
  return(raster)
}

#' Create reference SpatRaster
#'
#' @param raster terra SpatRaster of the captured reference.
#' @param roi Region Of Interest: extent to match data raster.
#' @param ref_type type of reference, one of "whiteref" or "darkref".
#' @param ... additional arguments.
#'
#' @return a terra SpatRaster of reference matching the data raster extent.
#' @export
#'
#' @description Creating reference SpatRaster covering core extent
#' Create one mean reference row SpatRaster by averaging data every column by aggregation
#' Create reference SpatRaster matching capture SpatRaster extent by disaggregation.
create_reference_raster <- function(raster, roi, ref_type, ...) {
  # Store additional parameters
  params <- rlang::list2(...)

  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  if (ref_type == "whiteref") {
    name <- "WHITEREF"
  } else {
    name <- "DARKREF"
  }
  # Aggregate data into one row SpatRaster, divide by number of rows
  cli::cli_alert("Aggregate { name }")

  raster <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), 1),
    fun = "mean",
    overwrite = TRUE,
    steps = terra::ncell(raster) * terra::nlyr(raster))

  # Set new extent to match extent of capture SpatRaster
  terra::ext(raster) <- roi

  # Disaggregate data over entire extent to match capture SpatRaster extent, multiply by ymax
  cli::cli_alert("Disaggregate { name }")

  raster <- terra::disagg(
    raster,
    fact = c(terra::ymax(raster), 1),
    filename = paste0(params$path, "/products/", name, "_", basename(params$path), "_disaggregated.tif"),
    overwrite = TRUE,
    steps = terra::ncell(raster) * terra::nlyr(raster))

  # Return raster
  return(raster)
}

#' Raster normalization: calculation
#'
#' @param capture a terra SpatRaster of captured data.
#' @param whiteref a terra SpatRaster of the white reference matching capture extent.
#' @param darkref a terra SpatRaster of the dark reference matching capture extent.
#'
#' @return a normalized terra SpatRaster of the capture.
#'
#' @description normalize captured hyperspectral data with white and dark reference according to equation from Butz et al. 2016.
#'
normalization <- function(capture = capture, whiteref = whiteref, darkref = darkref) {
  # Calculate numerator
  numerator <- capture - darkref

  # Coerce NA to 0
  numerator[is.na(numerator)] <- 0

  # Coerce negative values to 0
  numerator[numerator < 0] <- 0

  # Calculate denominator
  denominator <- whiteref - darkref

  # Normalize
  raster <- numerator / denominator

  # Return raster
  return(raster)
}

#' Raster normalization
#'
#' @param capture terra SpatRaster of captured data.
#' @param whiteref terra SpatRaster of the white reference matching capture extent.
#' @param darkref terra SpatRaster of the dark reference matching capture extent.
#' @param fun function to apply: normalization.
#' @param ... additional arguments.
#'
#' @return normalized terra SpatRaster of the capture.
#' @export
#'
#' @description apply normalization function over the combination of capture and reference SpatRasters using terra spatial dataset.
create_normalized_raster <- function(capture = capture, whiteref = whiteref, darkref = darkref, fun = normalization, ...) {
  # Store additional parameters
  params <- rlang::list2(...)

  # Named list with write options
  wopts <- list(steps = terra::ncell(capture) * terra::nlyr(capture))

  # Create terra spatial dataset combining SpatRasters
  # Create list
  dataset <- list(capture, whiteref, darkref) |>
    # Create terra dataset
    terra::sds()

  # Apply function over the dataset and write to file
  raster <- terra::lapp(
    x = dataset,
    fun = fun,
    filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), ".tif"),
    overwrite = TRUE,
    wopt = wopts)
}
