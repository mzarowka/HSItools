#' Crop SpatRaster
#'
#' @family Normalization
#'
#' @param raster terra SpatRaster to be cropped.
#' @param type either data raster or reference raster.
#' @param dir directory.
#' @param roi Region Of Interest: cropping extent.
#'
#' @return terra SpatRaster cropped to ROI.
#' @export
#'
#' @description Crop SpatRaster to large ROI (entire core)
#' For capture (core) SpatRaster use full extent
#' For reference (white and dark) SpatRaster use only x-direction.
raster_crop <- function(raster, type, dir = NULL, roi) {
  # Check if correct class is supplied.
  if (!inherits(raster, what = "SpatRaster")) {
    rlang::abort(message = "Supplied data is not a terra SpatRaster.")
  }

  # If cropping entire capture SpatRaster use entire large ROI
  if (type == "capture") {

    # Raster source directory
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()

    filename <- paste0(raster_src, "/products/", raster_name, "_cropped.tif")

    # Crop
    raster <- terra::crop(
      raster,
      roi,
      filename = filename,
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))

    # If cropping reference SpatRaster use only xmin and xmax from large ROI
    # White reference SpatRaster
  } else if (type == "whiteref") {

    # Raster source directory
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()

    filename <- paste0(raster_src, "/products/WHITEREFF_", raster_name, "_cropped.tif")

    # Crop
    raster <- terra::crop(
      raster,
      c(terra::xmin(roi),
        terra::xmax(roi),
        terra::ymin(raster),
        terra::ymax(raster)),
      filename = filename,
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))

    # Dark reference SpatRaster
  } else if (type == "darkref") {

    # Raster source directory
    raster_src <- raster |>
      terra::sources() |>
      fs::path_dir() |>
      fs::path_dir()

    # Raster source name
    raster_name <- raster |>
      terra::sources() |>
      fs::path_file() |>
      fs::path_ext_remove()

    filename <- paste0(raster_src, "/products/DARKREF_", raster_name, "_cropped.tif")

    # Crop
    raster <- terra::crop(
      raster,
      c(terra::xmin(roi),
        terra::xmax(roi),
        terra::ymin(raster),
        terra::ymax(raster)),
      filename = filename,
      overwrite = TRUE,
      steps = terra::ncell(raster) * terra::nlyr(raster))
  }

  # Return raster
  return(raster)
}

#' Create reference SpatRaster
#'
#' @family Normalization
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

  # Create 1 row template
  template_row <- terra::rast(terra::ext(c(terra::xmin(roi), terra::xmax(roi), 0, 1)), resolution = c(1, 1))

  # Create full core template
  template_core <- terra::rast(terra::ext(roi), resolution = c(1, 1), nlyrs = terra::nlyr(raster))

  # Aggregate data into one row SpatRaster, divide by number of rows
  raster <- terra::aggregate(
    raster,
    fact = c(terra::nrow(raster), 1),
    fun = "mean") |>
    terra::resample(
      template_row)

  # Set new extent to match extent of capture SpatRaster
  terra::ext(raster) <- roi

  # Scale and resample data over entire extent to match capture SpatRaster extent, multiply by ymax
  raster <- raster |>
    terra::rescale(fx = 1, fy = terra::nrow(template_core) * 2) |>
    terra::resample(
      template_core,
      filename = paste0(params$path, "/products/", name, "_", basename(params$path), "_resampled.tif"),
      overwrite = TRUE)

  # Return raster
  return(raster)
}

#' Raster normalization: calculation
#'
#' @family Normalization
#'
#' @param capture a terra SpatRaster of captured data.
#' @param whiteref a terra SpatRaster of the white reference matching capture extent.
#' @param darkref a terra SpatRaster of the dark reference matching capture extent.
#' @param tintw integration time of the white reference.
#' @param tints integration time of the captured data (sample).
#'
#' @return a normalized terra SpatRaster of the capture.
#'
#' @description normalize captured hyperspectral data with white and dark reference according to equation from Butz et al. 2016.
#'
normalization <- function(capture = capture, whiteref = whiteref, darkref = darkref, tintw = tintw, tints = tints) {
  # Calculate numerator
  numerator <- capture - darkref

  # Coerce NA to 0
  numerator[is.na(numerator)] <- 0

  # Coerce negative values to 0
  numerator[numerator < 0] <- 0

  # Calculate denominator
  denominator <- whiteref - darkref

  # Calculate integration fraction
  f_tint <- tintw / tints

  # Normalize
  raster <- numerator / denominator

  # Correct with tint
  raster <- raster * f_tint

  # Return raster
  return(raster)
}

#' Raster normalization
#'
#' @family Normalization
#'
#' @param capture terra SpatRaster of captured data.
#' @param whiteref terra SpatRaster of the white reference matching capture extent.
#' @param darkref terra SpatRaster of the dark reference matching capture extent.
#' @param tintw integration time of the white reference.
#' @param tints integration time of the captured data (sample).
#' @param fun function to apply: normalization.
#' @param ... additional arguments.
#'
#' @return normalized terra SpatRaster of the capture.
#' @export
#'
#' @description apply normalization function over the combination of capture and reference SpatRasters using terra spatial dataset.
create_normalized_raster <- function(
    capture = capture,
    whiteref = whiteref,
    darkref = darkref,
    tintw = tintw,
    tints = tints,
    fun = normalization,
    ...) {

  # Store additional parameters
  params <- rlang::list2(...)

  # Named list with write options
  wopts <- list(steps = terra::ncell(capture) * terra::nlyr(capture))

  # Create terra spatial dataset combining SpatRasters
  # Create list
  dataset <- list(capture, whiteref, darkref) |>
    # Create terra dataset
    terra::sds()

  # Initialize a counter for appending numbers
  counter <- 1

  # Do the while loop for file naming
  filename <- paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_ROI_", 0 + counter, ".tif")

  # Generate the full file path
  file_path <- filename

  # Check if the file exists
  while (file.exists(file_path)) {
    # Increment the counter
    counter <- counter + 1

      # Append the counter to the base file name
      file_path <- paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_ROI_", 0 + counter, ".tif")
  }

  # Apply function over the dataset and write to file
  raster <- terra::lapp(
    x = dataset,
    fun = fun,
    filename = file_path,
    tints = tints,
    tintw = tintw,
    overwrite = TRUE,
    wopt = wopts)
}
