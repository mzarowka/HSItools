#' Set raster extent to physical units
#'
#' @family HSI Calibration
#'
#' @param raster A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param reference A [`SpatVector`][terra::SpatVector-class] with a single point
#'   in pixel space that marks the physical origin anchor on the vertical axis.
#' @param um_per_pixel Numeric. Physical size of one pixel in µm.
#' @param origin Numeric. Physical position assigned to `reference` in µm.
#'   Default `0`.
#' @param units Character. Output units for the calibrated raster extent.
#'   One of `"um"`, `"mm"`, or `"cm"`. Default `"cm"`.
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with a physically calibrated
#'   extent.
#'
#' @details
#' This keeps the current raster values unchanged, but updates the spatial
#' extent so that downstream plotting and profile extraction work in physical
#' units.
#'
#' The calibration is linear: the raster grid is converted from pixels to
#' physical units using the supplied micrometers-per-pixel ratio. The
#' horizontal axis starts at the first cell centre, and the vertical axis is
#' anchored to `reference` at `origin`.
#'
#' Importantly, at this stage, calibration metag does not carry forvard into
#' analysis products. Calibrate immediate products, where real world units are necessary.
#'
#' @seealso
#' [`hsi_calibration_from_scale()`], [`hsi_calibration_direct()`],
#' [`hsi_calibration_from_dims()`] for obtaining `um_per_pixel`.
#' [`hsi_pixels_to_units()`] for direct conversion of pixel positions.
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#'
#' # Anchor point in pixel space, e.g. the core top in the first column.
#' reference <- terra::vect(cbind(1, terra::nrow(x)), type = "points")
#'
#' # 60 µm per pixel, supplied directly.
#' um <- hsi_calibration_direct(60)
#'
#' x_cal <- hsi_set_extent(x, reference, um_per_pixel = um, units = "cm")
#' }
#' @export
hsi_set_extent <- function(
	raster,
	reference,
	um_per_pixel,
	origin = 0,
	units = "cm"
) {
	# Validate inputs
	check_spatraster(raster)
	check_crs_null(raster)

	check_spatvector(reference)
	check_crs_null(reference)
	check_geom_type(reference, allowed = "points")

	if (terra::nrow(reference) != 1) {
		cli::cli_abort(
			"{.arg reference} must contain exactly one point.",
			class = "hsitools_error"
		)
	}

	check_numeric(um_per_pixel, len = 1, positive = TRUE)
	check_numeric(origin, len = 1)
	check_one_of(units, choices = c("um", "mm", "cm"))

	pixel_size <- from_um(um_per_pixel, to = units)

	nrow <- terra::nrow(raster)
	ncol <- terra::ncol(raster)
	ref_y <- terra::geom(reference)[1, "y"]

	# Cell-centre positions in physical units.
	x_min <- 0.5 * pixel_size
	x_max <- (ncol - 0.5) * pixel_size

	# Vertical positions are anchored to the supplied reference point.
	y_min <- -(origin + (nrow - ref_y) * pixel_size)
	y_max <- -(origin + (1 - ref_y) * pixel_size)

	# Create a copy of the input raster with the new extent.
	result <- terra::deepcopy(raster)
	terra::ext(result) <- terra::ext(x_min, x_max, y_min, y_max)

	# Add units metadata to the raster.
	tags <- terra::metags(result)
	if (is.data.frame(tags) && nrow(tags) > 0) {
		tags <- tags[tags$name != "hsi_units", , drop = FALSE]
	}
	tags <- rbind(
		tags,
		data.frame(
			name = "hsi_units",
			value = units,
			domain = ""
		)
	)

	# Set metags
	terra::metags(result) <- tags

	# Return result
	result
}
