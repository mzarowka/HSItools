#' Create structured hyperspectral metadata
#'
#' @family HSI Metadata
#' @param name Character. Capture name. A single non-empty string.
#' @param sensor_type Character. Sensor type. Default `NULL`.
#' @param manufacturer Character. Sensor manufacturer. Default `NULL`.
#' @param session_id Character. Session identifier grouping scans that share a white reference. Default `NULL`.
#' @param nrow Positive integer. Number of raster rows. Default `NULL`.
#' @param ncol Positive integer. Number of raster columns. Default `NULL`.
#' @param nlyr Positive integer. Number of raster layers. Default `NULL`.
#' @param xres Positive number. Pixel resolution in the x direction. Default `NULL`.
#' @param yres Positive number. Pixel resolution in the y direction. Default `NULL`.
#' @param spectral_resolution_nm Positive number. Spectral resolution in nm. Default `NULL`.
#' @param frame_rate_hz Positive number. Frame rate in Hz. Default `NULL`.
#' @param et_target_ms Positive number. Target integration time in ms. Default `NULL`.
#' @param et_white_ms Positive number. White reference integration time in ms. Default `NULL`.
#' @param target_start_mm Positive number. Motor position at scan start in mm. Default `NULL`.
#' @param target_stop_mm Positive number. Motor position at scan end in mm. Default `NULL`.
#' @param fov_mm Numeric. Across-track field of view in mm. Default `NULL`.
#' @param camera_position_mm Numeric. Camera position reading in mm. Default `NULL`.
#' @param stage_position_mm Numeric. Stage or focus-table position reading in mm. Default `NULL`.
#' @param scanning_speed_mm_s Numeric. Along-track scanning speed in mm/s. Default `NULL`.
#' @param spectral_binning Positive integer. Spectral binning factor. Default `NULL`.
#' @param spatial_binning Positive integer. Spatial binning factor. Default `NULL`.
#' @param wavelengths Positive numeric vector. Band centre wavelengths in nm, one value per layer. Default `NULL`.
#' @param fwhm Positive numeric vector. Band full width at half maximum in nm, one value per layer. Default `NULL`.
#' @returns An object of class `hsi_metadata`: a validated list of capture metadata fields with an internally stamped `schema_version`.
#' @examples
#' \dontrun{
#' # Minimal: name only
#' x_metadata <- hsi_create_metadata(name = "capture_01")
#'
#' # With sensor identity, dimensions, and spectral axis
#' x_metadata <- hsi_create_metadata(
#'   name = "capture_01",
#'   sensor_type = "VNIR",
#'   manufacturer = "Specim",
#'   nlyr = 224,
#'   wavelengths = seq(400, 1000, length.out = 224)
#' )
#' }
#' @export
hsi_create_metadata <- function(
  name,
  sensor_type = NULL,
  manufacturer = NULL,
  session_id = NULL,
  nrow = NULL,
  ncol = NULL,
  nlyr = NULL,
  xres = NULL,
  yres = NULL,
  spectral_resolution_nm = NULL,
  frame_rate_hz = NULL,
  et_target_ms = NULL,
  et_white_ms = NULL,
  target_start_mm = NULL,
  target_stop_mm = NULL,
  fov_mm = NULL,
  camera_position_mm = NULL,
  stage_position_mm = NULL,
  scanning_speed_mm_s = NULL,
  spectral_binning = NULL,
  spatial_binning = NULL,
  wavelengths = NULL,
  fwhm = NULL
) {
  result <- new_hsi_metadata(
    name = name,
    sensor_type = sensor_type,
    manufacturer = manufacturer,
    session_id = session_id,
    nrow = nrow,
    ncol = ncol,
    nlyr = nlyr,
    xres = xres,
    yres = yres,
    spectral_resolution_nm = spectral_resolution_nm,
    frame_rate_hz = frame_rate_hz,
    et_target_ms = et_target_ms,
    et_white_ms = et_white_ms,
    target_start_mm = target_start_mm,
    target_stop_mm = target_stop_mm,
    fov_mm = fov_mm,
    camera_position_mm = camera_position_mm,
    stage_position_mm = stage_position_mm,
    scanning_speed_mm_s = scanning_speed_mm_s,
    spectral_binning = spectral_binning,
    spatial_binning = spatial_binning,
    wavelengths = wavelengths,
    fwhm = fwhm
  )

  # Validate metadata
  result <- validate_hsi_metadata(result)

  # Return result
  result
}
