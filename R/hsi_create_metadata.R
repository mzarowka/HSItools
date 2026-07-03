#' Create structured hyperspectral metadata
#'
#' @param name
#' @param sensor_type
#' @param manufacturer
#' @param nrow
#' @param ncol
#' @param nlyr
#' @param xres
#' @param yres
#' @param et_target_ms
#' @param et_white_ms
#' @param target_start_mm
#' @param target_stop_mm
#' @param session_id
#' @param wavelengths
#' @param fwhm
#'
#' @returns
#'
#' @export
hsi_create_metadata <- function(
  name,
  sensor_type = NULL,
  manufacturer = NULL,
  nrow = NULL,
  ncol = NULL,
  nlyr = NULL,
  xres = NULL,
  yres = NULL,
  et_target_ms = NULL,
  et_white_ms = NULL,
  target_start_mm = NULL,
  target_stop_mm = NULL,
  session_id = NULL,
  wavelengths = NULL,
  fwhm = NULL
) {
  result <- new_hsi_metadata(
    name = name,
    sensor_type = sensor_type,
    manufacturer = manufacturer,
    nrow = nrow,
    ncol = ncol,
    nlyr = nlyr,
    xres = xres,
    yres = yres,
    et_target_ms = et_target_ms,
    et_white_ms = et_white_ms,
    target_start_mm = target_start_mm,
    target_stop_mm = target_stop_mm,
    session_id = session_id,
    wavelengths = wavelengths,
    fwhm = fwhm
  )

  # Validate metadata
  

  # Return result
  result
}
