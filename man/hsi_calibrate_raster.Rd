% Generated manually for the new calibration helper.
\name{hsi_calibrate_raster}
\alias{hsi_calibrate_raster}
\title{Calibrate a raster to physical units}
\usage{
hsi_calibrate_raster(raster, reference, um_per_pixel, origin = 0, units = "cm")
}
\arguments{
  \item{raster}{A \code{SpatRaster} with hyperspectral data.}
  \item{reference}{A \code{SpatVector} with a single point in pixel space.}
  \item{um_per_pixel}{Numeric. Physical size of one pixel in \u00b5m.}
  \item{origin}{Numeric. Physical position assigned to \code{reference} in \u00b5m.}
  \item{units}{Character. Output units for the calibrated raster extent. One of \code{"um"}, \code{"mm"}, or \code{"cm"}. Default \code{"cm"}.}
}
\value{
A calibrated \code{SpatRaster}.
}
\description{
Convenience wrapper that keeps the raster values unchanged but updates the
spatial extent so downstream plotting and profile extraction work in physical
units.
}
\seealso{
\code{\link{hsi_calibration_from_scale}}, \code{\link{hsi_calibration_direct}},
\code{\link{hsi_calibration_from_dims}}, \code{\link{hsi_pixels_to_units}}
}