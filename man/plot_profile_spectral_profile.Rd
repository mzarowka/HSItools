% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_profile_spectral_profile}
\alias{plot_profile_spectral_profile}
\title{Line plot of spectral profile from the ROI}
\usage{
plot_profile_spectral_profile(raster, extent = NULL, ext = NULL, write = FALSE)
}
\arguments{
\item{raster}{Reflectance SpatRaster.}

\item{extent}{an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.}

\item{ext}{character, a graphic format extension.}

\item{write}{logical, should resulting SpatRaster be written to file.}
}
\value{
line plot with of selected hyperspectral index.
}
\description{
Line plot of spectral profile from the ROI
}
\seealso{
Other Plotting: 
\code{\link{plot_composite}()},
\code{\link{plot_profile_spectral_series}()},
\code{\link{plot_raster_overlay}()},
\code{\link{plot_raster_proxy}()},
\code{\link{plot_raster_rgb}()},
\code{\link{stretch_raster_full}()}
}
\concept{Plotting}
