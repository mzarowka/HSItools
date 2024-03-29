% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{filter_median}
\alias{filter_median}
\title{Smooth raster with focal median}
\usage{
filter_median(
  raster = raster,
  window = 3,
  extent = NULL,
  ext = NULL,
  filename = NULL
)
}
\arguments{
\item{raster}{a terra SpatRaster to smooth.}

\item{window}{focal window size, default is 3.}

\item{extent}{an extent or SpatVector used to subset SpatRaster. Defaults to the entire SpatRaster.}

\item{ext}{character, a graphic format extension.}

\item{filename}{NULL (default) to write automatically into products, provide full path and ext to override.}
}
\value{
smoothed SpatRaster
}
\description{
Smooth raster with focal median
}
