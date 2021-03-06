% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marinus.R
\name{marinus}
\alias{marinus}
\title{Marinus Cylindrical Map Projection}
\usage{
marinus(lat, lon)
}
\arguments{
\item{lat}{Vector of latitudes.}

\item{lon}{Vector of longitudes.}
}
\value{
Matrix with column names "x" and "y" containing the x and y
  coordinates in the equidistant, cylindric map projection measured in units
  of kilometers.
}
\description{
This function converts x,y coordinates measured in units of latitude and
longitude, i.e., geographic coordinates measured in decimal degrees, to
coordinates in the equidistant, cylindric map projection measured in units of
kilometers.  The projection center is defined as the midpoint in latitude-
longitude space.  The map projection is here named after Marinus of Tyre (see
J.P. Snyder. USGS Prof Paper 1395, p. 90).
}
\examples{
lat <- 45 + runif(100, -5, 5)
lon <- 120 + runif(100, -10, 10)
marinus(lat, lon)

}
\author{
Denis White
}
