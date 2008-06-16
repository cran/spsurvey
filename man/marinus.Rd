\name{marinus}
\alias{marinus}
\title{Convert Coordinates from Latitude/Longitude to the Equidistant, Cylindric Map Projection}
\description{
  This function converts x,y coordinates measured in units of latitude and 
  longitude, i.e., geographic coordinates measured in decimal degrees, to 
  coordinates in the equidistant, cylindric map projection measured in units of
  kilometers.  The projection center is defined as the midpoint in latitude-
  longitude space.  The map projection is named after Marinus of Tyre.
}
\usage{
marinus(lat, lon)
}
\arguments{
  \item{lat}{vector of latitudes.}
  \item{lon}{vector of longitudes.}
}
\value{
  A data frame with column names "x" and "y" containing the x and y coordinates 
  in the equidistant, cylindric map projection measured in units of kilometers.
}
\references{
  J.P. Snyder. USGS Prof. Paper 1395, p. 90.
}
\author{Denis White \email{White.Denis@epa.gov}}
\examples{
lat <- 45 + runif(100, -5, 5)
lon <- 120 + runif(100, -10, 10)
marinus(lat, lon)
}
\keyword{misc}
