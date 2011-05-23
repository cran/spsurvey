\name{albersgeod}
\alias{albersgeod}
\title{Project Albers Projection in Plane to Latitude and Longitude (Spheroid)}
\description{
  Project Albers projection in the plane to spheroid models of the globe.
}
\usage{
albersgeod(x, y, sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)
}
\arguments{
  \item{x}{Albers x-coordinate vector to be projected to latitude/longitude.}
  \item{y}{Albers y-coordinate vector to be projected to latitude/longitude.}
  \item{sph}{spheroid options: Clarke1866, GRS80, WGS84.  The default is 
    GRS80.}
  \item{clon}{center longitude (decimal degrees).  The default is -96.}
  \item{clat}{origin latitude (decimal degrees).  The default is 23.}
  \item{sp1}{standard parallel 1 (decimal degrees).  The default is 29.5.}
  \item{sp2}{standard parallel 2 (decimal degrees).  The default is 45.5.}
}
\details{
  Ask Denis White.
}
\value{
  A data frame of latitude and longitude projections for Albers x-coordinates 
  and y-coordinates.
}
\references{J. Snyder, USGS Professional Paper 1395}
\author{Tony Olsen \email{Olsen.Tony@epa.gov}}
\keyword{survey}
