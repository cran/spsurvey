\name{geodalbers}
\alias{geodalbers}
\title{Project Latitude and Longitude (Spheroid) to Albers Projection in Plane}
\description{
  Project spheroid models of the globe to Albers projection in the plane.
}
\usage{
geodalbers(lon, lat, sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)
}
\arguments{
  \item{lon}{longitude (decimal degrees) vector to be projected using Albers.}
  \item{lat}{latitude (decimal degrees) vector to be projected using Albers.}
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
  A data frame of Albers x-coordinate and y-coordinate projections for latitude 
  and longitude.
}
\references{J. Snyder, USGS Professional Paper 1395}
\author{Tony Olsen \email{Olsen.Tony@epa.gov}}
\keyword{survey}
