\name{NE_lakes}
\alias{NE_lakes}
\docType{data}
\title{Lakes in the Southern New England Region of the U.S.}
\description{
	An object of class "SpatialPointsDataFrame" that was created from a
	shapefile of lakes in the southern New England region of the U.S.
}
\usage{data(NE_lakes)}
\format{
  The format is: Formal class "SpatialPointsDataFrame" [package "sp"].\cr
  The data slot contains 6,121 rows and the following 4 columns:
  \describe{
    \item{xcoord}{Albers projection x-coordinate of the lake centroid.}
    \item{ycoord}{Albers projection y-coordinate of the lake centroid.}
    \item{State}{state code for the lake.}
    \item{Area_Cat}{lake surface area category in hectares.}
  }
}
\examples{
data(NE_lakes)
plot(NE_lakes)
}
\keyword{datasets}


