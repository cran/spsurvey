\name{UT_ecoregions}
\alias{UT_ecoregions}
\docType{data}
\title{Ecoregions in Utah}
\description{
	An object of class "SpatialPolygonsDataFrame" that was created from a
	shapefile of ecoregions in Utah.
}
\usage{data(UT_ecoregions)}
\format{
  The format is: Formal class "SpatialPolygonsDataFrame" [package "sp"].\cr
  The data slot contains 10 rows and the following 3 columns:
  \describe{
    \item{Level3}{Level 3 ecoregion code for the polygon.}
    \item{Level3_Nam}{Level 3 ecoregion name for the polygon.}
    \item{Area_ha}{area of the polygon in hectares.}
  }
}
\examples{
data(UT_ecoregions)
plot(UT_ecoregions)
}
\keyword{datasets}


