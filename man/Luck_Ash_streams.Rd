\name{Luck_Ash_streams}
\alias{Luck_Ash_streams}
\docType{data}
\title{Streams in the Luckiamute Watershed in Oregon}
\description{
	An object of class "SpatialLinesDataFrame" that was created from a
	shapefile of streams in the Luckiamute watershed in Oregon.
}
\usage{data(Luck_Ash_streams)}
\format{
  The format is: Formal class "SpatialLinesDataFrame" [package "sp"].\cr
  The data slot contains 429 rows and the following 3 columns:
  \describe{
    \item{Per_Int}{stream type for the line segment.}
    \item{Level3_Nam}{Strahler order category for the line segment.}
    \item{Length_km}{length of the line segment in kilometers.}
  }
}
\examples{
data(Luck_Ash_streams)
plot(Luck_Ash_streams)
}
\keyword{datasets}


