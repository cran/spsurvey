\name{uniqueID}
\alias{uniqueID}
\title{Create Unique Site ID Values}
\description{
  This function creates unique site IDs by appending a unique number to each
  occurrence of a site ID.  It is intended for survey designs that have repeat
  visits to sites.
}
\usage{
uniqueID(siteID)
}
\arguments{
  \item{siteID}{the vector of site IDs.}
}
\value{
  A vector of unique site ID values.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
siteID <- paste("Site", c(1:10, 1, 3, 5), sep="")
uniqueID <- uniqueID(siteID)
}
\keyword{survey}
\keyword{misc}
