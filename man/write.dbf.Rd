\name{write.dbf}
\alias{write.dbf}
\title{Write a Data Frame to the Attribute (dbf) File of an ESRI Shapefile}
\description{
  This function writes a data frame to the dbf file of an ESRI shapefile.
}
\usage{
write.dbf(dframe, filename)
}
\arguments{
  \item{dframe}{a data frame to be written to the dbf file.}
  \item{filename}{name of the dbf file without any extension.}
}
\value{
  The attribute (dbf) file of an ESRI shapefile.
}
\references{
  ESRI Shapefile Technical Description: 
  \url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\seealso{
  \code{\link{read.dbf}}
}
\examples{
  \dontrun{
  write.dbf(my.data.frame, "my.dbffile")
  }
}
\keyword{survey}
