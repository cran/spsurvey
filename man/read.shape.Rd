\name{read.shape}
\alias{read.shape}
\title{Read an ESRI Shapefile}
\description{
  This function reads either a single shapefile or multiple shapefiles.  For 
  multiple shapefiles, all of the shapefiles must be the same type, i.e., 
  point, polyline, or polygon.
}
\usage{
read.shape(filename=NULL)
}
\arguments{
  \item{filename}{name of the shapefile without any extension.  If filename 
    equals a shapefile name, than that shapefile is read.  If filename 
    equals NULL, then all of the shapefiles in the current directory are 
    read.  The default is NULL.}
}
\value{
  An sp package object containing information in the shapefile.  The object is
  assigned class "SpatialPointsDataFrame", "SpatialLinesDataFrame", or
  "SpatialPolygonsDataFrame" corresponding to the shapefile type, i.e., point,
  polyline, or polygon, respectively.  For further information regarding the
  output object, see documentation for the sp package.
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
  read.shape("my.shapefile")
  }
}
\keyword{IO}
