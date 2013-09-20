\name{read.dbf}
\alias{read.dbf}
\title{Read the Attribute (dbf) File of an ESRI Shapefile}
\description{
  This function reads either a single dbf file or multiple dbf files.  For 
  multiple dbf files, all of the dbf files must have the same variable names.
}
\usage{
read.dbf(filename=NULL)
}
\arguments{
  \item{filename}{name of the dbf file without any extension.  If filename 
    equals a dbf file name, then that dbf file is read.  If filename 
    equals NULL, then all of the dbf files in the working directory are 
    read.  The default is NULL.}
}
\details{
  Function summary(), i.e., summary.SurveyFrame(), can be used to summarize the 
  the frame for a survey design.
}
\value{
  A data frame composed of either the contents of the single dbf file, when
  filename is provided, or the contents of the dbf file(s) in the working
  directory, when filename is NULL.  The data frame is assigned class
  "SurveyFrame".
}
\references{
  ESRI Shapefile Technical Description: 
  \url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\seealso{
  \code{\link{read.shape}}
  \code{\link{framesum}}
}
\examples{
  \dontrun{
  read.shape("my.dbffile")
  }
}
\keyword{survey}
