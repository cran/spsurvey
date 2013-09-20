\name{irslin}
\alias{irslin}
\title{Select an Independent Random Sample (IRS) of a Linear Resource}
\description{
  This function selects an independent random sample (IRS) of a linear resource.  }
\usage{
irslin(shapefilename=NULL, linframe, samplesize=100, SiteBegin=1)
}
\arguments{
  \item{shapefilename}{name of the input shapefile.  If shapefilename equals
    NULL, then the shapefile or shapefiles in the working directory are used.
    The default is NULL.}
  \item{linframe}{a data frame containing id, mdcaty and mdm.}
  \item{samplesize}{number of points to select in the sample.  The default is
    100.}
  \item{SiteBegin}{number to use for first site in the design.  The default is
    1.}
}
\value{
  A data frame of IRS sample points containing: SiteID, id, x, y, mdcaty,
  and weight.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\seealso{
\code{\link{irs}}
}
\keyword{survey}
