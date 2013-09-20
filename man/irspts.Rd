\name{irspts}
\alias{irspts}
\title{Select an Independent Random Sample (IRS) of a Finite Resource}
\description{
  This function selects an independent random sample (IRS) of a finite resource.  }
\usage{
irspts(ptsframe, samplesize=100, SiteBegin=1)
}
\arguments{
  \item{ptsframe}{a data frame containing id, x, y, mdcaty, and mdm.}
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
