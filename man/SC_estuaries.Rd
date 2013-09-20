\name{SC_estuaries}
\alias{SC_estuaries}
\docType{data}
\title{Estuaries in South Carolina}
\description{
  A data frame containing attributes for estuaries in South Carolina.
}
\usage{data(SC_estuaries)}
\format{
  A data frame with 135 rows and the following 10 columns:
  \describe{
    \item{siteID}{site ID value.}
    \item{xcoord}{Albers projection x-coordinate.}
    \item{ycoord}{Albers projection y-coordinate.}
    \item{wgt}{survey design weight.}
    \item{Stratum}{stratum code.}
    \item{Status}{site evaluation status code.}
    \item{IBI_score}{IBI (index of biotic integrity) score.}
    \item{IBI_status}{status category of the IBI score.}
    \item{WQ_score}{WQ (water quality) score.}
    \item{WQ_status}{status category of the WQ score.}
  }
}
\examples{
data(SC_estuaries)
head(SC_estuaries)
}
\keyword{datasets}


