\name{IN_streams}
\alias{IN_streams}
\docType{data}
\title{Streams in the Upper Wabash Basin in Indiana}
\description{
  A data frame containing attributes for streams in Indiana.
}
\usage{data(IN_streams)}
\format{
  A data frame with 100 rows and the following 11 columns:
  \describe{
    \item{siteID}{site ID value.}
    \item{xcoord}{Albers projection x-coordinate.}
    \item{ycoord}{Albers projection y-coordinate.}
    \item{wgt}{survey design weight.}
    \item{Strahler_Cat}{Strahler order category.}
    \item{Status}{site evaluation status code.}
    \item{TNT}{target or nontarget category for the site evaluation status code.}
    \item{IBI_Score}{IBI (index of biotic integrity) score.}
    \item{IBI_Status}{status category of the IBI score.}
    \item{QHEI_Score}{QHEI ((qualitative habitat evaluation index) score.}
    \item{QHEI_Status}{status category of the QHEI score.}
  }
}
\examples{
data(IN_streams)
head(IN_streams)
}
\keyword{datasets}


