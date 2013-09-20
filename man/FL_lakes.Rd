\name{FL_lakes}
\alias{FL_lakes}
\docType{data}
\title{Small Lakes in Florida}
\description{
  A data frame containing attributes for small lakes in Florida.
}
\usage{data(FL_lakes)}
\format{
  A data frame with 930 rows and the following 11 columns:
  \describe{
    \item{siteID}{site ID value.}
    \item{xcoord}{Albers projection x-coordinate.}
    \item{ycoord}{Albers projection y-coordinate.}
    \item{wgt}{survey design weight.}
    \item{Basin}{stream basin code.}
    \item{Status}{site evaluation status code.}
    \item{TNT}{target or nontarget category for the site evaluation status code.}
    \item{pH_Cat}{IBI (pH category.}
    \item{Coliform_Cat}{fecal coliform count category.}
    \item{Oxygen}{dissolved oxygen value.}
    \item{Turbidity}{turbidity value.}
  }
}
\examples{
data(FL_lakes)
head(FL_lakes)
}
\keyword{datasets}


