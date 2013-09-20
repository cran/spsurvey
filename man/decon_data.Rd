\name{decon_data}
\alias{decon_data}
\docType{data}
\title{Small Lakes in Florida}
\description{
  A data frame containing attributes for small lakes in Florida.
}
\usage{data(decon_data)}
\format{
  A data frame with 930 rows and the following 6 columns:
  \describe{
    \item{xcoord}{simulated x-coordinate value.}
    \item{ycoord}{simulated y-coordinate value.}
    \item{Richness}{simulated species richness value.}
    \item{Richness_25}{species richness value plus 25\% measurement error variance.}
    \item{Richness_50}{species richness value plus 50\% measurement error variance.}
    \item{Richness_100}{species richness value plus 100\% measurement error variance.}
  }
}
\examples{
data(decon_data)
head(decon_data)
}
\keyword{datasets}
