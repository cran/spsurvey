\name{NLA_2007}
\alias{NLA_2007}
\docType{data}
\title{Western Mountains Ecoregion Lakes in the EPA National Lakes Assessment (NLA) for 2007}
\description{
  A data frame containing attributes for lakes sampled by the EPA National Lakes
  Assessment (NLA) in the Western Mountains Ecoregion during 2007.
}
\usage{data(NLA_2007)}
\format{
  A data frame with 236 rows and the following 15 columns:
  \describe{
    \item{siteID}{site ID value.}
    \item{xcoord}{Albers projection x-coordinate.}
    \item{ycoord}{Albers projection y-coordinate.}
    \item{wgt}{survey design weight.}
    \item{Lake_Origin}{lake origin category.}
    \item{Chla}{chlorophyll-a concentration.}
    \item{OE5}{value of the index of macroinvertebrate taxa loss.}
    \item{PTL}{total phosphorus concentration.}
    \item{NTL}{total nitrogen concentration.}
    \item{Turbidity}{turbidity value.}
    \item{Chla_cond}{condition class category ("Good", "Fair", or "Poor") of the
      chlorophyll-a value.}
    \item{OE5_cond}{condition class category of the macroinvertebrate taxa loss
      index.}
    \item{PTL_cond}{condition class category of the total phosphorus value.}
    \item{NTL_cond}{condition class category of the total nitrogen value.}
    \item{Turbidity_cond}{condition class category of the turbidity value.}
  }
}
\examples{
data(NLA_2007)
head(NLA_2007)
}
\keyword{datasets}


