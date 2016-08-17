\name{NRSA_2009}
\alias{NRSA_2009}
\docType{data}
\title{Western Mountains Ecoregion Rivers and Streams in the EPA National Rivers
  and Streams Assessment (NRSA) for 2004 and 2008-2009}
\description{
  A data frame containing attributes for rivers and streams sampled by the EPA
  National Rivers and Streams Assessment (NRSA) in the Western Mountains
  Ecoregion during 2004 and 2008-2009.
}
\usage{data(NRSA_2009)}
\format{
  A data frame with 668 rows and the following 13 columns:
  \describe{
    \item{siteID}{site ID value.}
    \item{xcoord}{Albers projection x-coordinate.}
    \item{ycoord}{Albers projection y-coordinate.}
    \item{wgt}{survey design weight.}
    \item{Survey}{survey identifier, which is either WSA or NRSA.}
    \item{Revisit_Site}{identifier of revisit sites for the two surveys, where
      Y = a revisit site and N = not a revisit site.}
    \item{Stream_Size}{stream size category, which is either large or small.}
    \item{NTL}{total nitrogen concentration.}
    \item{PTL}{total phosphorus concentration.}
    \item{Benthic_MMI}{value of the benthic macroinvertebrate multimetric index
      (MMI).}
    \item{NTL_Cond}{condition class category of the total nitrogen value.}
    \item{PTL_Cond}{condition class category of the total phosphorus value.}
    \item{Benthic_MMI_Cond}{condition class category of the benthic MMI value.}
  }
}
\examples{
data(NRSA_2009)
head(NRSA_2009)
}
\keyword{datasets}


