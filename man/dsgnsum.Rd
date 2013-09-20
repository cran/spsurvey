\name{dsgnsum}
\alias{dsgnsum}
\title{Summarize the Sites Selected for a Survey Design}
\description{
  This function summarizes the sites selected for a survey design by producing
  contingency tables containing the cross-tabluation of number of sites for
  survey design variables and, optionally, for auxiliary variables.
}
\usage{
dsgnsum(sp.obj, auxvar=NULL)
}
\arguments{
  \item{sp.obj}{the sp package object of class "SpatialPointsDataFrame" produced
    by the grts function that contains survey design information and additional
    attribute (auxiliary) variables.}
  \item{auxvar}{a vector containing the names of columns from sites that
    identify auxiliary variables to be used to summarize the survey design.}
}
\value{
  A list containing the following components:
  \item{DesignSum}{ a list of contingency tables containing the cross-tabulation
    of number of sites for the following combinations of survey design
    variables:\cr
    (1) multidensity category (mdcaty) and stratum\cr
    (2) stratum and panel\cr
    (3) mdcaty, panel, and stratum}
  \item{AuxVarSum}{ a list of contingency tables containing the cross-tabulation
    of number of sites for each auxiliary variable and the design variables
    mdcaty, panel, and stratum}
  In addition the output list plus labeling information is printed to the
  console.
}
\references{
  Stevens, D.L., Jr., and A.R. Olsen. (2004). Spatially-balanced sampling of
  natural resources. Journal of the American Statistical Association \bold{99}:
  262-278.
}
\author{
Tony Olsen \email{Olsen.Tony@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\seealso{
  \code{\link{grts}}
  \code{\link{framesum}}
}
\examples{
\dontrun{
design <- list(Stratum1=list(panel=c(PanelOne=50), seltype="Equal", over=10),
   Stratum2=list(panel=c(PanelOne=50, PanelTwo=50), seltype="Unequal",
   caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25), over=75))
attframe <- read.dbf("shapefile")
samp <- grts(design=design, DesignID="Test.Site", type.frame="area",
   src.frame="shapefile", in.shape="shapefile", att.frame=attframe,
   stratum="stratum", mdcaty="mdcaty", shapefile=TRUE,
   shapefilename="sample")
dsgnsum(samp, auxvar=c("ecoregion", "state"))
}
}
\keyword{survey}
