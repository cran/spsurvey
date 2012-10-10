\name{spbalance}
\alias{spbalance}
\title{Calculate Spatial Balance Metrics for a Survey Design}
\description{
  This function calculates spatial balance metrics for a survey design.  Two
  options for calculation of spatial balance metrics are available: (1) use
  proportions obtained from the intersection of Dirichlet tesselation polygons
  for the sample points with the frame object and (2) use proportions obtained
  from a rectangular grid superimposed on the sample points and the frame
  object.  In both cases the proportions are used to calculate the spatial
  balance metrics.  Two metrics are calculated: (1) the Pielou evenness measure
  and (2) the chi-square statistic.
}
\usage{
spbalance(sp.sample, shapefilename=NULL, tess.ind=TRUE, sbc.ind=FALSE, nrows=5,
   dxdy=TRUE)
}
\arguments{
  \item{sp.sample}{the sp package object of class "SpatialPointsDataFrame"
   created by the grts or irs functions).}
  \item{shapefilename}{name of the input shapefile.  If shapefilename equal
   NULL, then the shapefile or shapefiles in the working directory are read.
   The default is NULL.}
  \item{tess.ind}{a logical variable indicating whether spatial balance metrics
   are calculated using proportions obtained from the intersection of Dirichlet
   tesselation polygons for the sample points with the frame object.  TRUE means
   calculate the metrics.  FALSE means do not calculate the metrics.  The
   default is TRUE}
  \item{sbc.ind}{a logical variable indicating whether spatial balance metrics
   are calculated using proportions obtained from a rectangular grid
   superimposed on the sample points and the frame.  TRUE means calculate the
   metrics. FALSE means do not calculate the metrics. The default is FALSE.}
  \item{nrows}{number of rows (and columns) for the grid of cells.  The default
   is 5.}
  \item{dxdy}{indicator for equal x-coordinate and y-coordinate grid cell
   increments, where TRUE means the increments are equal and FALSE means the
   increments are not equal.  The default is TRUE.}
}
\value{
  A list containing the following components:
  \item{tess}{results for spatial balance metrics using tesselation polygons.}
  \item{sbc}{results for spatial balance metrics using a rectangular grid.}
  If either the tess.ind or sbc.ind arguments are set to FALSE, the
  corresponding component in the list is set to NULL.  Otherwise, each
  component of the list is a lists that contains the following components:
  \item{J_subp}{Pielou evenness measure.}
  \item{chi_sq}{chi-square statistic.}
  \item{extent}{frame extent for each  Dirichlet tesselation polygon or
   rectangular grid cell.}
  \item{prop}{frame proportion for each Dirichlet tesselation polygon or
   rectangular grid cell.}
}
\references{
  Olsen, A. R., T. M. Kincaid, and Q. Payton (2012). Spatially balanced survey
  designs for natural resources. In R. A. Gitzen, J. J. Millspaugh, A. B.
  Cooper, and D. S. Licht (Eds.), \emph{Design and Analysis of Long-term
  Ecological Monitoring Studies}, pp. 126-150. Cambridge University Press.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
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
spbalance(samp, shapefilename="shapefile", sbc.ind=TRUE)
}
}
\keyword{survey}
