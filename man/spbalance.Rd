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
spbalance(spsample, spframe = NULL, tess_ind = TRUE, sbc_ind = FALSE, nrows = 5,
   dxdy = TRUE)
}
\arguments{
  \item{spsample}{an object of class SpatialDesign produced by either the grts
    or irs functions that contains survey design information and additional
    attribute (auxiliary) variables.}
  \item{spframe}{an sp package object of class SpatialPointsDataFrame,
    SpatialLinesDataFrame, or SpatialPolygonsDataFrame that contains the survey
    design frame.  The default is NULL.}
  \item{tess_ind}{a logical variable indicating whether spatial balance metrics
   are calculated using proportions obtained from the intersection of Dirichlet
   tesselation polygons for the sample points with the frame object.  TRUE means
   calculate the metrics.  FALSE means do not calculate the metrics.  The
   default is TRUE}
  \item{sbc_ind}{a logical variable indicating whether spatial balance metrics
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
  If either the tess_ind or sbc_ind arguments are set to FALSE, the
  corresponding component in the list is set to NULL.  If tess_ind equals TRUE,
  then the list named tess is a list containing the following components:
  \item{J_subp}{Pielou evenness measure.}
  \item{chi_sq}{chi-square statistic.}
  \item{extent}{frame extent for each Dirichlet tesselation polygon.}
  \item{prop}{frame proportion for each Dirichlet tesselation polygon.}
  If sbc_ind equals TRUE, then the list named sbc is a list containing the
  following components:
  \item{J_subp}{Pielou evenness measure.}
  \item{chi_sq}{chi-square statistic.}
  \item{extent_f}{frame extent for each rectangular grid cell.}
  \item{prop_f}{frame proportion for each rectangular grid cell.}
  \item{extent_s}{sample extent for each rectangular grid cell.}
  \item{prop_s}{sample proportion for each rectangular grid cell.}
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
frame <- read.shp("shapefile")
samp <- grts(design=design, DesignID="Test.Site", type.frame="area",
   src.frame="shapefile", in.shape="shapefile", att.frame=frame@data,
   stratum="stratum", mdcaty="mdcaty", shapefile=TRUE,
   shapefilename="sample")
spbalance(samp, frame, sbc_ind = TRUE)
}
}
\keyword{survey}
