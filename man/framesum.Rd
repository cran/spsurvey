\name{framesum}
\alias{framesum}
\title{Summarize Frame Size for a Survey Design}
\description{
  This function summarizes the frame for a survey design.  When type.frame
  equals "finite", summary is a count of number of units in att.frame for
  cross-tabulation of stratum, mdcaty, and auxvar.  When type.frame equals
  "linear" or "area", summary is the sum of length or area for units for
  cross-tabulation of stratum, mdcaty, and auxvar.  Note that length and area
  are taken from length\_mdm and area\_mdm, which are calculated by the function
  read.dbf and added to att.frame.  If argument mdcaty or argument stratum
  equals NULL or if both arguments equal NULL, then the cross-tabulation is
  performed without use of the design variable(s).
}
\usage{
framesum(att.frame, design, type.frame="finite", stratum=NULL, mdcaty=NULL,
  auxvar=NULL, units.in="Number", scale=1, units.out="Number")
}
\arguments{
  \item{att.frame}{a data frame composed of attributes associated with elements
    in the frame, which must contain the columns used for stratum and mdcaty (if
    required by the survey design).}
  \item{design}{named list of stratum design specifications which are also
    lists.  Stratum names must be subset of values in stratum argument.  Each
    stratum list has four list components:\cr
      panel = named vector of sample sizes for each panel in stratum;\cr
      seltype = the type of random selection, which must be one of following:
        "Equal" - equal probability selection, "Unequal" - unequal probability
        selection by the categories specified in caty.n and mdcaty, or
        "Continuous" - unequal probability selection proportional to auxiliary
        variable mdcaty;\cr
      caty.n = if seltype equals "Unequal", a named vector of sample sizes for
        each category specified by mdcaty, where sum of the sample sizes must
        equal sum of the panel sample sizes, and names must be a subset of
        values in mdcaty;\cr
      over = number of replacement sites ("oversample" sites) for the entire
        design, which is set equal to 0 if none are required.}
  \item{type.frame}{the type of frame, which must be one of following: "finite",
    "linear", or "area".  The default is "finite"}
  \item{stratum}{name of the column from att.frame that identifies stratum
    membership for each element in the frame.  If stratum equals NULL, the
    design is unstratified and a column named stratum with all its elements
    equal to "None" is added to att.frame.  The default is NULL.}
  \item{mdcaty}{name of the column from att.frame that identifies the unequal
    probability category for each element in the frame.  The default is
    NULL.}
  \item{auxvar}{a vector containing the names of columns from sites that
    identify auxiliary variables to be used to summarize frame size.  The
    default is NULL.}
  \item{units.in}{a character string giving the name of units used to measure
    size in the frame.  The default is "Number".}
  \item{scale}{the scale factor used to change units.in to units.out.  For
    example, use 1000 to change "Meters" to "Kilometers".  The default is 1.}
  \item{units.out}{a character string giving the name of units used to measure
    size in the results.  The default is "Number".}
}
\value{
  A list containing the following components:
  \item{DesignSize}{ a table (for type.frame equals "finite") or an array (for
    type.frame equals "linear" or "area") that contains the cross-tabulation of
    frame extent for design variables multidensity category (mdcaty) and
    stratum, where extent of the frame is the number of sites for type.frame
    equals "finite", the sum of site length for type.frame equals "linear", or
    the sum of site area for type.frame equals "area".}
  \item{AuxVarSize}{ a list containing a component for each auxiliary variable,
    where each component of the list is one of the following: (1) if the type of
    random selection does not equal "Continuous" for any stratum, each component
    is either a table (for type.frame equals "finite") or an array (for
    type.frame equals "linear" or "area") that contains the cross-tabulation of
    frame extent for mdcaty, stratum, and the auxiliary variable or (2) if the
    type of random selection equals "Continuous" for all strata, each component
    is either a table (finite frame) or an array (linear or area frame)
    containing the cross-tabulation of frame extent for stratum and the
    auxiliary variable.}
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
  \code{\link{dsgnsum}}
}
\examples{
\dontrun{
test.attframe <- read.dbf("test.shapefile")
test.design <- list(Stratum1=list(panel=c(PanelOne=50),
   seltype="Equal", over=10), Stratum2=list(panel=c(PanelOne=50,
   PanelTwo=50), seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25,
   CatyThree=25, CatyFour=25), over=75)
framesum(att.frame=test.attframe, design=test.design, type.frame="area",
   stratum="test.stratum", mdcaty="test.mdcaty", auxvar=c("test.ecoregion",
   "test.state"), units.in="Meters", scale=1000, units.out="Kilometers")
}
}
\keyword{survey}
