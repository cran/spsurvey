\name{grts}
\alias{grts}
\title{Generalized Random-Tessellation Stratified (GRTS) Survey Design}
\description{
  Selects a sample using a generalized random-tessellation stratified (GRTS)
  survey design. The GRTS survey design may include stratification, unequal
  probability using categories, unequal selection proportional to an auxiliary
  variable, survey over time structures, and provision for an oversample.
}
\usage{
grts(design, DesignID="Site", SiteBegin=1, type.frame="finite",
   src.frame="shapefile", in.shape=NULL, sp.object=NULL,  att.frame=NULL,
   id=NULL, xcoord=NULL, ycoord=NULL, stratum=NULL, mdcaty=NULL, startlev=NULL,
   maxlev=11, maxtry=1000, shift.grid=TRUE, do.sample=TRUE,shapefile=TRUE,
   prjfilename=NULL, out.shape="sample")
}
\arguments{
  \item{design}{named list of stratum design specifications, where each element of
    design is a list containing the design specifications for a stratum.  For
    an unstratified sample, design contains a single list.  If the sample is
    stratified, the names in design must occur among the strata names in the
    stratum column of the attributes data frame (att.frame).  If the sample is
    unstratified, the name of the single list in design is arbitrary.  Each
    list in design has four components:\cr
      panel = named vector of sample sizes for each panel in stratum\cr
      seltype = the type of random selection, which must be one of following:
        "Equal" - equal probability selection, "Unequal" - unequal probability
        selection by the categories specified in caty.n and mdcaty, or
        "Continuous" - unequal probability selection proportional to auxiliary
        variable mdcaty\cr
      caty.n = if seltype equals "Unequal", a named vector of sample sizes for
        each category specified by mdcaty, where sum of the sample sizes must
        equal sum of the panel sample sizes, and names must be a subset of
        values in mdcaty\cr
      over = number of replacement sites ("oversample" sites) for the entire
        design, which is set equal to 0 if none are required\cr\cr
    Example design for a stratified sample:\cr
      design <- list("Stratum 1"=list(panel=c(Panel=50), seltype="Equal",
        over=10),\cr "Stratum 2"=list(panel=c("Panel One"=50, "Panel Two"=50),
        seltype="Unequal",\cr caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25,
        CatyFour=25), over=75))\cr\cr
    Example design for an unstratified sample:\cr
      design <- list(None=list(panel=c(Panel1=50, Panel2=100, Panel3=50),
        seltype="Unequal",\cr caty.n=c("Caty 1"=50, "Caty 2"=25, "Caty 3"=25,
        "Caty 4"=25, "Caty 5"=75), over=100))\cr}
  \item{DesignID}{name for the design, which is used to create a site
    ID for each site.  The default is "Site".}
  \item{SiteBegin}{number to use for first site in the design.  The default is
    1.}
  \item{type.frame}{the type of frame, which must be one of following: "finite",
    "linear", or "area".  The default is "finite".}
  \item{src.frame}{source of the frame, which equals "shapefile" if the frame is
    to be read from a shapefile, "sp.object" if the frame is obtained from an sp
    package object, or "att.frame" if type.frame equals "finite" and the frame
    is included in att.frame.  The default is "shapefile".}
  \item{in.shape}{name (without any extension) of the input shapefile.  If
    src.frame equal "shapefile" and in.shape equals NULL, then the shapefile or
    shapefiles in the currrent directory are used.  The default is NULL.}
  \item{sp.object}{name of the sp package object when src.frame equals
    "sp.object".  The default is NULL.}
  \item{att.frame}{a data frame composed of attributes associated with elements
    in the frame, which must contain the columns used for stratum and mdcaty (if 
    required).  If src.frame equals "shapefile" and att.frame equals NULL, then
    att.frame is created from the dbf file(s) in the current directory.  If
    src.frame equals "att.frame", then att.frame includes columns that contain
    x-coordinates and y-coordinates for each element in the frame.  The default
    is NULL.}
  \item{id}{name of the column from att.frame that identifies the ID value for
    each element in the frame.  If id equals NULL, a column named "id" that
    contains values from one through the number of rows in att.frame is added to
    att.frame.  The default is NULL.}
  \item{xcoord}{name of the column from att.frame that identifies x-coordinates
    when src.frame equals "att.frame".  If xcoord equals NULL, then xcoord is
    given the value "x".  The default is NULL.}
  \item{ycoord}{name of the column from att.frame that identifies y-coordinates
    when src.frame equals "att.frame".  If ycoord equals NULL, then ycoord is
    given the value "y".  The default is NULL.}
  \item{stratum}{name of the column from att.frame that identifies stratum
    membership for each element in the frame.  If stratum equals NULL, the
    design is unstratified, and a column named "stratum" (with all its elements
    equal to the stratum name specified in design) is added to att.frame.  The
    default is NULL.}
  \item{mdcaty}{name of the column from att.frame that identifies the unequal
    probability category for each element in the frame.  The default is
    NULL.}
  \item{startlev}{initial number of hierarchical levels to use for the GRTS
    grid, which must be less than or equal to maxlev (if maxlev is specified)
    and cannot be greater than 11.  The default is NULL.}
  \item{maxlev}{maximum number of hierarchical levels to use for the GRTS grid,
    which cannot be greater than 11.  The default is 11.}
  \item{maxtry}{maximum number of iterations for randomly generating a point
    within a grid cell to select a site when type.frame equals "area".  The
    default is 1000.}
  \item{shift.grid}{option to randomly shift the hierarchical grid, where TRUE
    means shift the grid and FALSE means do not shift the grid, which is
    useful if one desires strict spatial stratification by hierarchical grid
    cells.  The default is TRUE.}
  \item{do.sample}{option to select a sample, where TRUE means select a sample
    and FALSE means return the entire sample frame in reverse hierarchical
    order.  Note that FALSE can only be used when type.frame equals "points"
    and seltype equals "Equal".  The default is TRUE.}
  \item{shapefile}{option to create a shapefile containing the survey design
    information, where TRUE equals create a shapefile and FALSE equals do not
    create a shapefile.  The default is TRUE.}
  \item{prjfilename}{name (without any extension) of the projection file for the
    input shapefile, which is use to name the projection file for the output
    shapefile.  The default is NULL.}
  \item{out.shape}{name (without any extension) of the output shapefile
    containing the survey design information.  The default is "sample".}
}
\details{
  The GRTS survey design process selects a spatially balanced sample based on
  the survey design specification.\cr\cr
  Function dsgnsum(), can be used to summarize the sites selected for a survey
  design.
}
\value{
  An sp package object containing the survey design information and any
  additional attribute variables that were provided.  The object is assigned
  class "SpatialPointsDataFrame".  For further information regarding the
  output object, see documentation for the sp package.  Optionally, a
  shapefile can be created that contains the survey design information.
}
\references{
  Stevens, D.L., Jr., and A.R. Olsen. (2004). Spatially-balanced sampling of
  natural resources. \emph{Journal of the American Statistical Association}, \bold{99},
  262-278.
}
\author{
Tony Olsen \email{Olsen.Tony@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\seealso{
  \code{\link{grtspts}}
  \code{\link{grtslin}}
  \code{\link{grtsarea}}
  \code{\link{albersgeod}}
  \code{\link{dsgnsum}}
}
\examples{
\dontrun{
The following example will select a sample from an area resource.  The design
includes two strata.  For Stratum 1, an equal probability sample of size 50
will be selected for a single panel.  For Stratum 2, an unequal probability
sample of size 50 will be selected for each of two panels.  The sample for
Stratum 2 will be approportioned into samples of size 25 for each of four
unequal probability categories.  In addition both strata will include
oversamples (size 10 for Stratum 1 and size 75 for Stratum 2).  It is assumed
that a shapefile defining the polygons for the area resource is located in the
folder from which R is started.  Attribute data for the design will be read
from the dbf file of the shapefile, which is assumed to have variables named
"test.stratum" and "test.mdcaty" that specify stratum membership value and
unequal probability category, respectively, for each record in the shapefile.
A shapefile named "test.sample" containing the survey design information will
be created.
test.design <- list("Stratum 1"=list(panel=c(Panel=50), seltype="Equal",
   over=10), "Stratum 2"=list(panel=c("Panel One"=50, "Panel Two"=50),
   seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25,
   CatyFour=25), over=75))
test.attframe <- read.dbf("test.shapefile")
test.sample <- grts(design=test.design, DesignID="Test.Site", type.frame="area",
   src.frame="shapefile", in.shape="test.shapefile", att.frame=test.attframe,
   stratum="test.stratum", mdcaty="test.mdcaty", shapefile=TRUE,
   out.shape="test.sample")
}
}
\keyword{survey}
