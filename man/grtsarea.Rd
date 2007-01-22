\name{grtsarea}
\alias{grtsarea}
\title{Select GRTS Sample of an Area Resource}
\description{
  This function select a GRTS sample of an area resource.  This function uses
  hierarchical randomization to ensure that the sample will include no more
  than one point per cell and then picks a point in selected cells.
}
\usage{
grtsarea(shapefilename=NULL, areaframe, samplesize=100, SiteBegin=1,
   shift.grid=TRUE, startlev=NULL, maxlev=11, maxtry=1000, acept=1)
}
\arguments{
  \item{shapefilename}{name of the input shapefile.  If shapefilename equals
    NULL, then the shapefile or shapefiles in the currrent directory are used.
    The default is NULL.}
  \item{areaframe}{a data frame containing id, mdcaty and mdm.}
  \item{samplesize}{number of points to select in the sample.  The default is
    100.}
  \item{SiteBegin}{number to use for first site in the design.  The default is
    1.}
  \item{shift.grid}{option to randomly shift the hierarchical grid, where TRUE
    means shift the grid and FALSE means do not shift the grid, which is
    useful if one desires strict spatial stratification by hierarchical grid
    cells.  The default is TRUE.}
  \item{startlev}{initial number of hierarchical levels to use for the GRTS
    grid, which must be less than or equal to maxlev (if maxlev is specified)
    and cannot be greater than 11.  The default is NULL.}
  \item{maxlev}{maximum number of hierarchical levels to use for the GRTS grid,
    which cannot be greater than 11.  The default is 11.}
  \item{maxtry}{maximum number of iterations for randomly generating a point
    The default is 1000.}
  \item{acept}{parameter that controls peakedness of a center-peaked
    distribution, which must be between 0 and 1.  A value of 0 gives a
    triangular distribution, and a value of 1 gives a uniform
    distribution.  The default is 1.}
}
\value{
  A data frame of GRTS sample points containing: SiteID, id, x, y, mdcaty,
  and weight.
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
\code{\link{grts}}
}
\keyword{survey}
