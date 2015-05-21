\name{SpatialDesign}
\alias{SpatialDesign}
\title{Create Objects of Class SpatialDesign}
\description{Create objects of class \link{SpatialDesign} from
  survey design \code{list}s and objects of class
  \link{SpatialPointsDataFrame-class}.}
\usage{ 
SpatialDesign(design, sp_obj)
}
\arguments{
  \item{design}{object of class list containing specifications for the survey
    design.}
  \item{sp_obj}{object of class SpatialPointsDataFrame containing spatial
    attributes that have spatial point locations.}
}
\value{
  Returns an object of class \code{SpatialDesign}.
}
\seealso{
  \code{\link{SpatialDesign-class}}
}
\keyword{manip}
