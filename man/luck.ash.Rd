\name{luck.ash}
\alias{luck.ash}
\docType{data}
\title{Example Polylines Dataset}
\description{
  This dataset is a list composed of two element, each of which is a matrix
  containing the x-coordinates and y-coordinates for a polyline.
}
\usage{data(luck.ash)}
\format{
  A list containing two elements:
  \describe{
    \item{\code{1}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the first polyline.}
    \item{\code{2}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the second polyline.}
    \item{\code{3}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the third polyline.}
    \item{\code{4}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the fourth polyline.}
    \item{\code{5}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the fifth polyline.}
  }
}
\source{
This dataset is a subset of all perennial and intermittent streams and rivers in 
the Luckiamute Watershed Council basin in Oregon.  Watershed boundaries were 
defined by the Luckiamute Watershed Council.  The GIS stream network coverage 
was obtained from the Pacific Northwest (PNW) portion of  the U.S. EPA reach 
file system (RF3).
}
\references{
Horn, C.R. and Grayman, W.M. (1993). Water-quality modeling with EPA reach file 
system.  \emph{Journal of Water Resources Planning and Management}, \bold{119}, 262-274.
}
\examples{
# This example converts the dataset to an sp package object
data(luck.ash)
n <- length(luck.ash)
nparts <- rep(1, n)
IDs <- as.character(1:n)
shapes <- vector(mode="list", length=n)
for(i in 1:n) {
   shapes[[i]] <- list(Pstart=0, verts=luck.ash[[i]], 
      nVerts=nrow(luck.ash[[i]]), nParts=nparts[i])
}
PolylinesList <- vector(mode="list", length=n)
for(i in 1:n) {
  PolylinesList[[i]] <- shape2spList(shape=shapes[[i]], shp.type="arc",
     ID=IDs[i])
}
att.data <- data.frame(id=1:n, length=rep(NA, n))
rownames(att.data) <- IDs
sp.obj <- SpatialLinesDataFrame(sl=SpatialLines(LinesList=PolylinesList),
   data=att.data)
# To convert the sp package object to a shapefile use the following code: 
# sp2shape(sp.obj, "luck.ash")
}
\keyword{datasets}
