\name{eco.l3.ut}
\alias{eco.l3.ut}
\docType{data}
\title{Example Polygons Dataset}
\description{
  This dataset is a list composed of two element, each of which is a matrix
  containing the x-coordinates and y-coordinates for a polygon.
}
\usage{data(eco.l3.ut)}
\format{
  A list containing two elements:
  \describe{
    \item{\code{1}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the first polygon.}
    \item{\code{2}}{a matrix containing two variables named xcoord and ycoord,
      which are the x-coordinates and y-coordinates for the second polygon.}
  }
}
\source{
This dataset is a subset of the Level III ecoregions in Utah.  Further 
information regarding ecoregions is available at the following web site: 
\url{http://www.epa.gov/wed/pages/ecoregions.htm}
}
\references{
Omernik, J.M., (1987). Ecoregions of the conterminous United States. \emph{Annals of 
Association of American Geographers}, \bold{77}, 1-118.
}
\seealso{
\code{\link{sp2shape}} 
}
\examples{
# This example converts the dataset to an sp package object
data(eco.l3.ut)
n <- length(eco.l3.ut)
nparts <- rep(1, n)
ringdir <- rep(1, n)
IDs <- as.character(1:n)
shapes <- vector(mode="list", length=n)
for(i in 1:n) {
   shapes[[i]] <- list(Pstart=0, verts=eco.l3.ut[[i]], 
      nVerts=nrow(eco.l3.ut[[i]]), nParts=nparts[i])
   attr(shapes[[i]], "RingDir") <- ringdir[i]
}
PolygonsList <- vector(mode="list", length=n)
for(i in 1:n) {
  PolygonsList[[i]] <- shape2spList(shape=shapes[[i]], shp.type="poly",
     ID=IDs[i])
}
att.data <- data.frame(id=1:n, area=1:n)
for(i in 1:n) {
   att.data$area[i] <- PolygonsList[[i]]@area
}
rownames(att.data) <- IDs
sp.obj <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=PolygonsList),
   data=att.data)
# To convert the sp package object to a shapefile use the following code: 
# sp2shape(sp.obj, "eco.l3.ut")
}
\keyword{datasets}
