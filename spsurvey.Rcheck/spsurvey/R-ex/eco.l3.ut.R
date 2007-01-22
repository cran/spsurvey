### Name: eco.l3.ut
### Title: Example Polygons Dataset
### Aliases: eco.l3.ut
### Keywords: datasets

### ** Examples

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



