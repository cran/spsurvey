### Name: luck.ash
### Title: Example Polygons Dataset
### Aliases: luck.ash
### Keywords: datasets

### ** Examples

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



