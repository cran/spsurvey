################################################################################
# Function: shape2spList
# Programmer: Tom Kincaid
# Date: July 21, 2005
# Last Revised: January 18, 2006
#
#' Internal Function: Create Object of Class Lines or Class Polygons
#'
#' This function creates an object of class Lines for a Polyline shapefile or
#' class Polygons for a Polygon shapefile.
#'
#' @param shape A single record from the .shp file of the shapefile.
#'
#' @param shp.type The type of shapefile, which is either "arc" for a Polyline
#'   shapefile or "poly" for a Polygon shapefile.
#'
#' @param ID The shape ID value, i.e., the shapefile record number.
#'
#' @return Object of class Lines for a Polyline shapefile or class Polygons
#'   for a Polygon shapefile - see documentation for the sp package for further
#'   details.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{Line}}}{sp package function to create an object of
#'       class Line}
#'     \item{\code{\link{Lines}}}{sp package function to create an object of
#'       class Lines}
#'     \item{\code{\link{Polygon}}}{sp package function to create an object of
#'       class Polygon}
#'     \item{\code{\link{Polygons}}}{sp package function to create an object of
#'       class Polygons}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

shape2spList <- function (shape, shp.type, ID) {

   nParts <- shape$nParts
   nVerts <- shape$nVerts
   Pstart <- shape$Pstart
   from <- integer(nParts)
   to <- integer(nParts)
   from[1] <- 1
   for(j in 1:nParts) {
      if(j == nParts) {
         to[j] <- nVerts
      } else {
         to[j] <- Pstart[j + 1]
         from[j + 1] <- to[j] + 1
      }
   }
   temp <- vector(mode="list", length=nParts)
   if(shp.type == "arc") {
      for(i in 1:nParts) {
         temp[[i]] <- Line(coords=shape$verts[from[i]:to[i],])
      }
      Lines <- Lines(slinelist=temp, ID=ID)
      return(Lines)
   } else {
      hole.ind <- as.logical(attr(shape, "RingDir") - 1)
      for(i in 1:nParts) {
         temp[[i]] <- Polygon(coords=shape$verts[from[i]:to[i],],
            hole=hole.ind[i])
      }
      Polygons <- Polygons(srl=temp, ID=ID)
      return(Polygons)
   }
}
