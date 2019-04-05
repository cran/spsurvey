################################################################################
# Function: sp2shape
# Programmer: Tom Kincaid
# Date: June 6, 2006
# Last Revised: June 10, 2016
#
#' Create ESRI Shapefile from sp Package Object
#'
#' This function creates an ESRI shapefile from an sp package object.  The
#' function can also accommodate an object created by the grts or irs functions
#' in spsurvey.  The type of shapefile, i.e., point, polyline, or polygon, is
#' determined by the class of the input object, which must be either
#' "SpatialDesign", "SpatialPointsDataFrame", "SpatialLinesDataFrame", or
#' "SpatialPolygonsDataFrame".
#'
#' @param sp.obj The sp package object or object created by either the grts or
#'   irs functions.
#'
#' @param shpfilename Name (without any extension) of the output shapefile.
#'   The default is "tempfile".
#'
#' @param prjfilename Name (without any extension) of the projection file for
#'   the output shapefile.  The default is NULL.
#'
#' @return An ESRI shapefile of type point, polyline, or polygon.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{writeShapeFilePoint}}{C function to create a point
#'       shapefile}
#'     \item{\code{writeShapeFilePolygon}}{C function to create a
#'       polyline or polygon shapefile}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

sp2shape <- function (sp.obj, shpfilename = "tempfile", prjfilename = NULL) {

# Create a Point shapefile

   if(class(sp.obj) %in% c("SpatialDesign", "SpatialPointsDataFrame")) {
      att.data <- sp.obj@data
      temp <- sapply(att.data, is.character)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.factor(att.data[,i])
         }
      }
      temp <- sapply(att.data, is.factor)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.character(att.data[,i])
            temp <- att.data[,i] == "" | is.na(att.data[,i])
            if(any(temp)) {
               att.data[temp,i] <- " "
            }
         }
      }
      temp <- .Call("writeShapeFilePoint", sp.obj@coords[,1], sp.obj@coords[,2],
         prjfilename, names(att.data), att.data, shpfilename)

# Create a Polyline shapefile

   } else if(class(sp.obj) == "SpatialLinesDataFrame") {
      att.data <- sp.obj@data
      temp <- sapply(att.data, is.character)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.factor(att.data[,i])
         }
      }
      temp <- sapply(att.data, is.factor)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.character(att.data[,i])
            temp <- att.data[,i] == "" | is.na(att.data[,i])
            if(any(temp)) {
               att.data[temp,i] <- " "
            }
         }
      }
      nrec <- length(sp.obj@lines)
      content.len <- numeric(nrec)
      nparts <- numeric(nrec)
      npoints <- numeric(nrec)
      parts <- numeric()
      xcoord <- numeric()
      ycoord <- numeric()
      for(i in 1:nrec) {
         nparts[i] <- length(sp.obj@lines[[i]]@Lines)
         for(j in 1:nparts[i]) {
            if(j == 1) {
               parts <- c(parts, 0)
               nextpart <- nrow(sp.obj@lines[[i]]@Lines[[j]]@coords)
            } else {
               parts <- c(parts, nextpart)
               nextpart <- nextpart +
                  nrow(sp.obj@lines[[i]]@Lines[[j]]@coords)
            }
            xcoord <- c(xcoord, sp.obj@lines[[i]]@Lines[[j]]@coords[,1])
            ycoord <- c(ycoord, sp.obj@lines[[i]]@Lines[[j]]@coords[,2])
         }
         npoints[i] <- npoints[i] + nextpart
         content.len[i] <- 22 + (2*nparts[i]) + (8*npoints[i])
      }
      filelength <- 50 + (4*nrec) + sum(content.len)
      temp <- .Call("writeShapeFilePolygon", 3, filelength,
         as.integer(content.len), as.integer(nparts), as.integer(npoints),
         as.integer(parts), xcoord, ycoord, prjfilename, names(att.data),
         att.data, shpfilename)

# Create a Polygon shapefile

   } else  if(class(sp.obj) == "SpatialPolygonsDataFrame") {
      att.data <- sp.obj@data
      temp <- sapply(att.data, is.character)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.factor(att.data[,i])
         }
      }
      temp <- sapply(att.data, is.factor)
      if(any(temp)) {
         for(i in seq(ncol(att.data))[temp]) {
            att.data[,i] <- as.character(att.data[,i])
            temp <- att.data[,i] == "" | is.na(att.data[,i])
            if(any(temp)) {
               att.data[temp,i] <- " "
            }
         }
      }
      nrec <- length(sp.obj@polygons)
      content.len <- numeric(nrec)
      nparts <- numeric(nrec)
      npoints <- numeric(nrec)
      parts <- numeric()
      xcoord <- numeric()
      ycoord <- numeric()
      for(i in 1:nrec) {
         nparts[i] <- length(sp.obj@polygons[[i]]@Polygons)
         for(j in 1:nparts[i]) {
            if(j == 1) {
               parts <- c(parts, 0)
               nextpart <- nrow(sp.obj@polygons[[i]]@Polygons[[j]]@coords)
            } else {
               parts <- c(parts, nextpart)
               nextpart <- nextpart +
                  nrow(sp.obj@polygons[[i]]@Polygons[[j]]@coords)
            }
            xcoord <- c(xcoord, sp.obj@polygons[[i]]@Polygons[[j]]@coords[,1])
            ycoord <- c(ycoord, sp.obj@polygons[[i]]@Polygons[[j]]@coords[,2])
         }
         npoints[i] <- npoints[i] + nextpart
         content.len[i] <- 22 + (2*nparts[i]) + (8*npoints[i])
      }
      filelength <- 50 + (4*nrec) + sum(content.len)
      temp <- .Call("writeShapeFilePolygon", 5, filelength,
         as.integer(content.len), as.integer(nparts), as.integer(npoints),
         as.integer(parts), xcoord, ycoord, prjfilename, names(att.data),
         att.data, shpfilename)

# Print an error message due to an improper class of input object

   } else {
      stop(paste("\nThe class of the object input to function sp2shape was \"", class(sp.obj), "\", \nwhich is not a valid class for this function.", sep=""))
   }

# Return a NULL object

   invisible(NULL)
}
