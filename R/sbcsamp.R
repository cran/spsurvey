################################################################################
# Function: sbcsamp
# Programmer: Tom Kincaid
# Date: September 29, 2011
#
#' Calculate Spatial Balance Grid Cell Extent and Proportions for a Survey Design
#'
#' This function calculates spatial balance grid cell extent and proportions
#' for a survey design.  The user must provide either sbc.frame or values for
#' dx, dy, xc, and yc.
#'
#' @param sp.sample The sp package object of class "SpatialPointsDataFrame"
#'   produced by the grts or irs functions that contains survey design
#'   information.
#' @param sbc.frame The object created by the sbcframe function.  The default is
#'   NULL.
#' @param dx Grid cell x-coordinate increment value.  The default is NULL.
#' @param dy Grid cell y-coordinate increment value.  The default is NULL.
#' @param xc Vector of grid cell x-coordinates.  The default is NULL.
#' @param yc Vector of grid cell y-coordinates.  The default is NULL.
#'
#' @return List containing the following components:
#'   \describe{
#'     \item{extent}{the sample extent for each grid cell}
#'     \item{prop}{the sample proportion for each grid cell}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{readShapeFilePts}}{C function to read the shp file of
#'       a point shapefile and return a data frame containing the x-coordinates
#'       and y-coordinates for elements in the frame}
#'     \item{\code{\link{cell.wt}}}{calculates number of points in a cell for a
#'       points object}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

sbcsamp <- function(sp.sample, sbc.frame = NULL, dx = NULL, dy = NULL,
   xc = NULL, yc = NULL) {

# Obtain the sample x-coordinates and y-coordinates from the sp.sample object
   xcoord <- sp.sample@data$xcoord
   ycoord <- sp.sample@data$ycoord

# If the sbc.frame object was provided, obtain values for dx, dy, xc, and yc
   if(!is.null(sbc.frame)) {
      dx <- sbc.frame$dx
      dy <- sbc.frame$dy
      xc <- sbc.frame$xc
      yc <- sbc.frame$yc
   }

# Calculate grid cell extent and proportion
   ncells <- length(xc)
   ptsframe <- data.frame(x=xcoord, y=ycoord, mdm=1)
   extent <- sapply(1:ncells, cell.wt, xc, yc, dx, dy, ptsframe)
   prop <- (extent/sum(extent))

# Return results
   list(extent=extent, prop=prop)
}
