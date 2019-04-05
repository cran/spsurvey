################################################################################
# Function: cell.wt
# Programmer: Tony Olsen
# Date: October 27, 2004
#
#' Total Inclusion Probablity for a Cell in a Matrix
#'
#' Calculates the total inclusion probability for a cell from a finite
#' population suvey design.
#'
#' @param cel Index value for a cell.
#'
#' @param xc x-coordinates that define the cells.
#'
#' @param yc y-coordinates that define the cells.
#'
#' @param dx Width of the cells along the x-axis.
#'
#' @param dy Width of the cells along the y-axis.
#'
#' @param pts Data frame containing x-coordinates, y-coordinates, and mdm
#'   (inclusion probability) values.
#'
#' @return The total inclusion probability for the cell.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

cell.wt <- function(cel, xc, yc, dx, dy, pts) {

   xr <- c( xc[cel] - dx, xc[cel])
   yr <- c( yc[cel] - dy, yc[cel])
   tstcell <- (xr[1] < pts$x) & (pts$x <= xr[2]) & (yr[1] < pts$y) & (pts$y <= yr[2])
   wt <- sum(pts$mdm[tstcell])
   wt
}
