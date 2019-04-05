################################################################################
# Function: selectrecordID
# Programmers: Tom Kincaid
# Date: November 15, 2010
#
#' Internal Function: Select Shapefile Record to Select Sample Point
#'
#' This function selects a shapefile record from which a sample point will be
#' selected.
#'
#' @param rdx Vector of the randomized hierarchical address identifying a grid
#'   cell that will get a sample point.
#'
#' @param cellID Vector of grid cell IDs.
#'
#' @param recordMeasure Vector of grid cell shapefile record length for
#'   polyline-type shapefiles or shapefile record area for polygon-type
#'   shapefiles.
#'
#' @param recordID Vector of grid cell shapefile record IDs.
#'
#' @param mdm Vector of multidensity multipliers for the shapefile records.
#'
#' @param id Vector of shapefile record IDs.
#'
#' @return The ID of a shapefile record.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

selectrecordID <- function(rdx, cellID, recordMeasure, recordID, mdm, id) {

   ind <- rdx == cellID
   nrec <- sum(ind)
   if(nrec > 1) {
      temp <- recordMeasure[ind] *
           mdm[match(recordID[ind], id)]
      probs <- temp/sum(temp)
      rslt <- sample(recordID[ind], 1, prob=probs)
   } else {
      rslt <- recordID[ind]
   }
   return(rslt)
}
