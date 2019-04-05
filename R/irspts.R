################################################################################
# Function: irspts
# Programmer: Tom Kincaid
# Date: November 16, 2005
# Last Revised: December 15, 2005
#'
#' Select an Independent Random Sample (IRS) of a Finite Resource
#'
#' This function selects an IRS of a finite resource (discrete points).
#'
#' @param ptsframe Data frame containing id, x, y, mdcaty, and mdm.
#'
#' @param samplesize Number of points to select in the sample.  The default is
#'   100.
#'
#' @param SiteBegin First number to start siteID numbering.  The default is 1.
#'
#' @return  Data frame of sample points containing: siteID, id, x, y, mdcaty,
#'   and weight.
#'
#' @author Tom Kincaid   \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

irspts <- function(ptsframe, samplesize = 100, SiteBegin = 1) {

  # Pick sample points

   if(nrow(ptsframe) <= samplesize) {
      id <- ptsframe$id
   } else {
      id <- sample(ptsframe$id, samplesize, prob=ptsframe$mdm)
   }
   temp <- ptsframe[match(id, ptsframe$id), ]
   temp$id <- factor(temp$id)

# Assign Site ID

   siteID <- SiteBegin - 1 + 1:nrow(temp)

# Create the output data frame

   rho <- data.frame(siteID=siteID, id=temp$id, xcoord=temp$x, ycoord=temp$y,
      mdcaty=temp$mdcaty, wgt=1/temp$mdm)
   row.names(rho) <- 1:nrow(rho)

# Return the sample

   rho
}
