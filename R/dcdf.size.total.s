dcdf.size.total <- function(g, wgt, swgt, cluster.ind, cluster, wgt1, swgt1,
   unitsize) {

################################################################################
# Function: dcdf.size.total
# Programmer: Tom Kincaid
# Date: December 3, 2002
# Last Revised: January 27, 2004
# Description:
#   This function calculates an estimate of the size-weighted, deconvoluted 
#   cumulative distribution function (CDF) for the total of a discrete resource.
#   The simulation extrapolation deconvolution method (Stefanski and Bay, 1996)
#   is use to deconvolute measurement error variance from the response.  If the
#   known sum of the size-weights of the resource is provided, the classic ratio
#   estimator is used to calculate the estimate. That estimator is the product
#   of the known sum of the size-weights of the resource and the Horvitz-
#   Thompson ratio estimator, where the latter is the ratio of two Horvitz-
#   Thompson estimators.  The numerator of the ratio estimates the size-weighted
#   total of the resource equal to or less than a specified value.  The
#   denominator of the ratio estimates the sum of the size-weights of the
#   resource.  If the known sum of the size-weights of the resource is not
#   provided, the Horvitz-Thompson estimator of the size-weighted total of
#   the resource equal to or less than a specified value is used to calculate
#   the estimate.  The function can accomodate single-stage and two-stage 
#   samples.
#   Input:
#      g = values of the deconvolution function g(.) evaluated at a specified
#         value for the response value for each site.
#      wgt = the final adjusted weight (inverse of the sample inclusion
#         probability) for each site, which is either the weight for a single-
#         stage sample or the stage two weight for a two-stage sample.
#      swgt = the size-weight for each site, which is the stage two size-weight 
#         for a two-stage sample.
#      cluster.ind = a logical value that indicates whether the sample is a two-
#         stage sample, where TRUE = a two-stage sample and FALSE = not a two-
#         stage sample.
#      cluster = the stage one sampling unit (primary sampling unit or cluster) 
#         code for each site.
#      wgt1 = the final adjusted stage one weight for each site.
#      swgt1 = the stage one size-weight for each site.
#      unitsize = the known sum of the size-weights of the resource, which for a 
#         stratified sample must be a vector containing a value for each stratum 
#         and must have the names attribute set to identify the stratum codes.  
#   Output is the deconvoluted CDF estimate
#   Other Functions Required: None
################################################################################

# Calculate additional required values

   wgt <- wgt*swgt
   if (cluster.ind) {
      cluster <- factor(cluster)
      ncluster <- length(levels(cluster))
      wgt2.lst <- split(wgt, cluster)
      wgt1 <- wgt1*swgt1
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
   }

# Calculate the cdf estimate

   if (cluster.ind) {
      temp <- array(0, c(ncluster, dim(g[[1]])[2]))
      for (i in 1:ncluster) {
         temp[i,] <- apply(g[[i]]*wgt2.lst[[i]], 2, sum)
      }
      cdf <- apply(wgt1.u*temp, 2, sum)
   } else {
      cdf <- apply(wgt*g, 2, sum)
   }

# Adjust the estimate when the sum of the size-weights of the resource is known

   if (!is.null(unitsize)) {
      if (cluster.ind)
         cdf <- unitsize*(cdf/sum(wgt1*wgt))
      else
         cdf <- unitsize*(cdf/sum(wgt))
   }

# Return the estimate

   cdf
}
