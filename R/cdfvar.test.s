cdfvar.test <- function(sampl, bounds, vartype) {

################################################################################
# Function: cdfvar.test
# Programmer: Tom Kincaid
# Date: November 2, 2000
# Last Revised: September 21, 2005
# Description:
#   This function calculates unweighted and weighted estimates of the population
#   proportions in a set of bins and estimates of the variance-covariance matrix
#   of the weighted proportions using either the local mean variance estimator
#   or the simple random sampling variance estimator.  The choice of variance
#   estimator is subject to user control.  The simple random sampling variance
#   estimator uses the independent random sample approximation to calculate
#   joint inclusion probabilities.
#   Input:
#      sampl = sample values, which is a list containing the following items:
#            z = the response value for each site.
#            wgt = the final adjusted weight (inverse of the sample inclusion
#               probability) for each site.
#            x = x-coordinate for location for each site, which may be NULL.
#            y = y-coordinate for location for each site, which may be NULL.
#      bounds = upper bounds for calculating the classes for the CDF.
#      vartype = the choice of variance estimator, where "Local" = local mean
#         estimator and "SRS" = SRS estimator.
#   Output is a list containing the following items: 
#      phat = weighted estimator of the bin proportions
#      varest = estimator of the variance-covariance matrix
#      df = degrees of freedom of the local mean variance-covariance estimator
#   Other Functions Required:
#      localmean.weight - calculate the weighting matrix for the local mean
#         variance/covariance estimator
#      localmean.cov - calculate the variance/covariance matrix using the
#         local mean estimator
#      localmean.df - calculate the degrees of freedom of the local mean 
#         variance-covariance estimator
################################################################################

# Assign input values

   z <- sampl$z
   x <- sampl$x
   y <- sampl$y
   wgt <- sampl$wgt

# Calculate other required input values

   n <- length(z)
   tw <- sum(wgt)
   prb <- 1/wgt
   m <- length(bounds)

# Calculate the indicator value matrix

   zm <- matrix(rep(z, m), nrow = n)
   ubound <- matrix(rep(bounds, n), nrow = n, byrow = TRUE)
   lbound <- matrix(rep(c(-1e10, bounds[-m]), n), nrow = n, byrow = TRUE)
   im <- ifelse(zm <= ubound, 1, 0) - ifelse(zm <= lbound, 1, 0)

# Calculate the number of values in each bin

   nbin <- apply(im, 2, sum)

# Calculate the weight matrix

   wm <- matrix(rep(wgt, m), nrow = n)

# Calculate the class proportion estimates

   phat <- apply(im*wm, 2, sum)/tw

# Calculate the weighted residual matrices

   rm <- (im - matrix(rep(phat, n), nrow = n, byrow = TRUE)) * wm

# Calculate the variance-covariance estimate

   if (vartype == "Local" && n < 4) {
      warning("\nThere are less than four response values, the simple random sampling variance-\ncovariance estimator was used.\n")
      vartype <- "SRS"
   } 
   if (vartype == "Local") {
      weight.lst <- localmean.weight(x=x, y=y, prb=prb)
      varest <- localmean.cov(z=rm, weight.lst) / (tw^2)
      df <- localmean.df(weight.lst)
   } else {
      varest <- n * var(rm) / (tw^2)
      df <- n-1
   }

# Return the estimates

   list(nbin=nbin, phat=phat, varest=varest, df=df)

}
