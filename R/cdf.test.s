cdf.test <- function(sample1, sample2, bounds, vartype="Local") {

################################################################################
# Function: cdf.test
# Programmer: Tom Kincaid
# Date: August 23, 2000
# Last Revised: May 5, 2006
# Description:
#   This function calculates the Wald, Rao-Scott first order corrected (mean 
#   eigenvalue corrected), and Rao-Scott second order corrected (Satterthwaite 
#   corrected) statistics for categorical data to test for differences between 
#   two cumulative distribution functions (CDFs).  The functions calculates 
#   both standard versions of those three statistics, which are distributed as
#   Chi-squared random variables, plus modified version of the statistics, which 
#   are distributed as F random variables.  The user supplies the set of upper 
#   bounds for defining the classes for the CDFs.  The Horvitz-Thompson ratio 
#   estimator, i.e., the ratio of two Horvitz-Thompson estimators, is used to 
#   calculate estimates of the class proportions for the CDFs.  Variance 
#   estimates for the test statistics are calculated using either the local 
#   mean variance estimator or the simple random sampling (SRS) variance 
#   estimator.  The choice of variance estimator is subject to user control.  
#   The SRS variance estimator uses the independent random sample approximation
#   to calculate joint inclusion probabilities.  The function checks for 
#   compatability of input values and removes missing values.
#   Input:
#      sample1 = the sample from the first population.
#      sample2 = the sample from the second population.
#         where sample1 and sample2 are lists containing the following items:
#            z = the response value for each site.
#            wgt = the final adjusted weight (inverse of the sample inclusion
#               probability) for each site.
#            x = x-coordinate for location for each site, which may be NULL.
#            y = y-coordinate for location for each site, which may be NULL.
#      bounds = upper bounds for calculating the classes for the CDF.
#      vartype = the choice of variance estimator, where "Local" = local mean
#         estimator and "SRS" = SRS estimator.  The default is "Local".
#   Output:
#      An object in data frame format containing the test statistic, degrees of
#      freedom (two values labeled Degrees of Freedom_1 and Degrees of 
#      Freedom_2), and p value for the Wald, mean eigenvalue, and 
#      Satterthwaite test procedures, which includes both Chi-squared 
#      distribution and F distribution versions of the procedures.  For the 
#      Chi-squared versions of the test procedures, Degrees of Freedom_1 
#      contains the relevant value and Degrees of Freedom_2 is set to missing 
#      (NA).  For the F-based versions of the test procedures Degrees of 
#      Freedom_1 contains the numerator degrees of freedom and Degrees of 
#      Freedom_2 contains the denominator degrees of freedom.
#   Other Functions Required:
#      wnas - remove missing values
#      localmean.weight - calculate the weighting matrix for the local mean
#         variance/covariance estimator
#      localmean.cov - calculate the variance/covariance matrix using the
#         local mean estimator
#      localmean.df - calculate the degrees of freedom of the local mean 
#         variance-covariance estimator
#   Examples:
#      z <- rnorm(100, 10, 1)
#      wgt <- runif(100, 10, 100)
#      sample1 <- list(z=z, wgt=wgt)
#      sample2 <- list(z=z+2, wgt=wgt)
#      bounds <- seq(min(sample1$z, sample2$z), max(sample1$z, sample2$z), 
#         length=4)[-1]
#      cdf.test(sample1, sample2, bounds, vartype="SRS")
#
#      x <- runif(100)
#      y <- runif(100)
#      sample1 <- list(z=z, wgt=wgt, x=x, y=y)
#      sample2 <- list(z=z+rnorm(100), wgt=wgt, x=x, y=y)
#      bounds <- seq(min(sample1$z, sample2$z), max(sample1$z, sample2$z), 
#         length=4)[-1]
#      cdf.test(sample1, sample2, bounds)
################################################################################

# Remove missing values

   if(vartype == "Local") {
      temp <- wnas(list(z=sample1$z, wgt=sample1$wgt, x=sample1$x, y=sample1$y))
      sample1$z <- temp$z
      sample1$wgt <- temp$wgt
      sample1$x <- temp$x
      sample1$y <- temp$y
      temp <- wnas(list(z=sample2$z, wgt=sample2$wgt, x=sample2$x, y=sample2$y))
      sample2$z <- temp$z
      sample2$wgt <- temp$wgt
      sample2$x <- temp$x
      sample2$y <- temp$y
   } else {
      temp <- wnas(list(z=sample1$z, wgt=sample1$wgt))
      sample1$z <- temp$z
      sample1$wgt <- temp$wgt
      temp <- wnas(list(z=sample2$z, wgt=sample2$wgt))
      sample2$z <- temp$z
      sample2$wgt <- temp$wgt
   }

# Check for compatability of input values

   if(length(sample1$z) != length(sample1$wgt))
      stop("\n\nNumber of response values must equal the number of weights for sample 1.")
   if(length(sample2$z) != length(sample2$wgt))
      stop("\n\nNumber of response values must equal the number of weights for sample 2.")
   if(min(sample1$wgt) <= 0)
      stop("\n\nWeights must be positive for sample 1.")
   if(min(sample2$wgt) <= 0)
      stop("\n\nWeights must be positive for sample 2.")
   if(vartype == "Local") {
      if (length(sample1$x) == 0 || length(sample1$y) == 0)
         stop("\n\nx-coordinate and y-coordinate values for sample 1 are required for the local mean variance estimator.")
      else if (length(sample1$z) != length(sample1$x) || length(sample1$z) != length(sample1$y))
         stop("\n\nNumber of response values must equal the number of x-coordinate and y-coordinate values for sample 1 for the local mean variance estimator.")
   }
   if(vartype == "Local") {
      if (length(sample2$x) == 0 || length(sample2$y) == 0)
         stop("\n\nx-coordinate and y-coordinate values for sample 2 are required for the local mean variance estimator.")
      else if (length(sample2$z) != length(sample2$x) || length(sample2$z) != length(sample2$y))
         stop("\n\nNumber of response values must equal the number of x-coordinate and y-coordinate values for sample 2 for the local mean variance estimator.")
   }

# Create the data frame for estimates

   rslt <- data.frame(array(0, c(6, 4)))
   dimnames(rslt) <- list(c("Wald  ", "Wald_F  ", "Mean Eigenvalue  ", "Mean Eigenvalue_F  ", "Satterthwaite  ", "Satterthwaite_F  "), c("Test Statistic", "Degrees of Freedom_1", "Degrees of Freedom_2", "p Value"))

# Assign the number of bins

   n.bin <- length(bounds)
   m <- n.bin - 1

# Assign length of the response vectors

   n1 <- length(sample1$z)
   n2 <- length(sample2$z)

# Calculate estimates for the first sample

   est1 <- cdfvar.test(sample1, bounds, vartype)
   sam1.nbin <- est1$nbin
   sam1.phat <- as.array(est1$phat)
   sam1.phatvar <- as.matrix(est1$varest)
   sam1.df <- est1$df

# Calculate estimates for the second sample

   est2 <- cdfvar.test(sample2, bounds, vartype)
   sam2.nbin <- est2$nbin
   sam2.phat <- as.array(est2$phat)
   sam2.phatvar <- as.matrix(est2$varest)
   sam2.df <- est2$df

# Determine whether the combined number of values in any bin is less than five
# and print a warning message, as necessary

   if(any((sam1.nbin + sam2.nbin) < 5))
      warning("\nThe combined number of values in at least one class is less than five. The user\n should consider using a smaller number of classes.\n\n")

# Calculate the Wald chi square statistic

   difr <- sam1.phat[-n.bin] - sam2.phat[-n.bin]
   tqr <- qr(sam1.phatvar[-n.bin,-n.bin] + sam2.phatvar[-n.bin,-n.bin])
   if (tqr$rank < m) {
      rslt[1, 1] <- NA
      rslt[1, 2] <- NA
      rslt[1, 3] <- NA
      rslt[1, 4] <- NA
   } else {
      rslt[1, 1] <- difr %*% solve(tqr) %*% difr
      rslt[1, 2] <- m
      rslt[1, 3] <- NA
      rslt[1, 4] <- 1 - pchisq(rslt[1, 1], rslt[1, 2])
   }

# Calculate the F-based version of the Wald statistic
   f.df <- sam1.df + sam2.df
   if (tqr$rank < m) {
      rslt[2, 1] <- NA
      rslt[2, 2] <- NA
      rslt[2, 3] <- NA
      rslt[2, 4] <- NA
   } else {
      rslt[2, 1] <- ((f.df - m + 1)/(f.df*m))*rslt[1, 1]
      rslt[2, 2] <- m
      rslt[2, 3] <- f.df - m + 1
      rslt[2, 4] <- 1 - pf(rslt[2, 1], rslt[2, 2], rslt[2, 3])
   }

# Calculate the mean eigenvalue-corrected chi square statistic

   phatmean <- ((n1 * sam1.phat) + (n2 * sam2.phat)) / (n1 + n2)
   difr1 <- sam1.phat[-n.bin] - phatmean[-n.bin]
   difr2 <- sam2.phat[-n.bin] - phatmean[-n.bin]
   tqr <- qr(diag(phatmean[-n.bin]) - outer(phatmean[-n.bin], phatmean[-n.bin]))
   if (tqr$rank < m) {
      rslt[3, 1] <- NA
      rslt[3, 2] <- NA
      rslt[3, 3] <- NA
      rslt[3, 4] <- NA
   } else {
      rmatinv <- solve(tqr)
      chisqtmp <- (n1 * difr1 %*% rmatinv %*% difr1) + (n2 * difr2 %*% rmatinv %*% difr2)
      sam1.amat <- n1 * (rmatinv %*% sam1.phatvar[-n.bin,-n.bin])
      sam2.amat <- n2 * (rmatinv %*% sam2.phatvar[-n.bin,-n.bin])
      eigmat <- eigen(((n2*sam1.amat) + (n1*sam2.amat)) / (n1 + n2))
      eigmean <- mean(eigmat$values)
      rslt[3, 1] <- chisqtmp / eigmean
      rslt[3, 2] <- m
      rslt[3, 3] <- NA
      rslt[3, 4] <- 1 - pchisq(rslt[3, 1], rslt[3, 2])
   }

# Calculate the F-based version of the mean eigenvalue-corrected statistic

   if (tqr$rank < m) {
      rslt[4, 1] <- NA
      rslt[4, 2] <- NA
      rslt[4, 3] <- NA
      rslt[4, 4] <- NA
   } else {
      rslt[4, 1] <- rslt[3, 1]/m
      rslt[4, 2] <- m
      rslt[4, 3] <- f.df*m
      rslt[4, 4] <- 1 - pf(rslt[4, 1], rslt[4, 2], rslt[4, 3])
   }
# Calculate the Satterthwaite-corrected chi square statistic

   if (tqr$rank < m) {
      rslt[5, 1] <- NA
      rslt[5, 2] <- NA
      rslt[5, 3] <- NA
      rslt[5, 4] <- NA
   } else {
      cvsquare <- var(eigmat$values) / (eigmean ^ 2)
      rslt[5, 1] <- rslt[3, 1] / (1 + cvsquare)
      rslt[5, 2] <- m / (1 + cvsquare)
      rslt[5, 3] <- NA
      rslt[5, 4] <- 1 - pchisq(rslt[5, 1], rslt[5, 2])
   }

# Calculate the F-based version of the Satterthwaite-corrected statistic

   if (tqr$rank < m) {
      rslt[6, 1] <- NA
      rslt[6, 2] <- NA
      rslt[6, 3] <- NA
      rslt[6, 4] <- NA
   } else {
      rslt[6, 1] <- rslt[4, 1]
      rslt[6, 2] <- rslt[5, 2]
      rslt[6, 3] <- f.df*rslt[5, 2]
      rslt[6, 4] <- 1 - pf(rslt[6, 1], rslt[6, 2], rslt[6, 3])
   }

# Return the data frame

   rslt
}
