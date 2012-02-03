cdfvar.test <- function(z, wgt, x, y, bounds, phat, stratum.ind, stratum.level,
   cluster.ind, cluster, wgt1, x1, y1, popsize, pcfactor.ind, pcfsize,
   N.cluster, stage1size, support, swgt.ind, swgt, swgt1, vartype, warn.ind,
   warn.df, warn.vec) {

################################################################################
# Function: cdfvar.test
# Purpose: Calculate variances and covariances of the class proportion estimates
#          for testing differences between cumulative distribution functions
#          (CDFs)
# Programmer: Tom Kincaid
# Date: November 2, 2000
# Last Revised: June 17, 2011
# Description:
#   This function calculates estimates of the variance-covariance matrix
#   of the population proportions in a set of intervals (classes).  The set of
#   values defining upper bounds for the classes is supplied to the function.
#   Either the simple random sampling (SRS) variance estimator or the local mean
#   variance estimator is calculated, which is subject to user control.  The
#   simple random sampling variance estimator uses the independent random sample
#   approximation to calculate joint inclusion probabilities.  The function can 
#   accomodate single-stage and two-stage samples.  Finite population and
#   continuous population correction factors can be utilized in variance
#   estimation.
# Arguments:
#   z = the response value for each site.
#   wgt = the final adjusted weight (inverse of the sample inclusion
#     probability) for each site, which is either the weight for a single-stage
#     sample or the stage two weight for a two-stage sample.
#   x = x-coordinate for location for each site, which is either the x-
#     coordinate for a single-stage sample or the stage two x-coordinate for a
#     two-stage sample.
#   y = y-coordinate for location for each site, which is either the y-
#     coordinate for a single-stage sample or the stage two y-coordinate for a
#     two-stage sample.
#   bounds = upper bounds for calculating classes for the CDF.
#   phat = the class proportions estimate.
#   stratum.ind = a logical value that indicates whether the sample is
#     stratified, where TRUE = a stratified sample and FALSE = not a stratified
#     sample.
#   stratum.level = the stratum level.
#   cluster.ind = a logical value that indicates whether the sample is a two-
#     stage sample, where TRUE = a two-stage sample and FALSE = not a two-stage
#     sample.
#   cluster = the stage one sampling unit (primary sampling unit or cluster)
#     code for each site.
#   wgt1 = the final adjusted stage one weight for each site.
#   x1 = the stage one x-coordinate for location for each site.
#   y1 = the stage one y-coordinate for location for each site.
#   popsize = known size of the resource, which is used to perform ratio
#     adjustment to estimators expressed using measurement units for the
#     resource.  For a finite resource, this argument is either the total number
#     of sampling units or the known sum of size-weights.  For an extensive
#     resource, this argument is the measure of the resource, i.e., either known
#     total length for a linear resource or known total area for an areal
#     resource.  For a stratified sample this variable must be a vector
#     containing a value for each stratum and must have the names attribute set
#     to identify the stratum codes.
#   pcfactor.ind = a logical value that indicates whether the population
#     correction factor is used during variance estimation, where TRUE = use the
#     population correction factor and FALSE = do not use the factor.
#   pcfsize = size of the resource, which is required for calculation of finite
#     and continuous population correction factors for a single-stage sample.
#     For a stratified sample this argument must be a vector containing a value
#     for each stratum and must have the names attribute set to identify the
#     stratum codes.
#   N.cluster = the number of stage one sampling units in the resource, which is
#     required for calculation of finite and continuous population correction
#     factors for a two-stage sample.  For a stratified sample this variable
#     must be a vector containing a value for each stratum and must have the
#     names attribute set to identify the stratum codes.
#   stage1size = size of the stage one sampling units of a two-stage sample,
#     which is required for calculation of finite and continuous population
#     correction factors for a two-stage sample and must have the names
#     attribute set to identify the stage one sampling unit codes.  For a
#     stratified sample, the names attribute must be set to identify both
#     stratum codes and stage one sampling unit codes using a convention where
#     the two codes are separated by the & symbol, e.g., "Stratum 1&Cluster 1".
#   support = the support value for each site - the value one (1) for a site
#     from a finite resource or the measure of the sampling unit associated with
#     a site from a continuous resource, which is required for calculation of
#     finite and continuous population correction factors.
#   swgt.ind = a logical value that indicates whether the sample includes
#     size-weights, where TRUE = the sample includes size-weights and FALSE =
#     the sample does not include size-weights.
#   swgt = the size-weight for each site, which is the stage two size-weight for
#     a two-stage sample.
#   swgt1 = the stage one size-weight for each site.
#   vartype = the choice of variance estimator, where "Local" = local mean
#     estimator and "SRS" = SRS estimator.
#   warn.ind = a logical value that indicates whether warning messages were
#     generated, where TRUE = warning messages were generated and FALSE =
#     warning messages were not generated.
#   warn.df = a data frame for storing warning messages.
#   warn.vec = a vector that contains names of the population type, the
#     subpopulation, and an indicator.
# Output:
#   An object in list format composed of a vector named nbin, which contains the
#   number of response values in each class, a vector named varest, which
#   contains variance estimates, a numeric value named df, which contain degrees
#   of freedom of the variance estimates, a logical variable named warn,ind,
#   which is the indicator for warning messges, and a data frame named warn.df,
#   which contains warning messages.
# Other Functions Required:
#   localmean.weight - calculate the weighting matrix for the local mean
#     variance/covariance estimator
#   localmean.cov - calculate the variance/covariance matrix using the local
#     mean estimator
#   localmean.df - calculate the degrees of freedom of the local mean 
#     variance-covariance estimator
################################################################################

# Assign the function name

   fname <- "cdfvar.test"

# Branch to handle two-stage and single-stage samples

   if(cluster.ind) {

# Begin the section for a two-stage sample

# Calculate additional required values

      m <- length(bounds)
      nbin <- numeric(m)
      df <- 0
      cluster <- factor(cluster)
      cluster.levels <- levels(cluster)
      ncluster <- length(cluster.levels)
      z.lst <- split(z, cluster)
      if(swgt.ind) {
         wgt <- wgt*swgt
         wgt1 <- wgt1*swgt1
      }
      wgt2.lst <- split(wgt, cluster)
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
      tw2 <- (sum(wgt1*wgt))^2
      if(vartype == "Local") {
         x2.lst <- split(x, cluster)
         y2.lst <- split(y, cluster)
         x1.u <- as.vector(tapply(x1, cluster, unique))
         y1.u <- as.vector(tapply(y1, cluster, unique))
      }
      if(pcfactor.ind) {
         support.lst <- split(support, cluster)
      } else {
         support.lst <- NULL
      }
      var.ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit residuals 
# and the variance/covariance of those totals for each stage one sampling unit

      total2est <- matrix(0, ncluster, m)
      var2est <- matrix(0, ncluster, m^2)
      for(i in 1:ncluster) {

# Calculate the weighted residuals matrix

         n <- length(z.lst[[i]])
         im <- ifelse(matrix(rep(z.lst[[i]], m), nrow=n) <= matrix(rep(bounds,
            n), nrow=n, byrow=TRUE), 1, 0) - ifelse(matrix(rep(z.lst[[i]], m),
            nrow=n) <= matrix(rep(c(-1e10, bounds[-m]), n), nrow=n, byrow=TRUE),
            1, 0)
         rm <- (im - matrix(rep(phat, n), nrow=n, byrow=TRUE)) *
            matrix(rep(wgt2.lst[[i]], m), nrow=n)

# Calculate the number of values in each class for the stage one sampling unit

         nbin <- nbin + apply(im, 2, sum)

# Calculate total estimates for the stage one sampling unit

         total2est[i,] <- apply(rm, 2, sum)

# Adjust the variance/covariance estimator for small sample size

         SRSind <- FALSE
         if(vartype == "Local" && n < 4) {
            warn.ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            if(stratum.ind) {
               warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], "\nin stratum ", stratum.level, ", the simple random sampling variance estimator \nwas used to calculate variance of the estimate.\n", sep="")
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=warn.vec[1], subpop=warn.vec[2],
                  indicator=warn.vec[3], stratum=I(stratum.level),
                  warning=I(warn), action=I(act)))
            } else {
               warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep="")
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=warn.vec[1], subpop=warn.vec[2],
                  indicator=warn.vec[3], stratum=NA, warning=I(warn),
                  action=I(act)))
            }
            vartype <- "SRS"
            SRSind <- TRUE
         }

# Calculate the population correction factor for the stage two sample

         pcfactor <- ifelse(pcfactor.ind, (stage1size[i] - sum(support.lst[[i]]))/stage1size[i], 1)

# Calculate variance/covariance estimates for the stage one sampling unit

         if(var.ind[i]) {
            if(vartype == "Local") {
               weight.lst <- localmean.weight(x2.lst[[i]], y2.lst[[i]], 1/wgt2.lst[[i]])
               var2est[i,] <- as.vector(pcfactor*localmean.cov(rm, weight.lst))
               df <- df + localmean.df(weight.lst)
            } else {
               var2est[i,] <- as.vector(pcfactor*n*var(rm))
               df <- df + (n-1)
               if(SRSind)
                  vartype <- "Local"
            }
         }
      }

# Assign the mean variance to stage one sampling units with a single stage two
# sampling unit
      for(j in 1:m) {
         ind <- var2est[,j] == 0
         if(sum(ind) > 0) {
            var.mean <- mean(var2est[!ind,j])
            var2est[ind,j] <- var.mean
         }
      }

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && ncluster < 4) {
         warn.ind <- TRUE
         act <- "The simple random sampling variance estimator was used.\n"
         if(stratum.ind) {
            warn <- paste("There are less than four stage one sampling units in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
               action=I(act)))
         } else {
            warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the estimate.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         }
         vartype <- "SRS"
      }

# Calculate the population correction factor for the stage one sample

      pcfactor <- ifelse(pcfactor.ind, (N.cluster - ncluster)/N.cluster, 1)

# Calculate the variance estimate

      if(vartype == "Local") {
         weight.lst <- localmean.weight(x1.u, y1.u, 1/wgt1.u)
         varest <- (pcfactor*localmean.cov(total2est * matrix(rep(wgt1.u, m),
            nrow = ncluster), weight.lst) + matrix(apply(var2est *
            matrix(rep(wgt1.u, m^2), nrow = ncluster), 2, sum), nrow=m)) / tw2
      } else {
         varest <- (pcfactor*ncluster*var(total2est * matrix(rep(wgt1.u, m),
            nrow=ncluster)) + matrix(apply(var2est * matrix(rep(wgt1.u, m^2),
            nrow=ncluster), 2, sum), nrow=m))/ tw2
      }

# End of section for a two-stage sample

   } else {

# Begin the section for a single-stage sample

# Calculate additional required values

      n <- length(z)
      m <- length(bounds)
      if(swgt.ind) 
         wgt <- wgt*swgt
      tw2 <- (sum(wgt))^2

# Calculate the weighted residuals matrix

      im <- ifelse(matrix(rep(z, m), nrow=n) <= matrix(rep(bounds, n), nrow=n,
         byrow=TRUE), 1, 0) - ifelse(matrix(rep(z, m), nrow=n) <= matrix(rep(
         c(-1e10, bounds[-m]), n), nrow=n, byrow=TRUE), 1, 0)
      rm <- (im - matrix(rep(phat, n), nrow=n, byrow=TRUE)) * matrix(rep(wgt,
         m), nrow=n)

# Calculate the number of values in each class

         nbin <- apply(im, 2, sum)

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && n < 4) {
         warn.ind <- TRUE
         act <- "The simple random sampling variance estimator was used.\n"
         if(stratum.ind) {
            warn <- paste("There are less than four response values in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
               action=I(act)))
         } else {
            warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the estimate.\n"
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
          }
         vartype <- "SRS"
      }

# Calculate the population correction factor

      pcfactor <- ifelse(pcfactor.ind, (pcfsize - sum(support))/pcfsize, 1)

# Calculate the variance estimate

       if(vartype == "Local") {
         weight.lst <- localmean.weight(x, y, 1/wgt)
         varest <- pcfactor*localmean.cov(rm, weight.lst) / tw2
         df <- localmean.df(weight.lst)
      } else {
         varest <- pcfactor*n*var(rm) / tw2
         df <- n-1
      }

# End section for a single-stage sample

   }

# Return the number of values in each class, the variance estimate, degrees of
# freedom for the variance estimate, the warning message indicator, and the
# warn.df data frame

   list(nbin=nbin, varest=varest, df=df, warn.ind=warn.ind, warn.df=warn.df)
}
