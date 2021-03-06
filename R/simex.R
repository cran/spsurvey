################################################################################
# Function: simex
# Programmer: Tom Kincaid
# Date: December 2, 2002
# Last Revised: February 18, 2004
#
#' Internal Function: Extrapolation for Simulation-Extrapolation Function
#'
#' This function executes the extrapolation step of the simulation extrapolation
#' deconvolution method (Stefanski and Bay, 1996).  The function can accomodate
#' single-stage and two-stage samples.
#'
#' @param z Vector of the response value for each site.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @param sigma Measurement error variance.
#'
#' @param var.sigma Variance of the estimated measurement error variance.
#'
#' @param cluster.ind  Logical value that indicates whether the survey design
#'   utilizes two stages.
#'
#' @param cluster Vector of the stage one sampling unit (primary sampling unit
#'  or cluster) code for each site.
#'
#' @return Output is a list containing the following matrices:
#'   \describe{
#'     \item{g}{values of the function g(.) evaluated at val for each value of z}
#'     \item{dg}{values of the derivative of the function g(.)}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

simex <- function(z, val, sigma, var.sigma, cluster.ind, cluster) {

# Assign additional required values

   nlambda <- 10
   minlambda <- 0.05
   maxlambda <- 2
   m <- length(val)
   if(cluster.ind) {
      cluster <- factor(cluster)
      ncluster <- length(levels(cluster))
      z.lst <- split(z, cluster)
   }

# Assign values for the vector lambda

   lambda <- seq(minlambda, maxlambda, (maxlambda - minlambda)/(nlambda - 1))

# Assign matrices required for the calculations

   d <- matrix(c(1, -1, 1), nrow=1)
   D <- matrix(c(rep(1, nlambda), lambda, lambda^2), nrow=nlambda)

# Create objects for the function g(.) and the derivative of the function g(.)

   if(cluster.ind) {
      g <- vector("list", ncluster)
      dg <- vector("list", ncluster)
      for (i in 1:ncluster) {
         n <- length(z.lst[[i]])
         g[[i]] <- array(0, c(n, m))
         dg[[i]] <- array(0, c(n, m))
      }
   } else {
      n <- length(z)
      g <- array(0, c(n, m))
      dg <- array(0, c(n, m))
   }

# Calculate values of the function g(.) and the derivative of the function g(.)

   V <- d %*% solve(t(D) %*% D) %*% t(D)
   if(cluster.ind) {
      for(i in 1:ncluster) {
         for(j in 1:m) {
            g[[i]][,j] <- V %*% matrix(pnorm(sqrt((sigma*lambda)^(-1)) %o% (val[j] - z.lst[[i]])), nrow=nlambda)
            if(!is.null(var.sigma)) {
               d1 <- dnorm(sqrt((sigma*lambda)^(-1)) %o% (val[j] - z.lst[[i]]))
               d2 <- -(((sigma^(3/2) * sqrt(lambda))^(-1)) %o% (val[j] - z.lst[[i]]))/2
               dg[[i]][,j] <- V %*% (d1*d2)
            }
         }
      }
   } else {
      for(j in 1:m) {
         g[,j] <- V %*% matrix(pnorm(sqrt((sigma*lambda)^(-1)) %o% (val[j] - z)), nrow=nlambda)
         if(!is.null(var.sigma)) {
            d1 <- dnorm(sqrt((sigma*lambda)^(-1)) %o% (val[j] - z))
            d2 <- -(((sigma^(3/2) * sqrt(lambda))^(-1)) %o% (val[j] - z))/2
            dg[,j] <- V %*% (d1*d2)
         }
      }
   }

# Return the values

   list(g=g, dg=dg)
}
