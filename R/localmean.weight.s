localmean.weight <- function(x, y, prb, nbh=4) {

################################################################################
# Function: localmean.weight
# Programmers: Don Stevens and Tom Kincaid
# Date: September 5, 2001
# Last Revised: May 5, 2006
# Description:
#   This function calculates the index values of neighboring points and
#   associated weights required by the local mean variance estimator.
#   Input:
#      x = x-coordinates for location of the sample points.
#      y = y-coordinates for location of the sample points.
#      prb = inclusion probabilities for the sample points.
#      nbh = number of neighboring points to use in the calculations.
#   Output:
#      An object in list format containing two elements: a matrix named ij 
#      composed of the index values of neighboring points and a vector named gwt
#      composed of weights.
################################################################################

   n <- length(x)

# Calculate indices of nearest neighbors

   idx <- apply(as.matrix(dist(cbind(x, y), diag = TRUE, upper = TRUE)), 2, order)[1:nbh,]

# Make neighbors symmetric

   jdx <- rep(1:n, rep(nbh, n))
   kdx <- unique(c((jdx - 1) * n + idx, (idx - 1) * n + jdx)) - 1
   ij <- cbind((kdx) %/% n + 1, (kdx) %% n + 1)
   ij <- ij[order(ij[, 1]),  ] 

# Apply linear taper to the  inverse probability weights

   gct <- tabulate(ij[, 1])
   gwt <- numeric(0)
   for(i in 1:n)
      gwt <- c(gwt, 1 - (1:gct[i] - 1)/(gct[i]))
   gwt <- gwt/prb[ij[, 2]]

# Normalize to make true average

   smwt <- sapply(split(gwt, ij[, 1]), sum)
   gwt <- gwt/smwt[ij[, 1]]
   smwt <- sapply(split(gwt, ij[, 2]), sum)

# Make weights doubly stochastic

   hij <- matrix(0, n, n)
   hij[ij] <- 0.5
   a22 <- ginv(diag(gct/2) - hij %*% diag(2/gct) %*% hij)
   a21 <-  - diag(2/gct) %*% hij %*% a22
   lm <- a21 %*% (1 - smwt)
   gm <- a22 %*% (1 - smwt)
   gwt <- (lm[ij[, 1]] + gm[ij[, 2]])/2 + gwt

# Return the results

   list(ij=ij, gwt=gwt)
}
