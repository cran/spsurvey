\name{adjwgt}
\alias{adjwgt}
\title{Adjust Initial Survey Design Weights}
\description{
  This function adjusts initial survey design weights when implementation results
  in use of oversample sites or when it is desired to have final weights sum to a
  known frame size.  Adjusted weights are equal to initial weight times the frame
  size divided by the sum of the initial weights.  The adjustment is done 
  separately for each category specified in argument wtcat.
}
\usage{
adjwgt(sites, wgt, wtcat, framesize)
}
\arguments{
  \item{sites}{the logical value for each site, where TRUE = include the site
    and FALSE = do not include the site.}
  \item{wgt}{the initial weight (inverse of the sample inclusion probability)
    for each site.}
  \item{wtcat}{the weight adjustment category name for each site.}
  \item{framesize}{the known size of the frame for each category name in 
    wtcat, which must have the names attribute set to match the category 
    names used in wtcat.}
}
\value{
  A vector of adjusted weights, where the adjusted weight is set to zero for 
  sites that have the logical value in the sites argument set to FALSE.
}
\author{Tony Olsen \email{Olsen.Tony@epa.gov}}
\examples{
sites <- as.logical(rep(rep(c("TRUE","FALSE"), c(9,1)), 5))
wgt <- runif(50, 10, 100)
wtcat <- rep(c("A","B"), c(30, 20))
framesize <- c(1650, 1100)
names(framesize) <- c("A","B")
adjwgt(sites, wgt, wtcat, framesize)
}
\keyword{survey}
\keyword{misc}
