\name{ash1.wgt}
\alias{ash1.wgt}
\title{Compute the Average Shifted Histogram for Weighted Data}
\description{
  This function computes the average shifted histogram (ASH) for weighted data.
}
\usage{
ash1.wgt(x, wgt=rep(1,length(x)), m=5, nbin=50, ab=NULL, support="Continuous")
}
\arguments{
  \item{x}{vector of data to be used to estimate density. NAs are allowed.}
  \item{wgt}{vector of weights for each observation from a probability sample.
    The default is equal weights (equal probability).}
  \item{m}{the number of empty bins to add to the ends when the range is not
    completely specified.  The default is 5.}
  \item{nbin}{the number of bins for density estimation.  the default is 50.}
  \item{ab}{optional range for support associated with the density.  Both values
    may be equal to NA.  If equal to NA, then corresponding limit will be based
    on nicerange().  the default is NULL.}
  \item{support}{the type of support.  If equal to "Continuous", then data are
    from a continuous distribution.  If equal to "Ordinal", then data are from a
    discrete distribution defined for integers only.  The default is
    "Continuous".}
}
\value{
  A vector containing the ASH density estimate.
}
\author{Tony Olsen \email{Olsen.Tony@epa.gov}}
\examples{
x <- rnorm(100, 10, sqrt(10))
wgt <- runif(100, 10, 100)
rslt <- ash1.wgt(x, wgt)
plot(rslt)
}
\keyword{survey}
\keyword{misc}
