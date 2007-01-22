\name{cdf.test}
\alias{cdf.test}
\title{Cumulative Distribution Function - Inference}
\description{
  This function calculates the Wald, Rao-Scott first order corrected (mean 
  eigenvalue corrected), and Rao-Scott second order corrected (Satterthwaite
  corrected) statistics for categorical data to test for differences between two
  cumulative distribution functions (CDFs).  The functions calculates both 
  standard versions of those three statistics, which are distributed as 
  Chi-squared random variables, plus modified version of the statistics, which 
  are distributed as F random variables.
}
\usage{
cdf.test(sample1, sample2, bounds, vartype="Local")
}
\arguments{
  \item{sample1}{the sample from the first population in the form of a list
    containing the following components:\cr
       z = the response value for each site\cr
       wgt = the final adjusted weight (inverse of the sample inclusion probability) for each site\cr
       x = x-coordinate for location for each site, which may be NULL\cr
       y = y-coordinate for location for each site, which may be NULL}
  \item{sample2}{the sample from the second population in the form of a list
    containing the following components:\cr
       z = the response value for each site\cr
       wgt = the final adjusted weight (inverse of the sample inclusion probability) for each site\cr
       x = x-coordinate for location for each site, which may be NULL\cr
       y = y-coordinate for location for each site, which may be NULL}
  \item{bounds}{upper bounds for calculating the classes for the CDF.}
  \item{vartype}{the choice of variance estimator, where "Local" = local mean
     estimator and "SRS" = SRS estimator.  The default is "Local".}
}
\details{
  The user supplies the set of upper bounds for defining the classes for the
  CDFs.  The Horvitz-Thompson ratio estimator, i.e., the ratio of two
  Horvitz-Thompson estimators, is used to calculate estimates of the class
  proportions for the CDFs.  Variance estimates for the test statistics are
  calculated using either the local mean variance estimator or the simple random
  sampling (SRS) variance estimator.  The choice of variance estimator is
  subject to user control.  The SRS variance estimator uses the
  independent random sample approximation to calculate joint inclusion
  probabilities.  The function checks for compatability of input values and
  removes missing values.}
\value{
  Value is a data frame containing the test statistic, degrees of 
  freedom (two values labeled Degrees of Freedom\_1 and Degrees of  Freedom\_2),
  and p value for the Wald, mean eigenvalue, and Satterthwaite test procedures,
  which includes both Chi-squared distribution and F  distribution versions of 
  the procedures.  For the Chi-squared versions of  the test procedures, Degrees
  of Freedom\_1 contains the relevant value  and Degrees of Freedom\_2 is set to 
  missing (NA).  For the F-based  versions of the test procedures Degrees of 
  Freedom\_1 contains the  numerator degrees of freedom and Degrees of 
  Freedom\_2 contains the  denominator degrees of freedom.
}
\references{
  Kincaid, T.M. (2000). Testing for differences between cumulative distribution
  functions from complex environmental sampling surveys.  In \emph{2000
  Proceeding of the Section on Statistics and the Environment}, American
  Statistical Association, Alexandria, VA.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
sample1 <- list(z=z, wgt=wgt)
sample2 <- list(z=z+2, wgt=wgt)
bounds <- seq(min(sample1$z, sample2$z), max(sample1$z,
   sample2$z), length=4)[-1]
cdf.test(sample1, sample2, bounds, vartype="SRS")

x <- runif(100)
y <- runif(100)
sample1 <- list(z=z, wgt=wgt, x=x, y=y)
sample2 <- list(z=z+rnorm(100), wgt=wgt, x=x, y=y)
bounds <- seq(min(sample1$z, sample2$z), max(sample1$z,
   sample2$z), length=4)[-1]
cdf.test(sample1, sample2, bounds)
}
\keyword{survey}
\keyword{distribution}
