\name{cdf.est}
\alias{cdf.est}
\title{Cumulative Distribution Function - Estimation}
\description{
  This function calculates an estimate of the cumulative distribution function
  (CDF) for the proportion (expressed as percent) and the total of a response
  variable, where the response variable may be defined for either a finite
  or an extensive resource.  Optionally, for a finite resource, the
  size-weighted CDF can be calculated.  In addition the standard error of the
  estimated CDF and confidence bounds are calculated.
}
\usage{
cdf.est(z, wgt, x=NULL, y=NULL, stratum=NULL, cluster=NULL, wgt1=NULL, x1=NULL,
   y1=NULL, popsize=NULL, popcorrect=FALSE, pcfsize=NULL, N.cluster=NULL,
   stage1size=NULL, support=NULL, sizeweight=FALSE, swgt=NULL, swgt1=NULL,
   vartype="Local", conf=95, cdfval=NULL, pctval=c(5,10,25,50,75,90,95),
   check.ind=TRUE, warn.ind=NULL, warn.df=NULL, warn.vec=NULL)
}
\arguments{
  \item{z}{the response value for each site.}
  \item{wgt}{the final adjusted weight (inverse of the sample inclusion
    probability) for each site, which is either the weight for a 
    single-stage sample or the stage two weight for a two-stage sample.}
  \item{x}{x-coordinate for location for each site, which is either the
    x-coordinate for a single-stage sample or the stage two 
    x-coordinate for a two-stage sample.  The default is NULL.}
  \item{y}{y-coordinate for location for each site, which is either the
    y-coordinate for a single-stage sample or the stage two 
    y-coordinate for a two-stage sample.  The default is NULL.}
  \item{stratum}{the stratum for each site.  The default is NULL.}
  \item{cluster}{the stage one sampling unit (primary sampling unit or cluster) 
    code for each site.  The default is NULL.}
  \item{wgt1}{the final adjusted stage one weight for each site.  The default
    is NULL.}
  \item{x1}{the stage one x-coordinate for location for each site.  The default
    is NULL.}
  \item{y1}{the stage one y-coordinate for location for each site.  The default
    is NULL.}
  \item{popsize}{known size of the resource, which is used to perform ratio
    adjustment to estimators expressed using measurement units for the
    resource and to calculate strata proportions for calculating estimates for
    a stratified sample.  For a finite resource, this argument is either the
    total number of sampling units or the known sum of size-weights.  For an
    extensive resource, this argument is the measure of the resource, i.e.,
    either known total length for a linear resource or known total area for an
    areal resource.  For a stratified sample this variable must be a vector
    containing a value for each stratum and must have the names attribute set
    to identify the stratum codes.  The default is NULL.}
  \item{popcorrect}{a logical value that indicates whether finite or continuous 
    population correction factors should be employed during variance 
    estimation, where TRUE = use the correction factors and FALSE = do not 
    use the correction factors.  The default is FALSE.}
  \item{pcfsize}{size of the resource, which is required for calculation of
    finite and continuous population correction factors for a single-stage
    sample.  For a stratified sample this argument must be a vector containing a
    value for each stratum and must have the names attribute set to identify the
    stratum codes.  The default is NULLL.}
  \item{N.cluster}{the number of stage one sampling units in the resource, which 
    is required for calculation of finite and continuous population 
    correction factors for a two-stage sample.  For a stratified sample 
    this variable must be a vector containing a value for each stratum and
    must have the names attribute set to identify the stratum codes.  The
    default is NULL.}
  \item{stage1size}{size of the stage one sampling units of a two-stage sample,
    which is required for calculation of finite and continuous population
    correction factors for a two-stage sample and must have the names
    attribute set to identify the stage one sampling unit codes.  For a
    stratified sample, the names attribute must be set to identify both
    stratum codes and stage one sampling unit codes using a convention where
    the two codes are separated by the & symbol, e.g., "Stratum 1&Cluster 1".
    The default is NULL.}
  \item{support}{the support value for each site - the value one (1) for a site
    from a finite resource or the measure of the sampling unit associated with
    a site from an extensive resource, which is required for calculation of
    finite and continuous population correction factors.  The default is NULL.}
  \item{sizeweight}{a logical value that indicates whether size-weights should 
    be used in the analysis, where TRUE = use the size-weights and FALSE = 
    do not use the size-weights.  The default is FALSE.}
  \item{swgt}{the size-weight for each site, which is the stage two size-weight 
    for a two-stage sample.  The default is NULL.}
  \item{swgt1}{the stage one size-weight for each site.  The default is NULL.}
  \item{vartype}{the choice of variance estimator, where "Local" = local mean
    estimator and "SRS" = SRS estimator.  The default is "Local".}
  \item{conf}{the confidence level.  The default is 95\%.}
  \item{cdfval}{the set of values at which the CDF is estimated.  If a set of
    values is not provided, then the sorted set of unique values of the
    response variable is used.  The default is NULL.}
  \item{pctval}{the set of values at which percentiles are estimated.  The
    default set is: \{5, 10, 25, 50, 75, 90, 95\}.}
  \item{check.ind}{a logical value that indicates whether compatability
    checking of the input values is conducted, where TRUE = conduct 
    compatibility checking and FALSE = do not conduct compatibility 
    checking.  The default is TRUE.}
  \item{warn.ind}{a logical value that indicates whether warning messages were
    generated, where TRUE = warning messages were generated and FALSE = warning
    messages were not generated.  The default is NULL.}
  \item{warn.df}{a data frame for storing warning messages.  The default is
    NULL.}
  \item{warn.vec}{a vector that contains names of the population type, the
    subpopulation, and an indicator.  The default is NULL.}
}
\details{
  This function calculates an estimate of the cumulative distribution function
  (CDF) for the proportion (expressed as percent) and the total of a
  response  variable, where the response variable may be defined for either a
  finite or an extensive resource.  Optionally, for a finite resource, the
  size-weighted CDF can be calculated.  In addition the standard error of
  the estimated CDF and confidence bounds are calculated.  The user
  can supply the set of values at which the CDF is estimated.  For the 
  CDF of a proportion, the Horvitz-Thompson ratio estimator, i.e., the
  ratio of two Horvitz-Thompson estimators, is used to calculate the CDF
  estimate.  For the CDF of a total, the user can supply the known size of
  the resource or the known sum of the size-weights of the resource, as
  appropriate.  For the CDF of a total when either the size of the
  resource or the sum of the size-weights of the resource is provided, the
  classic ratio estimator is used to calculate the CDF estimate, where
  that estimator is the product of the known value and the Horvitz-Thompson ratio
  estimator.   For the CDF of a total when neither the size of the
  resource nor the sum of the size-weights of the resource is provided, the
  Horvitz-Thompson estimator is used to calculate the CDF estimate.
  Variance estimates for the estimated CDF are calculated using either the
  local mean variance estimator or the simple random sampling (SRS) 
  variance estimator.  The choice of variance estimator is subject to user 
  control. The local mean variance estimator requires the x-coordinate and
  the y-coordinate of each site.  The SRS variance estimator uses
  the independent random sample approximation to calculate joint inclusion
  probabilities.  Confidence bounds are calculated using a Normal
  distribution multiplier.  In addition the function uses the estimated 
  CDF to calculate percentile estimates.  Estimated confidence bounds for
  the percentile estimates are calculated.  The user can supply the set of values
  for which percentiles estimates are desired.  Optionally, the user can use the
  default set of percentiles.  The function can accommodate a stratified sample.
  For a stratified sample, separate estimates and standard errors are calculated
  for each stratum, which are used to produce estimates and standard errors for
  all strata combined.  Strata that contain a single value are removed.  For a
  stratified sample, when either the size of the resource or the sum of the size-
  weights of the resource is provided for each stratum, those values are used as 
  stratum weights for calculating the estimates and standard  errors for all 
  strata combined.  For a stratified sample when neither the size of the resource
  nor the sum of the size-weights of the resource is provided for each stratum, 
  estimated values are used as stratum weights for calculating the estimates and 
  standard errors for all strata combined.   The function can accommodate 
  single-stage and two-stage samples for both stratified and unstratified 
  sampling designs.  Finite population and continuous population correction 
  factors can be utilized in variance estimation.  The function checks for 
  compatibility of input values and removes missing values.
}
\section{Value}{
  If the function was called by the cont.analysis function, then value is a list
  containing the following components:
  \itemize{
    \item \code{Results} - a list composed of two objects:
    \itemize{
      \item \code{CDF} - a data frame that contains CDF estimates
      \item \code{Pct} - a data frame that contains percentile estimates
    }
    \item \code{warn.ind} - a logical value indicating whether warning messages
      were generated
    \item \code{warn.df} - a data frame containing warning messages
  }
  If the function was called directly, then value is a list containing the
  following components:
  \itemize{
    \item \code{CDF} - a data frame that contains CDF estimates
    \item \code{Pct} - a data frame that contains percentile estimates
  }
}
\references{
  Diaz-Ramos, S., D.L. Stevens, Jr., and A.R. Olsen. (1996).  \emph{EMAP
  Statistical Methods Manual.} EPA/620/R-96/XXX.  Corvallis, OR: U.S.
  Environmental Protection Agency, Office of Research and Development, National
  Health Effects and Environmental Research Laboratory, Western Ecology
  Division.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
cdfval <- seq(min(z), max(z), length=20)
cdf.est(z, wgt, vartype="SRS", cdfval=cdfval)

x <- runif(100)
y <- runif(100)
cdf.est(z, wgt, x, y, cdfval=cdfval)
}
\keyword{survey}
\keyword{distribution}
