\name{category.est}
\alias{category.est}
\title{Category Proportion and Size Estimates}
\description{
  This function estimates proportion (expressed as percent) and size of a
  resource in each of a set of categories and can also be used to estimate
  proportion and size for site status categories.  Upper and lower confidence
  bounds also are estimated.
}
\usage{
category.est(catvar, wgt, x=NULL, y=NULL, stratum=NULL, cluster=NULL,
   N.cluster=NULL, wgt1=NULL, x1=NULL, y1=NULL, popsize=NULL, stage1size=NULL,
   support=NULL, swgt=NULL, swgt1=NULL, unitsize=NULL, vartype="Local", conf=95,
   check.ind=TRUE, warn.ind=NULL, warn.df=NULL, warn.vec=NULL)
}
\arguments{
  \item{catvar}{the value of the categorical response variable or the site
    status for each site.}
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
  \item{N.cluster}{the number of stage one sampling units in the resource, which 
    is required for calculation of finite and continuous population 
    correction factors for a two-stage sample.  For a stratified sample 
    this variable must be a vector containing a value for each stratum and
    must have the names attribute set to identify the stratum codes.  The
    default is NULL.}
  \item{wgt1}{the final adjusted stage one weight for each site.  The default
    is NULL.}
  \item{x1}{the stage one x-coordinate for location for each site.  The default
    is NULL.}
  \item{y1}{the stage one y-coordinate for location for each site.  The default
    is NULL.}
  \item{popsize}{the known size of the resource - the total number of sampling 
    units of a finite resource or the measure of an extensive resource,
    which is required for calculation of finite and continuous population 
    correction factors for a single-stage sample.  This variable is also 
    used to adjust estimators for the known size of a resource.  For a
    stratified sample this variable must be a vector containing a value 
    for each stratum and must have the names attribute set to identify the
    stratum codes.  The default is NULL.}
  \item{stage1size}{the known size of the stage one sampling units of a two-
    stage sample, which is required for calculation of finite and  
    continuous population correction factors for a two-stage sample and 
    must have the names attribute set to identify the stage one sampling 
    unit codes.  For a stratified sample, the names attribute must be set
    to identify both stratum codes and stage one sampling unit codes using
    a convention where the two codes are separated by the \# symbol, e.g.,
    "Stratum 1\#Cluster 1".  The default is NULL.}
  \item{support}{the support value for each site - the value one (1) for a 
    site from a finite resource or the measure of the sampling unit  
    associated with a site from an extensive resource, which is required  
    for calculation of finite and continuous population correction  
    factors.  The default is NULL.}
  \item{swgt}{the size-weight for each site, which is the stage two size-weight 
    for a two-stage sample.  The default is NULL.}
  \item{swgt1}{the stage one size-weight for each site.  The default is NULL.}
  \item{unitsize}{the known sum of the size-weights of the resource, which for a 
    stratified sample must be a vector containing a value for each stratum 
    and must have the names attribute set to identify the stratum codes.  
    The default is NULL.}
  \item{vartype}{the choice of variance estimator, where "Local" = local mean
    estimator and "SRS" = SRS estimator.  The default is "Local".}
  \item{conf}{the confidence level.  The default is 95\%.}
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
  Proportion estimates are calculated using the Horvitz-Thompson ratio estimator,
  i.e., the ratio of two Horvitz-Thompson estimators.  The numerator of the ratio
  estimates the size of the category.  The denominator of the ratio
  estimates the size of the resource.   Variance estimates for the proportion
  estimates are calculated using either the local mean variance estimator or the
  simple random sampling (SRS) variance estimator.  The choice of variance
  estimator is subject to user control.  The local mean variance estimator
  requires the x-coordinate and the y-coordinate of each site.  The
  SRS variance estimator uses the independent  random sample approximation
  to calculate joint inclusion probabilities.   Confidence bounds are calculated
  using a Normal distribution multiplier.  For a finite resource size is
  the number of units in the resource.  For an extensive resource size is the 
  measure (extent) of the resource, i.e., length, area, or volume.  Size 
  estimates are calculated using the Horvitz- Thompson estimator.  Variance 
  estimates for the size estimates are calculated using either the local mean 
  variance estimator or the SRS variance estimator. The function can 
  accommodate a stratified sample.  For a stratified sample, separate estimates 
  and standard errors are calculated for each stratum, which are used to produce 
  estimates and standard errors for all strata combined.  Strata that contain a 
  single value are removed.  For a stratified sample, when either the size of the
  resource or the sum of the size-weights for the resource is provided for each 
  stratum, those values are used as  stratum weights for calculating the 
  estimates and standard errors for all strata combined.  In addition, when 
  either of those known values is provided for each stratum, size estimates are 
  obtained by multiplying the proportion estimate, i.e., the Horvitz-Thompson 
  ratio estimator, by the known value for the stratum.  For a stratified sample 
  when neither the size of the  resource nor the sum of the size-weights of the 
  resource is provided for  each stratum, estimated values are used as stratum 
  weights for calculating  the estimates and standard errors for all strata 
  combined.  The function can accommodate single-stage and two-stage samples for 
  both stratified and unstratified sampling designs.  Finite population and 
  continuous population correction factors can be utilized in variance 
  estimation.  The function checks for compatibility of input values and removes
  missing values.
}
\section{Value}{
  If the function was called by the cat.analysis function, then value is a list
  containing the following components:
  \itemize{
    \item \code{Results} - a data frame containing estimates and confidence
      bounds
    \item \code{warn.ind} - a logical value indicating whether warning messages
      were generated
    \item \code{warn.df} - a data frame containing warning messages
  }
  If the function was called directly, then value is a data frame containing
  estimates and confidence bounds.
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
catvar <- rep(c("north", "south", "east", "west"), rep(25, 4))
wgt <- runif(100, 10, 100)
category.est(catvar, wgt, vartype="SRS")

x <- runif(100)
y <- runif(100)
category.est(catvar, wgt, x, y)
}
\keyword{survey}
\keyword{univar}
