\name{relrisk.est}
\alias{relrisk.est}
\title{Compute the Relative Risk Estimate}
\description{
  This function calculates the relative risk estimate for a 2x2 table of cell
  counts defined by a categorical response variable and a categorical
  explanatory (stressor) variable for an unequal probability design.  Relative
  risk is the ratio of two probabilities: the numerator is the probability that
  the first level of the response variable is observed given occurrence of the
  first level of the stressor variable, and the denominator is the probability
  that the first level of the response variable is observed given occurrence of
  the second level of the stressor variable.  The standard error of the base e
  log of the relative risk estimate and confidence limits for the estimate also
  are calculated.
}
\usage{
relrisk.est(response, stressor, response.levels=c("Poor", "Good"),
   stressor.levels=c("Poor", "Good"), wgt, xcoord=NULL, ycoord=NULL,
   stratum=NULL, cluster=NULL, wgt1=NULL, xcoord1=NULL, ycoord1=NULL,
   popcorrect=FALSE, pcfsize=NULL, N.cluster=NULL, stage1size=NULL,
   support=NULL, sizeweight=FALSE, swgt=NULL, swgt1=NULL, vartype="Local",
   conf=95, check.ind=TRUE, warn.ind=NULL, warn.df=NULL, warn.vec=NULL)
}
\arguments{
  \item{response}{the categorical response variable values.}
  \item{stressor}{the categorical explanatory (stressor) variable values.}
  \item{response.levels}{category values (levels) for the categorical response 
    variable, where the first level is used for calculating the numerator and 
    the denominator of the relative risk estimate.  If response.levels is not 
    supplied, then values "Poor" and "Good" are used for the first level and 
    second level of the response variable, respectively.  The default is 
    c("Poor", "Good").}
  \item{stressor.levels}{category values (levels) for the categorical stressor
    variable, where the first level is used for calculating the numerator of 
    the relative risk estimate and the second level is used for calculating 
    the denominator of the estimate.  If stressor.levels is not supplied, then
    values "Poor" and "Good" are used for the first level and second level of
    the stressor variable, respectively.  The default is c("Poor", "Good").}
  \item{wgt}{the final adjusted weight (inverse of the sample inclusion
    probability) for each site, which is either the weight for a 
    single-stage sample or the stage two weight for a two-stage sample.}
  \item{xcoord}{x-coordinate for location for each site, which is either the
    x-coordinate for a single-stage sample or the stage two 
    x-coordinate for a two-stage sample.  The default is NULL.}
  \item{ycoord}{y-coordinate for location for each site, which is either the
    y-coordinate for a single-stage sample or the stage two 
    y-coordinate for a two-stage sample.  The default is NULL.}
  \item{stratum}{the stratum for each site.  The default is NULL.}
  \item{cluster}{the stage one sampling unit (primary sampling unit or cluster) 
    code for each site.  The default is NULL.}
  \item{wgt1}{the final adjusted stage one weight for each site.  The default
    is NULL.}
  \item{xcoord1}{the stage one x-coordinate for location for each site.  The
    default is NULL.}
  \item{ycoord1}{the stage one y-coordinate for location for each site.  The
    default is NULL.}
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
  The relative risk estimate is computed using the ratio of a numerator
  probability to a denominator probability, which are estimated using cell and
  marginal totals from a 2x2 table of cell counts defined by a categorical
  response variable and a categorical stressor variable (Van Sickle and Paulsen,
  2008). An estimate of the numerator probability is provided by the ratio of
  the cell total defined by the first level of the response variable and the
  first level of the stressor variable to the marginal total for the first level
  of the stressor variable.  An estimate of the denominator probability is
  provided by the ratio of the cell total defined by the first level of response
  variable and the second level of the stressor variable to the marginal total
  for the second level of the stressor variable.  Cell and marginal totals are
  estimated using the Horvitz-Thompson estimator. The standard error of the base
  e log of the relative risk estimate is calculated using a first-order Taylor
  series linearization (Sarndal et al., 1992).
}
\section{Value}{
  If the function was called by the relrisk.analysis function, then value is a 
  list containing the following components:
  \itemize{
    \item \code{Results} - a list containing estimates, confidence
      bounds, and associated values
    \item \code{warn.ind} - a logical value indicating whether warning messages
      were generated
    \item \code{warn.df} - a data frame containing warning messages
  }
  If the function was called directly, then value is the Results list, which 
  contains the following components:
  \itemize{
    \item \code{RelRisk} - the relative risk estimate
    \item \code{RRnum} - numerator ("elevated" risk) of the relative risk
      estimate
    \item \code{RRdenom} - denominator ("baseline" risk) of the relative risk
      estimate
    \item \code{RRlog.se} - standard error for the log of the relative risk
      estimate
    \item \code{ConfLimits} - confidence limits for the relative risk estimate
    \item \code{WeightTotal} - sum of the final adjusted weights
    \item \code{CellCounts} - cell and margin counts for the 2x2 table
    \item \code{CellProportions} - estimated cell proportions for the 2x2 table
  }
}
\references{
  Van Sickle, J. and S. G. Paulsen. (2008). Assessing the attributable risks,
  relative risks, and regional extent of aquatic stressors. \emph{Journal of the
  North American Benthological Society} \bold{27}, 920-931.\cr
  Sarndal, C.E., B. Swensson, and J. Wretman. (1992). \emph{Model Assisted
  Survey Sampling}. Springer-Verlag, New York.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}\cr
Tony Olsen \email{Olsen.Tony@epa.gov}\cr
John Vansickle \email{Vansickle.John@epa.gov}}
\examples{
response <- sample(c("Poor", "Good"), 100, replace=TRUE)
stressor <- sample(c("Poor", "Good"), 100, replace=TRUE)
wgt <- runif(100, 10, 100)
relrisk.est(response, stressor, wgt=wgt, vartype="SRS")

xcoord <- runif(100)
ycoord <- runif(100)
relrisk.est(response, stressor, wgt=wgt, xcoord=xcoord, ycoord=ycoord)
}
\keyword{survey}
\keyword{survival}
