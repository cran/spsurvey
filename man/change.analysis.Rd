\name{change.analysis}
\alias{change.analysis}
\title{Change Analysis for Probability Survey Data}
\description{
  This function organizes input and output for analysis of change between two
  probability surveys.
}
\usage{
change.analysis(sites, repeats=NULL, subpop=NULL, design, data.cat=NULL,
   data.cont=NULL, revisitwgt=FALSE, test="mean", popsize_1=NULL,
   popsize_2=NULL, popcorrect_1=FALSE, popcorrect_2=FALSE, pcfsize_1=NULL,
   pcfsize_2=NULL, N.cluster_1=NULL, N.cluster_2=NULL, stage1size_1=NULL,
   stage1size_2=NULL, sizeweight_1=FALSE, sizeweight_2=FALSE, vartype_1="Local",
   vartype_2="Local", conf=95)
}
\arguments{
  \item{sites}{a data frame consisting of three variables: the first variable is
    site IDs, and the other variables are logical vectors indicating which
    sites to use in the analysis.  The first logical vector indicates the
    complete set of sites for the first survey.  The second logical vector
    indicates the complete set of sites for the second survey.}
  \item{repeats}{a data frame that identifies site IDs for repeat visit sites
   from the two surveys.   The first variable is site IDs for survey one. The
   second variable is site IDs for survey two.  For each row of the data frame,
   the two site IDs must correspond to the same site. This argument should equal
   NULL when repeat visit sites are not present.  The default is NULL.}
  \item{subpop}{a data frame describing sets of populations and subpopulations 
    for which estimates will be calculated.  The first variable is site  
    IDs.  Each subsequent variable identifies a Type of population, where
    the variable name is used to identify Type.  A Type variable
    identifies each site with one of the subpopulations of that Type.  The
    default is NULL.}
  \item{design}{a data frame consisting of design variables.  Variables should
   be named as follows:\cr
       siteID = site IDs\cr
       wgt = final adjusted weights, which are either the weights for a
         single-stage sample or the stage two weights for a two-stage sample\cr
       xcoord = x-coordinates for location, which are either the x-coordinates
         for a single-stage sample or the stage two x-coordinates for a
         two-stage sample\cr
       ycoord = y-coordinates for location, which are either the y-coordinates
         for a single-stage sample or the stage two y-coordinates for a
         two-stage sample\cr
       stratum = the stratum codes\cr
       cluster = the stage one sampling unit (primary sampling unit or cluster)
         codes\cr
       wgt1 = final adjusted stage one weights\cr
       xcoord1 = the stage one x-coordinates for location\cr
       ycoord1 = the stage one y-coordinates for location\cr
       support = support values - the value one (1) for a site from a 
         finite resource or the measure of the sampling unit associated 
         with a site from an extensive resource, which is required for 
         calculation of finite and continuous population correction 
         factors\cr
       swgt = size-weights, which is the stage two size-weight for a two-
         stage sample\cr
       swgt1 = stage one size-weights}
  \item{data.cat}{a data frame of categorical response variables.  The first 
    variable is site IDs.  Subsequent variables are response variables.
    Missing data (NA) is allowed.  The default is NULL.}
  \item{data.cont}{a data frame of continuous response variables.  The first 
    variable is site IDs.  Subsequent variables are response variables.
    Missing data (NA) is allowed.  The default is NULL.}
  \item{revisitwgt}{a logical value that indicates whether each repeat visit
    site has the same survey design weight in the two surveys, where TRUE = the
    weight for each repeat visit site is the same and FALSE = the weight for
    each repeat visit site is not the same.  When this argument is FALSE, all
    of the repeat visit sites are assigned equal weights when calculating the
    covariance component of the change estimate standard error.  The default
    is FALSE.}
  \item{test}{a character string or character vector providing the location
    measure(s) to use for change estimation for continuous variables.  The
    choices are "mean", "median", or c("mean", "median").  The default is
    "mean".}
  \item{popsize_1}{known size of the resource for survey one, which is used to
    perform ratio adjustment to estimators expressed using measurement units for
    the resource and to calculate strata proportions for calculating estimates
    for a stratified sample.  For a finite resource, this argument is either the
    total number of sampling units or the known sum of size-weights.  For an
    extensive resource, this argument is the measure of the resource, i.e.,
    either known total length for a linear resource or known total area for an
    areal resource.  The argument must be in the form of a list containing an
    element for each population Type in the subpop data frame, where NULL is a
    valid choice for a population Type.  The list must be named using the
    column names for the population Types in subpop. If a population Type
    doesn't contain subpopulations, then each element of the list is either a
    single value for an unstratified sample or a vector containing a value for
    each stratum for a stratified sample, where elements of the vector are
    named using the stratum codes.  If a population Type contains
    subpopulations, then each element of the list is a list containing an
    element for each subpopulation, where the list is named using the
    subpopulation names.  The element for each subpopulation will be either a
    single value for an unstratified sample or a named vector of values for a
    stratified sample.  The default is NULL.\cr\cr
    Example popsize for a stratified sample:\cr
      popsize = list("Pop 1"=c("Stratum 1"=750,\cr
                               "Stratum 2"=500,\cr
                               "Stratum 3"=250),\cr
                     "Pop 2"=list("SubPop 1"=c("Stratum 1"=350,\cr
                                               "Stratum 2"=250,\cr
                                               "Stratum 3"=150),\cr
                                  "SubPop 2"=c("Stratum 1"=250,\cr
                                               "Stratum 2"=150,\cr
                                               "Stratum 3"=100),\cr
                                  "SubPop 3"=c("Stratum 1"=150,\cr
                                               "Stratum 2"=150,\cr
                                               "Stratum 3"=75)),\cr
                     "Pop 3"=NULL)\cr\cr
    Example popsize for an unstratified sample:\cr
      popsize = list("Pop 1"=1500,\cr
                     "Pop 2"=list("SubPop 1"=750,\cr
                                  "SubPop 2"=500,\cr
                                  "SubPop 3"=375),\cr
                     "Pop 3"=NULL)\cr}
  \item{popsize_2}{known size of the resource for survey two.  The default is
    NULL.}
  \item{popcorrect_1}{a logical value that indicates whether finite or
    continuous population correction factors should be employed during variance
    estimation for survey one, where TRUE = use the correction factor and FALSE
    = do not use the correction factor.  The default is FALSE.  To employ the
    correction factor for a single-stage sample, values must be supplied for
    argument pcfsize_1 and for the support variable of the design argument.  To
    employ the correction factor for a two-stage sample, values must be supplied
    for arguments N.cluster_1 and  stage1size_1 and for the support variable of
    the design argument.}
  \item{popcorrect_2}{a logical value that indicates whether finite or
    continuous population correction factors should be employed during variance
    estimation for survey two, where TRUE = use the correction factor and FALSE
    = do not use the correction factor.  The default is FALSE.  To employ the
    correction factor for a single-stage sample, values must be supplied for
    argument pcfsize_2 and for the support variable of the design argument.  To
    employ the correction factor for a two-stage sample, values must be supplied
    for arguments N.cluster_2 and  stage1size_2 and for the support variable of
    the design argument.}
  \item{pcfsize_1}{size of the resource for survey one, which is required for
    calculation of finite and continuous population correction factors for a
    single-stage sample.  For a stratified sample this argument must be a vector
    containing a value for each stratum and must have the names attribute set to
    identify the stratum codes.  The default is NULL.}
  \item{pcfsize_2}{size of the resource for survey two.  The default is NULL.}
  \item{N.cluster_1}{the number of stage one sampling units in the resource for
    survey one, which is required for calculation of finite and continuous
    population correction factors for a two-stage sample.  For a stratified
    sample this variable must be a vector containing a value for each stratum
    and must have the names attribute set to identify the stratum codes.  The
    default is NULL.}
  \item{N.cluster_2}{the number of stage one sampling units in the resource for
    survey two.  The default is NULL.}
  \item{stage1size_1}{size of the stage one sampling units of a two-stage sample
    for survey one, which is required for calculation of finite and continuous
    population correction factors for a two-stage sample and must have the names
    attribute set to identify the stage one sampling unit codes.  For a
    stratified sample, the names attribute must be set to identify both
    stratum codes and stage one sampling unit codes using a convention where
    the two codes are separated by the & symbol, e.g., "Stratum 1&Cluster 1".
    The default is NULL.}
  \item{stage1size_2}{size of the stage one sampling units of a two-stage sample
    for survey two.  The default is NULL.}
  \item{sizeweight_1}{a logical value that indicates whether size-weights should 
    be used in the analysis of survey one, where TRUE = use the size-weights and
    FALSE = do not use the size-weights.  The default is FALSE.}
  \item{sizeweight_2}{a logical value that indicates whether size-weights should 
    be used in the analysis of survey two.  The default is FALSE.}
  \item{vartype_1}{the choice of variance estimator for survey one, where
    "Local" = local mean estimator and "SRS" = SRS estimator.  The default is
    "Local".}
  \item{vartype_2}{the choice of variance estimator for survey two.  The default
    is "Local".}
  \item{conf}{the confidence level.  The default is 95\%.}
}
\value{
  Value is a data frame of change estimates for all combinations of population
  Types, subpopulations within Types, response variables, and categories within
  each response variable (for categorical variables only).  Estimates provided
  plus standard error and confidence interval estimates.
}
\references{
  Diaz-Ramos, S., D.L. Stevens, Jr., and A.R. Olsen. (1996).  \emph{EMAP
  Statistical Methods Manual.} EPA/620/R-96/XXX.  Corvallis, OR: U.S.
  Environmental Protection Agency, Office of Research and Development, National
  Health Effects and Environmental Research Laboratory, Western Ecology
  Division.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\seealso{
\code{\link{change.est}} 
}
\examples{
# Categorical variable example for three resource classes:
mysiteID <- paste("Site", 1:200, sep="")
mysites <- data.frame(siteID=mysiteID,
                      Survey1=rep(c(TRUE, FALSE), c(100,100)),
                      Survey2=rep(c(FALSE, TRUE), c(100,100)))
myrepeats <- data.frame(siteID_1=paste("Site", 1:40, sep=""),
                        siteID_2=paste("Site", 101:140, sep=""))
mysubpop <- data.frame(siteID=mysiteID,
                       All_Sites=rep("All Sites", 200),
                       Region=rep(c("North","South"), 100))
mydesign <- data.frame(siteID=mysiteID,
                       wgt=runif(200, 10, 100),
                       xcoord=runif(200),
                       ycoord=runif(200),
                       stratum=rep(rep(c("Stratum1", "Stratum2"), c(2,2)), 50))
mydata.cat <- data.frame(siteID=mysiteID,
                         Resource_Class=sample(c("Good","Fair","Poor"),
                            200, replace=TRUE))
change.analysis(sites=mysites, repeats=myrepeats, subpop=mysubpop,
   design=mydesign, data.cat=mydata.cat, data.cont=NULL)
}
\keyword{survey}
\keyword{univar}
