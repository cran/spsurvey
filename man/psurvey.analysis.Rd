\name{psurvey.analysis}
\alias{psurvey.analysis}
\title{Create an Object of Class psurvey.analysis}
\description{
  This function creates an object of class psurvey.analysis that contains all 
  of the information necessary to use the analysis functions in the 
  psurvey.analysis library.
}
\usage{
psurvey.analysis(sites=NULL, subpop=NULL, design=NULL, data.cat=NULL,
   data.cont=NULL, siteID=NULL, wgt=NULL, sigma=NULL, var.sigma=NULL,
   xcoord=NULL, ycoord=NULL, stratum=NULL, cluster=NULL, N.cluster=NULL,
   wgt1=NULL, xcoord1=NULL, ycoord1=NULL, popsize=NULL, stage1size=NULL,
   popcorrect=FALSE, support=NULL, sizeweight=FALSE, swgt=NULL, swgt1=NULL,
   unitsize=NULL, vartype="Local", conf=95, pctval=c(5,10,25,50,75,90,95))
}
\arguments{
  \item{sites}{a data frame consisting of two variables: the first variable is 
    site IDs and the second variable is a logical vector indicating which 
    sites to use in the analysis.  If this data frame is not provided,  
    then the data frame will be created, where (1) site IDs are obtained  
    either from the design argument, the siteID argument, or both (when  
    siteID is a formula); and (2) a variable named use.sites that contains the 
    value TRUE for all sites is created.  
    The default is NULL.}
  \item{subpop}{a data frame describing sets of populations and subpopulations 
    for which estimates will be calculated.  The first variable is site  
    IDs and each subsequent variable identifies a Type of population,  
    where the variable name is used to identify Type.  A Type variable  
    identifies each site with one of the subpopulations of that Type.  If   
    this data frame is not provided, then the data frame will be created,  
    where (1) site IDs are obtained either from the design argument, the  
    siteID argument, or both (when siteID is a formula); and (2) a single Type 
    variable named all.sites that contains the value "All Sites" for all sites 
    is created.  The default is NULL.}
  \item{design}{a data frame consisting of design variables.  If variable names 
    are provided as formulas in the corresponding arguments, then the 
    formulas are interpreted using this data frame.  If this data frame is 
    not provided, then the data frame will be created from inputs to the 
    design variables in the argument list.  The default is NULL.  If 
    variable names are not provided as formulas, then variables should be 
    named as follows:\cr
       siteID = site IDs\cr
       wgt = final adjusted weights\cr
       xcoord = x-coordinates for location\cr
       ycoord = y-coordinates for location\cr
       stratum = stratum codes\cr
       cluster = stage one sampling unit codes\cr
       wgt1 = final adjusted stage one weights\cr
       xcoord1 = stage one x-coordinates for location\cr
       ycoord1 = stage one y-coordinates for location\cr
       support = support values\cr
       swgt = size-weights\cr
       swgt1 = stage one size-weights}
  \item{data.cat}{a data frame of categorical response variables.  The first 
    variable is site IDs.  Subsequent variables are response variables.
    Missing data (NA) is allowed.  The default is NULL.}
  \item{data.cont}{a data frame of continuous response variables.  The first 
    variable is site IDs.  Subsequent variables are response variables.
    Missing data (NA) is allowed.  The default is NULL.}
  \item{siteID}{site IDs.  This variable can be input directly or as a formula 
    and must be supplied either as this argument or in the design data 
    frame.  The default is NULL.}
  \item{wgt}{the final adjusted weight (inverse of the sample inclusion
    probability) for each site, which is either the weight for a 
    single-stage sample or the stage two weight for a two-stage sample.
    The default is NULL.}
  \item{sigma}{measurement error variance.  This variable must be a vector
    containing a value for each response variable and must have the names
    attribute set to identify the response variable names.  Missing data (NA)
    is allowed.  The default is NULL.}
  \item{var.sigma}{variance of the measurement error variance.  This variable
    must be a vector containing a value for each response variable and must have
    the names attribute set to identify the response variable names.  Missing
    data (NA) is allowed.  The default is NULL.}
  \item{xcoord}{x-coordinate for location for each site, which is either the
    x-coordinate for a single-stage sample or the stage two 
    x-coordinate for a two-stage sample.  The default is NULL.}
  \item{ycoord}{y-coordinate for location for each site, which is either the
    y-coordinate for a single-stage sample or the stage two 
    y-coordinate for a two-stage sample.  The default is NULL.}
  \item{stratum}{the stratum codes.  This variable can be input directly or as a 
    formula.  The default is NULL.}
  \item{cluster}{the stage one sampling unit (primary sampling unit or cluster) 
    codes.  This variable can be input directly or as a formula.  The 
    default is NULL.}
  \item{N.cluster}{the number of stage one sampling units in the resource, which 
    is required for calculation of finite and continuous population 
    correction factors for a two-stage sample.  For a stratified sample 
    this variable must be a vector containing a value for each stratum and
    must have the names attribute set to identify the stratum codes.  The
    default is NULL.}
  \item{wgt1}{the final adjusted stage one weights.  This variable can be input 
    directly or as a formula.  The default is NULL.}
  \item{xcoord1}{the stage one x-coordinates for location.  This variable can be 
    input directly or as a formula.  The default is NULL.}
  \item{ycoord1}{the stage one y-coordinates for location.  This variable can be 
    input directly or as a formula.  The default is NULL.}
  \item{popsize}{the known size of the resource - the total number of sampling 
    units of a finite resource or the measure of an extensive resource,
    which is used to adjust estimators for the known size of a resource.
    This argument also is required for calculation of finite and
    continuous population correction factors for a single-stage sample.   
    The argument must be in the form of a list containing an element for   
    each population Type in the subpop data frame, where NULL is a valid   
    choice for a population Type.  The list must be named using the column  
    names for the population Types in subpop. If a population Type doesn't  
    contain subpopulations, then each element of the list is either a  
    single value for an unstratified sample or a vector containing a value  
    for each stratum for a stratified sample, where elements of the vector
    are named using the stratum codes.  If a population Type contains 
    subpopulations, then each element of the list is a list containing an 
    element for each subpopulation, where the list is named using the 
    subpopulation names.  The element for each subpopulation will be 
    either a single value for an unstratified sample or a named vector of 
    values for a stratified sample.  The default is NULL.\cr\cr
    Example popsize for a stratified sample:\cr
       popsize = list("Pop 1"=c("Stratum 1"=750, "Stratum 2"=500,
          "Stratum 3"=250),\cr "Pop2"=list("SubPop 1"=c("Stratum 1"=350,
          "Stratum 2"=250, "Stratum 3"=150),\cr "SubPop 2"=c("Stratum 1"=250,
          "Stratum 2"=150, "Stratum 3"=100),\cr "SubPop 3"=c("Stratum 1"=150,
          "Stratum 2"=150, "Stratum 3"=75)),\cr "Pop 3"=NULL)\cr\cr
    Example popsize for an unstratified sample:\cr
       popsize = list("Pop 1"=1500, "Pop2"=list("SubPop 1"=750,
          "SubPop 2"=500, "SubPop 3"=375), "Pop 3"=NULL)\cr}
  \item{stage1size}{the known size of the stage one sampling units of a two-
    stage sample, which is required for calculation of finite and  
    continuous population correction factors for a two-stage sample and 
    must have the names attribute set to identify the stage one sampling 
    unit codes.  For a stratified sample, the names attribute must be set
    to identify both stratum codes and stage one sampling unit codes using
    a convention where the two codes are separated by the & symbol, e.g.,
    "Stratum 1&Cluster 1".  The default is NULL.}
  \item{popcorrect}{a logical value that indicates whether finite or continuous 
    population correction factors should be employed during variance 
    estimation, where TRUE = use the correction factors and FALSE = do not 
    use the correction factors.  The default is FALSE.}
  \item{support}{the support value for each site - the value one (1) for a 
    site from a finite resource or the measure of the sampling unit  
    associated with a site from an extensive resource, which is required  
    for calculation of finite and continuous population correction  
    factors.  This variable can be input directly or as a formula.  The default
    is NULL.}
  \item{sizeweight}{a logical value that indicates whether size-weights should 
    be used in the analysis, where TRUE = use the size-weights and FALSE = 
    do not use the size-weights.  The default is FALSE.}
  \item{swgt}{the size-weight for each site, which is the stage two size-weight 
    for a two-stage sample.  This variable can be input directly or as a 
    formula.  The default is NULL.}
  \item{swgt1}{the stage one size-weight for each site.  This variable can be 
    input directly or as a formula.  The default is NULL.}
  \item{unitsize}{the known sum of the size-weights of the resource.  The 
    argument must be in the form of a list containing an element for each  
    population Type in the subpop data frame, where NULL is a valid choice  
    for a population Type.  The list must be named using the column  
    names for population Types in subpop.  If a population Type doesn't  
    contain subpopulations, then each element of the list is either a  
    single value for an unstratified sample or a vector containing a value  
    for each stratum for a stratified sample, where elements of the vector
    are named using the stratum codes.  If a population Type contains 
    subpopulations, then each element of the list is a list containing an 
    element for each subpopulation, where the list is named using the 
    subpopulation names.  The element for each subpopulation will be 
    either a single value for an unstratified sample or a named vector of 
    values for a stratified sample.  The default is NULL.}
  \item{vartype}{the choice of variance estimator, where "Local" = local mean
    estimator and "SRS" = SRS estimator.  The default is "Local".}
  \item{conf}{the confidence level.  The default is 95\%.}
  \item{pctval}{the set of values at which percentiles are estimated.  The 
    default set is: {5, 10, 25, 50, 75, 90, 95}.}
}
\section{Value}{
  Value is a list of class psurvey.analysis.  Only those sites indicated by the 
  logical variable in the sites data frame are retained in the output. The
  sites, subpop, and design data frames will always exist in the output. At 
  least one of the data.cat and data.cont data frames will exist.  Depending
  upon values of the input variables, other elements in the output may be 
  NULL.  The list is composed of the following components:
  \itemize{
    \item \code{sites} - the sites data frame
    \item \code{subpop} - the subpop data frame
    \item \code{design} - the design data frame
    \item \code{data.cat} - the data.cat data frame
    \item \code{data.cont} - the data.cont data frame
    \item \code{N.cluster} - the number of stage one sampling units in the
      resource
    \item \code{popsize} - the known size of the resource
    \item \code{stage1size} - the known size of the stage one sampling units
    \item \code{unitsize} - the known sum of the size-weights of the resource
    \item \code{stratum.ind} - a logical value that indicates whether the sample
      is stratified, where TRUE = a stratified sample and FALSE = not a
      stratified sample
    \item \code{cluster.ind} - a logical value that indicates whether the sample
      is a two-stage sample, where TRUE = a two-stage sample and FALSE = not a
      two-stage sample
    \item \code{pcfactor.ind} - a logical value that indicates whether the
      population correction factor is used during variance estimation, where
      TRUE = use the population correction factor and FALSE = do not use the
      factor
    \item \code{swgt.ind} - a logical value that indicates whether the sample is
      a size-weighted sample, where TRUE = a size-weighted sample and FALSE =
      not a  size-weighted sample
    \item \code{vartype} - the choice of variance estimator, where "Local" =
      local mean estimator and "SRS" = SRS estimator
    \item \code{conf} - the confidence level
    \item \code{pctval} - the set of values at which percentiles are estimated,
      where the default set is: {5, 25, 50, 75, 95}
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
\seealso{
\code{\link{cat.analysis}, \link{cont.analysis}} 
}
\examples{
# Categorical variable example
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10,
   100), xcoord=runif(100), ycoord=runif(100), stratum= rep(c("Stratum1",
   "Stratum2"), 50))
mydata.cat <- data.frame(siteID=mysiteID, CatVar= rep(c("north", "south",
   "east", "west"), 25))
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))
psurvey.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, popsize=mypopsize)

# Continuous variable example - including deconvolution estimates
mydesign <- data.frame(ID=mysiteID, wgt=runif(100, 10, 100),
   xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
   "Stratum2"), 50))
ContVar <- rnorm(100, 10, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar,
   ContVar.1=ContVar + rnorm(100, 0, sqrt(0.25)),
   ContVar.2=ContVar + rnorm(100, 0, sqrt(0.50)))
mysigma <- c(ContVar=NA, ContVar.1=0.25, ContVar.2=0.50)
psurvey.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cont=mydata.cont, siteID=~ID, sigma=mysigma,
   popsize=mypopsize)
}
\keyword{survey}
