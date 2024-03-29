% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trend_analysis.R
\name{trend_analysis}
\alias{trend_analysis}
\title{Trend analysis}
\usage{
trend_analysis(
  dframe,
  vars_cat = NULL,
  vars_cont = NULL,
  subpops = NULL,
  model_cat = "SLR",
  cat_rhs = NULL,
  model_cont = "LMM",
  cont_rhs = NULL,
  siteID = "siteID",
  yearID = "year",
  weight = "weight",
  xcoord = NULL,
  ycoord = NULL,
  stratumID = NULL,
  clusterID = NULL,
  weight1 = NULL,
  xcoord1 = NULL,
  ycoord1 = NULL,
  sizeweight = FALSE,
  sweight = NULL,
  sweight1 = NULL,
  fpc = NULL,
  popsize = NULL,
  invprboot = TRUE,
  nboot = 1000,
  vartype = "Local",
  jointprob = "overton",
  conf = 95,
  All_Sites = FALSE
)
}
\arguments{
\item{dframe}{Data to be analyzed (analysis data). A data frame or
\code{sf} object containing survey design variables, response
variables, and subpopulation (domain) variables.}

\item{vars_cat}{Vector composed of character values that identify the names
of categorical response variables in \code{dframe}. If
argument \code{model_cat} equals "GLMM", the categorical variables in the
\code{dframe} data frame must be factors each of which has two levels,
where the second level will be assumed to specify "success".  The default
value is \code{NULL}.}

\item{vars_cont}{Vector composed of character values that identify the
names of continuous response variables in \code{dframe}.
The default value is \code{NULL}.}

\item{subpops}{Vector composed of character values that identify the
names of subpopulation (domain) variables in \code{dframe}.
If a value is not provided, the value \code{"All_Sites"} is assigned to the
subpops argument and a factor variable named \code{"All_Sites"} that takes
the value \code{"All Sites"} is added to \code{dframe}.  The
default value is \code{NULL}.}

\item{model_cat}{Character value identifying the analytical procedure used
for trend estimation for categorical variables.  The choices are:
\code{"SLR"} (simple linear regression), \code{"WLR"} (weighted linear
regression), and \code{"GLMM"} (generalized linear mixed-effects model).
The default value is \code{"SLR"}.}

\item{cat_rhs}{Character value specifying the right hand side of the formula
for a generalized linear mixed-effects model.  If a value is not provided,
the argument is assigned a value that specifies the Piepho and Ogutu (2002)
model.  The default value is \code{NULL}.}

\item{model_cont}{Character value identifying the analytical procedure used
for trend estimation for continuous variables.  The choices are:
\code{"SLR"} (simple linear regression), \code{"WLR"} (weighted linear
regression), and \code{"LMM"} (linear mixed-effects model).  The default
value is \code{"LMM"}.}

\item{cont_rhs}{Character value specifying the right hand side of the
formula for a linear mixed-effects model.  If a value is not provided, the
argument is assigned a value that specifies the Piepho and Ogutu (2002)
model.  The default value is \code{NULL}.}

\item{siteID}{Character value providing name of the site ID variable in
\code{dframe}.  If repeat visit sites are present, the site
ID value for each revisit site will be the same for each survey.  For a
two-stage sample, the site ID variable identifies stage two site IDs.  The
default value is \code{"siteID"}.}

\item{yearID}{Character value providing name of the time period variable in
\code{dframe}, which must be numeric and will be forced to
numeric if it is not.  The default assumption is that the time period
variable is years.  The default value is \code{"year"}.}

\item{weight}{Character value providing name of the design weight
variable in \code{dframe}.  For a two-stage sample, the
weight variable identifies stage two weights.  The default value is
\code{"weight"}.}

\item{xcoord}{Character value providing name of the x-coordinate variable in
\code{dframe}.  For a two-stage sample, the x-coordinate
variable identifies stage two x-coordinates.  Note that x-coordinates are
required  for calculation of the local mean variance estimator.  If \code{dframe}
is an \code{sf} object, this argument is not required (as the geometry column
in \code{dframe} is used to find the x-coordinate). The
default value is \code{NULL}.}

\item{ycoord}{Character value providing name of the y-coordinate variable in
\code{dframe}.  For a two-stage sample, the y-coordinate
variable identifies stage two y-coordinates.  Note that y-coordinates are
required for calculation of the local mean variance estimator.  If \code{dframe}
is an \code{sf} object, this argument is not required (as the geometry column
in \code{dframe} is used to find the y-coordinate). The default
value is \code{NULL}.}

\item{stratumID}{Character value providing name of the stratum ID variable in
\code{dframe}.  The default value is \code{NULL}.}

\item{clusterID}{Character value providing name of the cluster (stage one) ID
variable in \code{dframe}.  Note that cluster IDs are
required for a two-stage sample.  The default value is \code{NULL}.}

\item{weight1}{Character value providing name of the stage one weight
variable in \code{dframe}.  The default value is \code{NULL}.}

\item{xcoord1}{Character value providing name of the stage one x-coordinate
variable in \code{dframe}.  Note that x-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{ycoord1}{Character value providing name of the stage one y-coordinate
variable in \code{dframe}.  Note that y-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{sizeweight}{Logical value that indicates whether size weights should be
used during estimation, where \code{TRUE} = use size weights and
\code{FALSE} = do not use size weights. To employ size weights for a
single-stage sample, a value must be supplied for argument weight.  To
employ size weights for a two-stage sample, values must be supplied for
arguments \code{weight} and \code{weight1}. The default value is
\code{FALSE}.}

\item{sweight}{Character value providing name of the size weight variable in
\code{dframe}.  For a two-stage sample, the size weight
variable identifies stage two size weights.  The default value is
\code{NULL}.}

\item{sweight1}{Character value providing name of the stage one size weight
variable in \code{dframe}.  The default value is
\code{NULL}.}

\item{fpc}{Object that specifies values required for calculation of the
  finite population correction factor used during variance estimation. The
  object must match the survey design in terms of stratification and whether
  the design is single-stage or two-stage.  For an unstratified design, the
  object is a vector.  The vector is composed of a single numeric value for a
  single-stage design.  For a two-stage unstratified design, the object is a
  named vector containing one more than the number of clusters in the sample,
  where the first item in the vector specifies the number of clusters in the
  population and each subsequent item specifies the number of stage two units
  for the cluster.  The name for the first item in the vector is arbitrary.
  Subsequent names in the vector identify clusters and must match the cluster
  IDs.  For a stratified design, the object is a named list of vectors, where
  names must match the strata IDs.  For each stratum, the format of the
  vector is identical to the format described for unstratified single-stage
  and two-stage designs.  Note that the finite population correction factor
  is not used with the local mean variance estimator.

  Example fpc for a single-stage unstratified survey design:

  \verb{fpc <- 15000}

  Example fpc for a single-stage stratified survey design:

  \verb{fpc <- list(
    Stratum_1 = 9000,
    Stratum_2 = 6000)
   }

  Example fpc for a two-stage unstratified survey design:

  \verb{fpc <- c(
    Ncluster = 150,
    Cluster_1 = 150,
    Cluster_2 = 75,
    Cluster_3 = 75,
    Cluster_4 = 125,
    Cluster_5 = 75)
  }

  Example fpc for a two-stage stratified survey design:

  \verb{fpc <- list(
    Stratum_1 = c(
      Ncluster_1 = 100,
      Cluster_1 = 125,
      Cluster_2 = 100,
      Cluster_3 = 100,
      Cluster_4 = 125,
      Cluster_5 = 50),
    Stratum_2 = c(
      Ncluster_2 = 50,
      Cluster_1 = 75,
      Cluster_2 = 150,
      Cluster_3 = 75,
      Cluster_4 = 75,
      Cluster_5 = 125))
  }}

\item{popsize}{Object that provides values for the population argument of the
  \code{calibrate} or \code{postStratify} functions in the survey package. If
  a value is provided for popsize, then either the \code{calibrate} or
  \code{postStratify} function is used to modify the survey design object
  that is required by functions in the survey package.  Whether to use the
  \code{calibrate} or \code{postStratify} function is dictated by the format
  of popsize, which is discussed below.  Post-stratification adjusts the
  sampling and replicate weights so that the joint distribution of a set of
  post-stratifying variables matches the known population joint distribution.
  Calibration, generalized raking, or GREG estimators generalize
  post-stratification and raking by calibrating a sample to the marginal
  totals of variables in a linear regression model. For the \code{calibrate}
  function, the object is a named list, where the names identify factor
  variables in \code{dframe}.  Each element of the list is a
  named vector containing the population total for each level of the
  associated factor variable.  For the \code{postStratify} function, the
  object is either a data frame, table, or xtabs object that provides the
  population total for all combinations of selected factor variables in the
  \code{dframe} data frame.  If a data frame is used for \code{popsize}, the
  variable containing population totals must be the last variable in the data
  frame.  If a table is used for \code{popsize}, the table must have named
  \code{dimnames} where the names identify factor variables in the
  \code{dframe} data frame.  If the popsize argument is equal to \code{NULL},
  then neither calibration nor post-stratification is performed.  The default
  value is \code{NULL}.

  Example popsize for calibration:

  \verb{popsize <- list(
    Ecoregion = c(
      East = 750,
      Central = 500,
      West = 250),
    Type = c(
      Streams = 1150,
      Rivers = 350))
  }

  Example popsize for post-stratification using a data frame:

  \verb{popsize <- data.frame(
    Ecoregion = rep(c("East", "Central", "West"),
      rep(2, 3)),
    Type = rep(c("Streams", "Rivers"), 3),
    Total = c(575, 175, 400, 100, 175, 75))
  }

  Example popsize for post-stratification using a table:

  \verb{popsize <- with(MySurveyFrame,
    table(Ecoregion, Type))}

  Example popsize for post-stratification using an xtabs object:

  \verb{popsize <- xtabs(~Ecoregion + Type,
    data = MySurveyFrame)}}

\item{invprboot}{Logical value that indicates whether the inverse probability
bootstrap procedure is used to calculate trend parameter estimates.  This
bootstrap procedure is only available for the "LMM" option for continuous
variables.  Inverse probability references the design weights, which
are the inverse of the sample inclusion probabilities.  The default value
is \code{TRUE}.}

\item{nboot}{Numeric value for the number of bootstrap iterations.  The
default is \code{1000}.}

\item{vartype}{Character value providing choice of the variance estimator,
where \code{"Local"} = the local mean estimator, \code{"SRS"} = the simple
random sampling estimator, \code{"HT"} = the Horvitz-Thompson estimator,
and \code{"YG"} = the Yates-Grundy estimator.  The default value is
\code{"Local"}.}

\item{jointprob}{Character value providing choice of joint inclusion
probability approximation for use with Horvitz-Thompson and Yates-Grundy
variance estimators, where \code{"overton"} indicates the Overton
approximation, \code{"hr"} indicates the Hartley_Rao approximation, and
\code{"brewer"} equals the Brewer approximation.  The default value is
\code{"overton"}.}

\item{conf}{Numeric value for the Gaussian-based confidence level.  The default is
\code{95}.}

\item{All_Sites}{A logical variable used when \code{subpops} is not
\code{NULL}. If \code{All_Sites} is \code{TRUE}, then alongside the
subpopulation output, output for all sites (ignoring subpopulations) is
returned for each variable in \code{vars}. If \code{All_Sites} is
\code{FALSE}, then alongside the subpopulation output, output for all sites
(ignoring subpopulations) is not returned for each variable in \code{vars}.
The default is \code{FALSE}.}
}
\value{
The analysis results. A list composed of two data frames containing trend estimates for all
  combinations of population Types, subpopulations within Types, and response
  variables.  For categorical variables, trend estimates are calculated for
  each category of the variable.  The two data frames in the output list are:
  \describe{
    \item{\code{catsum}}{data frame containing trend estimates for
      categorical variables}
    \item{\code{contsum}}{data frame containing trend
      estimates for continuous variables}
   }

  For the SLR and WLR model options, the data frame contains the following
  variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{Trend_Estimate}{trend estimate}
    \item{Trend_Std_Error}{trend standard error}
    \item{Trend_LCBxxPct}{trend xx\% (default 95\%) lower confidence bound}
    \item{Trend_UCBxxPct}{trend xx\% (default 95\%) upper confidence bound}
    \item{Trend_p_Value}{trend p-value}
    \item{Intercept_Estimate}{intercept estimate}
    \item{Intercept_Std_Error}{intercept standard error}
    \item{Intercept_LCBxxPct}{intercept xx\% (default 95\%) lower confidence
      bound}
    \item{Intercept_UCBxxPct}{intercept xx\% (default 95\%) upper confidence
      bound}
    \item{Intercept_p_Value}{intercept p-value}
    \item{R_Squared}{R-squared value}
    \item{Adj_R_Squared}{adjusted R-squared value}
  }

  For the GLMM and LMM model options, contents of the data frames will vary
  depending on the model specified by arguments \code{cat_rhs} and
  \code{cont_rhs}.  For the default PO model, the data frame contains the
  following variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{Trend_Estimate}{trend estimate}
    \item{Trend_Std_Error}{trend standard error}
    \item{Trend_LCBxxPct}{trend xx\% (default 95\%) lower confidence bound}
    \item{Trend_UCBxxPct}{trend xx\% (default 95\%) upper confidence bound}
    \item{Trend_p_Value}{trend p-value}
    \item{Intercept_Estimate}{intercept estimate}
    \item{Intercept_Std_Error}{intercept standard error}
    \item{Intercept_LCBxxPct}{intercept xx\% (default 95\%) lower confidence
      bound}
    \item{Intercept_UCBxxPct}{intercept xx\% (default 95\%) upper confidence
      bound}
    \item{Intercept_p_Value}{intercept p-value}
    \item{Var_SiteInt}{variance of the site intercepts}
    \item{Var_SiteTrend}{variance of the site trends}
    \item{Corr_SiteIntSlope}{correlation of site intercepts and site trends}
    \item{Var_Year}{year variance}
    \item{Var_Residual}{residual variance}
    \item{AIC}{generalized Akaike Information Criterion}
  }
}
\description{
This function organizes input and output for estimation of trend across time
for a series of samples (for categorical and continuous variables). Trend is estimated using the
analytical procedure identified by the model arguments.  For categorical
variables, the choices for the \code{model_cat} argument are: (1) simple linear
regression, (2) weighted linear regression, and (3) generalized linear
mixed-effects model. For continuous variables, the choices for the
\code{model_cont} argument are: (1) simple linear regression, (2) weighted
linear regression, and (3)  linear mixed-effects model.  The analysis data,
\code{dframe}, can be either a data frame or a simple features (\code{sf}) object.  If an
\code{sf} object is used, coordinates are extracted from the geometry column in the
object, arguments \code{xcoord} and \code{ycoord} are assigned values
\code{"xcoord"} and \code{"ycoord"}, respectively, and the geometry column is
dropped from the object.
}
\section{Details}{

For the simple linear regression (SLR) model, a design-based estimate of the
category proportion (categorical variables) or the mean (continuous
variables) is calculated for each time period (year).  Four choices of
variance estimator are available for calculating variance of the design-based
estimates: (1) the local mean estimator, (2) the simple random sampling
estimator, (3) the Horvitz-Thompson estimator, and (4) the Yates-Grundy
estimator.  For the Horvitz-Thompson and Yates-Grundy estimators, there are
three choices for calculating joint inclusion probabilities: (1) the Overton
approximation, (2) the Hartley-Rao approximation, and (3) the Brewer
approximation.  The \code{lm} function in the stats package is used to fit a
linear model using a \code{formula} argument that specifies the proportion or
mean estimates as the response variable and years as the regressor variable.
For fitting the SLR model, the \code{yearID} variable from the \code{dframe}
argument is modified by subtracting the minimum value of years from all
values of the variable.  Parameter estimates are extracted from the object
returned by the \code{lm} function.  For the weighted linear regression (WLR)
model, the process is the same as the SLR model except that the inverse of
the variances of the proportion or mean estimates is used as the
\code{weights} argument in the call to the \code{lm} function.  For the LMM
option, the \code{lmer} function in the lme4 package is used to fit a linear
mixed-effects model for trend across years.  For both the GLMM and LMM
options, the default Piepho and Ogutu (PO) model includes fixed effects for
intercept and trend (slope) and random effects for intercept and trend for
individual sites, where the \code{siteID} variable from the \code{dframe}
argument identifies sites.  Correlation between the random effects for site
intercepts and site trends is included in the model. Finally, the PO model
contains random effects for year variance and residual variance. For the GLMM
and LMM options, arguments \code{cat_rhs} and \code{cont_rhs}, respectively,
can be used to specify the right hand side of the model formula. Internally,
a variable named \code{Wyear} is created that is useful for specifying the
\code{cat_rhs} and \code{cont_rhs} arguments.  The \code{Wyear} variable is
created by subtracting the minimum value of the \code{yearID} variable from
all values of the variable.  If argument \code{invprboot} is \code{FALSE},
parameter estimates are extracted from the object returned by the \code{lmer}
function. If argument \code{invprboot} is \code{TRUE}, the \code{boot}
function in the boot package is used to generate bootstrap replicates using a
function named \code{bootfcn} as the \code{statistic} argument passed to the
\code{boot} function.  For each bootstrap replicate, \code{bootfcn} calls the
\code{glmer} or \code{lmer} function, as appropriate, using the specified
model.  design weights identified by the \code{weight} argument for
the \code{trend_analysis} function are passed as the \code{weights} argument
for the \code{boot} function, which specifies importance weights.  Using the
design weights as the \code{weights} argument ensures that bootstrap
replicates are representative of the survey population.  Parameter estimates
are calculated using the object returned by the \code{boot} function.
}

\examples{
# Example using a categorical variable with three resource classes and a
# continuous variable
mydframe <- data.frame(
  siteID = rep(paste0("Site", 1:40), rep(5, 40)),
  yearID = rep(seq(2000, 2020, by = 5), 40),
  wgt = rep(runif(40, 10, 100), rep(5, 40)),
  xcoord = rep(runif(40), rep(5, 40)),
  ycoord = rep(runif(40), rep(5, 40)),
  All_Sites = rep("All Sites", 200),
  Region = sample(c("North", "South"), 200, replace = TRUE),
  Resource_Class = sample(c("Good", "Fair", "Poor"), 200, replace = TRUE),
  ContVar = rnorm(200, 10, 1)
)
myvars_cat <- c("Resource_Class")
myvars_cont <- c("ContVar")
mysubpops <- c("All_Sites", "Region")
trend_analysis(
  dframe = mydframe,
  vars_cat = myvars_cat,
  vars_cont = myvars_cont,
  subpops = mysubpops,
  model_cat = "WLR",
  model_cont = "SLR",
  siteID = "siteID",
  yearID = "yearID",
  weight = "wgt",
  xcoord = "xcoord",
  ycoord = "ycoord"
)
}
\seealso{
\describe{
  \item{\code{\link{change_analysis}}}{for change analysis}
  }
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
