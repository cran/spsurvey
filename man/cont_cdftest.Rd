% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cont_cdftest.R
\name{cont_cdftest}
\alias{cont_cdftest}
\title{Cumulative distribution function (CDF) inference for a probability survey}
\usage{
cont_cdftest(
  dframe,
  vars,
  subpops = NULL,
  surveyID = NULL,
  siteID = "siteID",
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
  vartype = "Local",
  jointprob = "overton",
  testname = "adjWald",
  nclass = 3
)
}
\arguments{
\item{dframe}{Data frame containing survey design variables, response
variables, and subpopulation (domain) variables.}

\item{vars}{Vector composed of character values that identify the
names of response variables in the \code{dframe} data frame.}

\item{subpops}{Vector composed of character values that identify the
names of subpopulation (domain) variables in the \code{dframe} data frame.
If a value is not provided, the value \code{"All_Sites"} is assigned to the
subpops argument and a factor variable named \code{"All_Sites"} that takes
the value \code{"All Sites"} is added to the \code{dframe} data frame.  The
default value is \code{NULL}.}

\item{surveyID}{Character value providing name of the survey ID variable in
the \code{dframe} data frame.  If this argument equals \code{NULL}, then
the dframe data frame contains data for a single survey.  The default value
is \code{NULL}.}

\item{siteID}{Character value providing name of the site ID variable in
the \code{dframe} data frame.  For a two-stage sample, the site ID variable
identifies stage two site IDs.  The default value is \code{"siteID"}.}

\item{weight}{Character value providing name of the survey design weight
variable in the \code{dframe} data frame.  For a two-stage sample, the
weight variable identifies stage two weights.  The default value is
\code{"weight"}.}

\item{xcoord}{Character value providing name of the x-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the x-coordinate
variable identifies stage two x-coordinates.  Note that x-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{ycoord}{Character value providing name of the y-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the y-coordinate
variable identifies stage two y-coordinates.  Note that y-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{stratumID}{Character value providing name of the stratum ID variable in
the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{clusterID}{Character value providing the name of the cluster
(stage one) ID variable in the \code{dframe} data frame.  Note that cluster
IDs are required for a two-stage sample.  The default value is \code{NULL}.}

\item{weight1}{Character value providing name of the stage one weight
variable in the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{xcoord1}{Character value providing the name of the stage one
x-coordinate variable in the \code{dframe} data frame.  Note that x
coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{ycoord1}{Character value providing the name of the stage one
y-coordinate variable in the \code{dframe} data frame.  Note that
y-coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{sizeweight}{Logical value that indicates whether size weights should be
used during estimation, where \code{TRUE} uses size weights and
\code{FALSE} does not use size weights. To employ size weights for a
single-stage sample, a value must be supplied for argument weight.  To
employ size weights for a two-stage sample, values must be supplied for
arguments \code{weight} and \code{weight1}. The default value is \code{FALSE}.}

\item{sweight}{Character value providing the name of the size weight variable
in the \code{dframe} data frame.  For a two-stage sample, the size weight
variable identifies stage two size weights.  The default value is
\code{NULL}.}

\item{sweight1}{Character value providing name of the stage one size weight
variable in the \code{dframe} data frame.  The default value is \code{NULL}.}

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
  variables in the \code{dframe} data frame.  Each element of the list is a
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

\item{vartype}{Character value providing the choice of the variance
estimator, where \code{"Local"} indicates the local mean estimator,
\code{"SRS"} indicates the simple random sampling estimator, \code{"HT"}
indicates the Horvitz-Thompson estimator, and \code{"YG"} indicates the
Yates-Grundy estimator.  The default value is \code{"Local"}.}

\item{jointprob}{Character value providing the choice of joint inclusion
probability approximation for use with Horvitz-Thompson and Yates-Grundy
variance estimators, where \code{"overton"} indicates the Overton
approximation, \code{"hr"} indicates the Hartley-Rao approximation, and
\code{"brewer"} equals the Brewer approximation.  The default value is
\code{"overton"}.}

\item{testname}{Name of the test statistic to be reported in the output
data frame.  Choices for the name are: \code{"Wald"}, \code{"adjWald"},
\code{"RaoScott_First"}, and \code{"RaoScott_Second"}, which correspond to
the Wald statistic, adjusted Wald statistic, Rao-Scott first-order
corrected statistic, and Rao-Scott second-order corrected statistic,
respectively.  The default is \code{"adjWald"}.}

\item{nclass}{Number of classes into which the CDFs will be divided
(binned), which must equal at least \code{2}.  The default is \code{3}.}
}
\value{
Data frame of CDF test results for all pairs of subpopulations
  within each population type for every response variable.  The data frame
  includes the test statistic specified by argument \code{testname} plus its
  degrees of freedom and p-value.
}
\description{
This function organizes input and output for conducting inference regarding
cumulative distribution functions (CDFs) generated by a probability survey.
For every response variable and every subpopulation (domain) variable,
differences between CDFs are tested for every pair of subpopulations within
the domain.  Data input to the function can be either a single survey or
multiple surveys (two or more).  If the data contain multiple surveys, then
the domain variables will reference those surveys and (potentially)
subpopulations within those surveys.  The inferential procedures divide the
CDFs into a discrete set of intervals (classes) and then utilize procedures
that have been developed for analysis of categorical data from probability
surveys.  Choices for inference are the Wald, adjusted Wald, Rao-Scott first
order corrected (mean eigenvalue corrected), and Rao-Scott second order
corrected (Satterthwaite corrected) test statistics. The default test
statistic is the adjusted Wald statistic.  The input data argument can be
either a data frame or a simple features (sf) object.  If an sf object is
used, coordinates are extracted from the geometry column in the object,
arguments xcoord and ycoord are assigned values "xcoord" and "ycoord",
respectively, and the geometry column is dropped from the object.
}
\examples{
n <- 200
mysiteID <- paste("Site", 1:n, sep = "")
dframe <- data.frame(
  siteID = mysiteID,
  wgt = runif(n, 10, 100),
  xcoord = runif(n),
  ycoord = runif(n),
  stratum = rep(c("Stratum1", "Stratum2"), n / 2),
  Resource_Class = sample(c("Agr", "Forest", "Urban"), n, replace = TRUE)
)
ContVar <- numeric(n)
tst <- dframe$Resource_Class == "Agr"
ContVar[tst] <- rnorm(sum(tst), 10, 1)
tst <- dframe$Resource_Class == "Forest"
ContVar[tst] <- rnorm(sum(tst), 10.1, 1)
tst <- dframe$Resource_Class == "Urban"
ContVar[tst] <- rnorm(sum(tst), 10.5, 1)
dframe$ContVar <- ContVar
myvars <- c("ContVar")
mysubpops <- c("Resource_Class")
mypopsize <- data.frame(
  Resource_Class = rep(c("Agr", "Forest", "Urban"), rep(2, 3)),
  stratum = rep(c("Stratum1", "Stratum2"), 3),
  Total = c(2500, 1500, 1000, 500, 600, 450)
)
cont_cdftest(dframe,
  vars = myvars, subpops = mysubpops, siteID = "siteID",
  weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
  stratumID = "stratum", popsize = mypopsize, testname = "RaoScott_First"
)
}
\seealso{
\describe{
  \item{\code{\link{cdf_plot}}}{ for visualizing CDF plots}
  \item{\code{\link{cont_cdfplot}}}{ for making CDF plots output to pdfs}
  }
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
