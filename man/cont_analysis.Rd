% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cont_analysis.R
\name{cont_analysis}
\alias{cont_analysis}
\title{Continuous variable analysis}
\usage{
cont_analysis(
  dframe,
  vars,
  subpops = NULL,
  siteID = NULL,
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
  conf = 95,
  pctval = c(5, 10, 25, 50, 75, 90, 95),
  statistics = c("CDF", "Pct", "Mean", "Total"),
  All_Sites = FALSE
)
}
\arguments{
\item{dframe}{Data to be analyzed (analysis data). A data frame or
\code{sf} object containing survey design
variables, response variables, and subpopulation (domain) variables.}

\item{vars}{Vector composed of character values that identify the
names of response variables in \code{dframe}.}

\item{subpops}{Vector composed of character values that identify the
names of subpopulation (domain) variables in \code{dframe}.
If a value is not provided, the value \code{"All_Sites"} is assigned to the
subpops argument and a factor variable named \code{"All_Sites"} that takes
the value \code{"All Sites"} is added to the \code{dframe} data frame.  The
default value is \code{NULL}.}

\item{siteID}{Character value providing name of the site ID variable in
the \code{dframe} data frame.  For a two-stage sample, the site ID variable
identifies stage two site IDs.  The default value is \code{NULL}, which
assumes that each row in \code{dframe} represents a unique site.}

\item{weight}{Character value providing name of the design weight
variable in \code{dframe}.  For a two-stage sample, the
weight variable identifies stage two weights.  The default value is
\code{"weight"}.}

\item{xcoord}{Character value providing name of the x-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the x-coordinate
variable identifies stage two x-coordinates.  Note that x-coordinates are
required for calculation of the local mean variance estimator.  If \code{dframe}
is an \code{sf} object, this argument is not required (as the geometry column
in \code{dframe} is used to find the x-coordinate). The default
value is \code{NULL}.}

\item{ycoord}{Character value providing name of the y-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the y-coordinate
variable identifies stage two y-coordinates.  Note that y-coordinates are
required for calculation of the local mean variance estimator.  If \code{dframe}
is an \code{sf} object, this argument is not required (as the geometry column
in \code{dframe} is used to find the y-coordinate). The default
value is \code{NULL}.}

\item{stratumID}{Character value providing name of the stratum ID variable in
the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{clusterID}{Character value providing the name of the cluster
(stage one) ID variable in \code{dframe}.  Note that cluster
IDs are required for a two-stage sample.  The default value is \code{NULL}.}

\item{weight1}{Character value providing name of the stage one weight
variable in \code{dframe}.  The default value is \code{NULL}.}

\item{xcoord1}{Character value providing the name of the stage one
x-coordinate variable in \code{dframe}.  Note that x
coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{ycoord1}{Character value providing the name of the stage one
y-coordinate variable in \code{dframe}.  Note that
y-coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{sizeweight}{Logical value that indicates whether size weights should be
used during estimation, where \code{TRUE} uses size weights and
\code{FALSE} does not use size weights. To employ size weights for a
single-stage sample, a value must be supplied for argument weight.  To
employ size weights for a two-stage sample, values must be supplied for
arguments \code{weight} and \code{weight1}. The default value is \code{FALSE}.}

\item{sweight}{Character value providing the name of the size weight variable
in \code{dframe}.  For a two-stage sample, the size weight
variable identifies stage two size weights.  The default value is
\code{NULL}.}

\item{sweight1}{Character value providing name of the stage one size weight
variable in \code{dframe}.  The default value is \code{NULL}.}

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

\item{conf}{Numeric value providing the Gaussian-based confidence level.  The default value
is \code{95}.}

\item{pctval}{Vector of the set of values at which percentiles are
estimated.  The default set is: \code{c(5, 10, 25, 50, 75, 90, 95)}.}

\item{statistics}{Character vector specifying desired estimates, where
\code{"CDF"} specifies CDF estimates, \code{"Pct"} specifies percentile
estimates, \code{"Mean"} specifies mean estimates, and "Total" specifies
total estimates.  Any combination of the four choices may be provided by
the user.  The default value is \code{c("CDF", "Pct", "Mean", "Total")}.}

\item{All_Sites}{A logical variable used when \code{subpops} is not
\code{NULL}. If \code{All_Sites} is \code{TRUE}, then alongside the
subpopulation output, output for all sites (ignoring subpopulations) is
returned for each variable in \code{vars}. If \code{All_Sites} is
\code{FALSE}, then alongside the subpopulation output, output for all sites
(ignoring subpopulations) is not returned for each variable in \code{vars}.
The default is \code{FALSE}.}
}
\value{
The analysis results. A list composed of one, two, three, or four
  data frames that contain population estimates for all combinations of
  subpopulations, categories within each subpopulation, and response
  variables, where the number of data frames is determined by argument
  \code{statistics}.  The possible data frames in the output list are:
  \describe{
    \item{\code{CDF}}{: a data frame containing CDF estimates}
    \item{\code{Pct}}{: data frame containing percentile estimates}
    \item{\code{Mean}}{: a data frame containing mean estimates}
    \item{\code{Total}}{: a data frame containing total estimates}
  }
  
  The \code{CDF} data frame contains the following variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{Value}{value of response variable}
    \item{nResp}{sample size at or below \code{Value}}
    \item{Estimate.P}{CDF proportion estimate (in \%)}
    \item{StdError.P}{standard error of CDF proportion estimate}
    \item{MarginofError.P}{margin of error of CDF proportion estimate}
    \item{LCBxxPct.P}{xx\% (default 95\%) lower confidence bound of CDF proportion estimate}
    \item{UCBxxPct.P}{xx\% (default 95\%) upper confidence bound of CDF proportion estimate}
    \item{Estimate.U}{CDF total estimate}
    \item{StdError.U}{standard error of CDF total estimate}
    \item{MarginofError.U}{margin of error of CDF total estimate}
    \item{LCBxxPct.U}{xx\% (default 95\%) lower confidence bound of CDF total estimate}
    \item{UCBxxPct.U}{xx\% (default 95\%) upper confidence bound of CDF total estimate}
  }
  
  The \code{Pct} data frame contains the following variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{Statistic}{value of percentile}
    \item{nResp}{sample size at or below \code{Value}}
    \item{Estimate}{percentile estimate}
    \item{StdError}{standard error of percentile estimate}
    \item{MarginofError}{margin of error of percentile estimate}
    \item{LCBxxPct}{xx\% (default 95\%) lower confidence bound of percentile estimate}
    \item{UCBxxPct}{xx\% (default 95\%) upper confidence bound of percentile estimate}
  }
  
  The \code{Mean} data frame contains the following variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{nResp}{sample size at or below \code{Value}}
    \item{Estimate}{mean estimate}
    \item{StdError}{standard error of mean estimate}
    \item{MarginofError}{margin of error of mean estimate}
    \item{LCBxxPct}{xx\% (default 95\%) lower confidence bound of mean estimate}
    \item{UCBxxPct}{xx\% (default 95\%) upper confidence bound of mean estimate}
  }
  
  The \code{Total} data frame contains the following variables:
  \describe{
    \item{Type}{subpopulation (domain) name}
    \item{Subpopulation}{subpopulation name within a domain}
    \item{Indicator}{response variable}
    \item{nResp}{sample size at or below \code{Value}}
    \item{Estimate}{total estimate}
    \item{StdError}{standard error of total estimate}
    \item{MarginofError}{margin of error of total estimate}
    \item{LCBxxPct}{xx\% (default 95\%) lower confidence bound of total estimate}
    \item{UCBxxPct}{xx\% (default 95\%) upper confidence bound of total estimate}
  }
}
\description{
This function organizes input and output for the analysis of continuous
variables. The analysis data, \code{dframe}, can be either a data frame or a
simple features (\code{sf}) object.  If an \code{sf} object is used,
coordinates are extracted from the geometry column in the object, arguments
\code{xcoord} and \code{ycoord} are assigned values \code{"xcoord"} and
\code{"ycoord"}, respectively, and the geometry column is dropped from the
object.
}
\examples{
dframe <- data.frame(
  siteID = paste0("Site", 1:100),
  wgt = runif(100, 10, 100),
  xcoord = runif(100),
  ycoord = runif(100),
  stratum = rep(c("Stratum1", "Stratum2"), 50),
  ContVar = rnorm(100, 10, 1),
  All_Sites = rep("All Sites", 100),
  Resource_Class = rep(c("Good", "Poor"), c(55, 45))
)
myvars <- c("ContVar")
mysubpops <- c("All_Sites", "Resource_Class")
mypopsize <- data.frame(
  Resource_Class = c("Good", "Poor"),
  Total = c(4000, 1500)
)
cont_analysis(dframe,
  vars = myvars, subpops = mysubpops, siteID = "siteID",
  weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
  stratumID = "stratum", popsize = mypopsize, statistics = "Mean"
)
}
\seealso{
\describe{
  \item{\code{\link{cat_analysis}}}{ for categorical variable analysis}
  }
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
\keyword{univar}
