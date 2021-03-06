% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdf.size.total.R
\name{cdf.size.total}
\alias{cdf.size.total}
\title{Size-weighted Cumulative Distribution Funcition Estimate for Total}
\usage{
cdf.size.total(z, wgt, val, cluster.ind, cluster, wgt1, popsize, swgt,
  swgt1)
}
\arguments{
\item{z}{Vector of the response value for each site.}

\item{wgt}{Vector of the final adjusted weight (inverse of the sample
inclusion probability) for each site, which is either the weight for a
single- stage sample or the stage two weight for a two-stage sample.}

\item{val}{Vector of the set of values at which the CDF is estimated.}

\item{cluster.ind}{Logical value that indicates whether the sample is a
two- stage sample, where TRUE = a two-stage sample and FALSE = not a
two-stage sample.}

\item{cluster}{Vector of the stage one sampling unit (primary sampling unit
or cluster) code for each site.}

\item{wgt1}{Vector of the final adjusted stage one weight for each site.}

\item{popsize}{Known size of the resource, which is used to perform ratio
adjustment to estimators expressed using measurement units for the
resource.  For a finite resource, this argument is either the total number
of sampling units or the known sum of size-weights.  For an extensive
resource, this argument is the measure of the resource, i.e., either known
total length for a linear resource or known total area for an areal
resource.  For a stratified sample this variable must be a vector
containing a value for each stratum and must have the names attribute set
to identify the stratum codes.}

\item{swgt}{Vector of the size-weight for each site, which is the stage two
size-weight for a two-stage sample.}

\item{swgt1}{Vector of the stage one size-weight for each site.}
}
\value{
The size-weighted CDF estimate.
}
\description{
This function calculates an estimate of the size-weighted cumulative
distribution function (CDF) for the total of a finite resource.  The set of
values at which the CDF is estimated is supplied to the function.  If the
known sum of the size-weights of the resource is provided, the classic ratio
estimator is used to calculate the estimate. That estimator is the product of
the known sum of the size-weights of the resource and the Horvitz- Thompson
ratio estimator, where the latter is the ratio of two Horvitz- Thompson
estimators.  The numerator of the ratio estimates the size-weighted total of
the resource equal to or less than a specified value.  The denominator of the
ratio estimates the sum of the size-weights of the resource.  If the known
sum of the size-weights of the resource is not provided, the Horvitz-Thompson
estimator of the size-weighted total of the resource equal to or less than a
specified value is used to calculate the estimate.  The function can
accomodate single-stage and two-stage samples.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
