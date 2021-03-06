% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdf.test.prop.R
\name{cdf.test.prop}
\alias{cdf.test.prop}
\title{Estimate of Population Proportion for Classes}
\usage{
cdf.test.prop(z, wgt, bounds, cluster.ind, cluster, wgt1)
}
\arguments{
\item{z}{Vector of the response value for each site.}

\item{wgt}{Vector of the final adjusted weight (inverse of the sample
inclusion probability) for each site, which is either the weight for a
single-stage sample or the stage two weight for a two-stage sample.}

\item{bounds}{Upper bounds for calculating classes for the CDF.}

\item{cluster.ind}{Logical value that indicates whether the sample is a
two- stage sample, where TRUE = a two-stage sample and FALSE = not a
two-stage sample.}

\item{cluster}{Vector of the stage one sampling unit (primary sampling unit
or cluster) code for each site.}

\item{wgt1}{Vector of the final adjusted stage one weight for each site.}
}
\value{
The class proportion estimates.
}
\description{
This function calculates an estimate of the population proportion in a set
of intervals (classes).  The set of values defining the upper bound of each
class is supplied to the function.  The Horvitz-Thompson ratio estimator,
i.e., the ratio of two Horvitz-Thompson estimators, is used to calculate the
estimate.  The numerator of the ratio estimates the total of the resource
within a class.  The denominator of the ratio estimates the size of the
resource.  For a finite resource size is the number of units in the resource.
For an extensive resource size is the extent (measure) of the resource, i.e.,
length, area, or volume.  The function can accomodate single stage and
two-stage samples.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
