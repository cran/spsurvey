% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_summary.R
\name{sp_summary}
\alias{sp_summary}
\alias{sp_summary.default}
\alias{sp_summary.sp_design}
\title{Summarize sampling frames, design sites, and analysis data.}
\usage{
sp_summary(object, ...)

\method{sp_summary}{default}(object, formula = ~1, onlyshow = NULL, ...)

\method{sp_summary}{sp_design}(object, formula = ~siteuse, siteuse = NULL, onlyshow = NULL, ...)
}
\arguments{
\item{object}{An object to summarize. When summarizing sampling frames,
an \code{sf}
object. When summarizing design sites, an object created by \code{grts()} or
\code{irs()} (which has class \code{sp_design}). When summarizing analysis data,
a data frame or an \code{sf} object.}

\item{...}{Additional arguments to pass to \code{sp_summary()}. If the left-hand
side of the formula is empty, the appropriate generic arguments are passed
to \code{summary.data.frame}. If the left-hand side of the formula is provided,
the appropriate generic arguments are passed to \code{summary.default}.}

\item{formula}{A formula. One-sided formulas are used to summarize the
distribution of numeric or categorical variables. For one-sided formulas,
variable names are placed to the right of \code{~} (a right-hand side variable).
Two sided formulas are
used to summarize the distribution of a left-hand side variable
for each level of each right-hand side categorical variable in the formula.
Note that only for two-sided formulas are numeric right-hand side variables
coerced to a categorical variables. If an intercept
is included as a right-hand side variable (whether the formula is one-sided or
two-sided), the total will also be summarized. When summarizing sampling frames
or analysis data, the default formula is \code{~ 1}. When summarizing design sites,
\code{siteuse} should be used in the formula, and the default formula is
\code{~ siteuse}.}

\item{onlyshow}{A string indicating the single level of the single right-hand side
variable for which a summary is requested. This argument is only used when
a single right-hand side variable is provided.}

\item{siteuse}{A character vector indicating the design sites
for which summaries are requested in \code{object}. Defaults to computing summaries for
each non-\code{NULL} \code{sites_*} list in \code{object}.}
}
\value{
If the left-hand side of the formula is empty, a named list
containing summaries of the count distribution for each right-hand side
variable is returned. If the left-hand side of the formula contains a
variable, a named list containing five number
summaries (numeric left-hand side) or tables (categorical or factor left
hand side) is returned for each right-hand side variable.
}
\description{
\code{sp_summary()} summarizes sampling frames, design sites, and analysis data. The right-hand of the
formula specifies the variables (or factors) to
summarize by. If the left-hand side of the formula is empty, the
summary will be of the distributions of the right-hand side variables. If the left-hand side
of the formula contains a variable, the summary will be of the left-hand size variable
for each level of each right-hand side variable. Equivalent to \code{spsurvey::summary()}; both
are currently maintained for backwards compatibility.
}
\examples{
\dontrun{
data("NE_Lakes")
sp_summary(NE_Lakes, ELEV ~ 1)
sp_summary(NE_Lakes, ~ ELEV_CAT * AREA_CAT)
sample <- grts(NE_Lakes, 100)
sp_summary(sample, ~ ELEV_CAT * AREA_CAT)
}
}
\author{
Michael Dumelle \email{Dumelle.Michael@epa.gov}
}
