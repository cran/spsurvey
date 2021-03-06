% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warnprnt.R
\name{warnprnt}
\alias{warnprnt}
\title{Internal Function: Print the Warnings Data Frame}
\usage{
warnprnt(warn.df = get("warn.df", envir = .GlobalEnv),
  m = 1:nrow(warn.df))
}
\arguments{
\item{warn.df}{Data frame that contains warning messages.  The default is
"warn.df", which is the name given to the warnings data frame created by
functions in the spsurvey package.}

\item{m}{Vector of indices for warning messages that are to be printed. The
default is a vector containing the integers from 1 through the number of
rows in warn.df, which will print all warning messages in the data frame.}
}
\value{
Invisible return.  Prints warnings.
}
\description{
This function prints the warnings data frame.
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
