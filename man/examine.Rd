\name{examine}
\alias{examine}
\title{Examine Data Frame Variables}
\description{
  This function examines variables in a data frame by printing either a table
  or the results of a call to the describe function in the Hmisc package.
}
\usage{
examine(dframe, subpop = NULL, ord = TRUE, cmax = 50)
}
\arguments{
  \item{dframe}{a data frame.}
  \item{subpop}{a character string identifying a variable in dframe that is used
    to group output.  A separate table or call to the describe function is
    printed for each unique value in the variable.  The default value is NULL.}
  \item{ord}{a logical value that controls the order in which the variables in
    dframe are processed.  TRUE mean that variables are processed in sorted
    order.  FALSE means that variables are processed in the order in which they
    occur in dframe.  The default value is TRUE.}
  \item{cmax}{a numeric value that controls whether a call to table or a call to
    describe is used to process variables in dframe.  If the number of unique
    values in a variable is less than or equal to cmax, then table is called.
    If the number of unique values in a variable is greater than cmax, then
    describe is called.  The default value is 50.}
}
\section{Value}{
  Tables and/or the output from calls to describe are printed.  The function
  returns NULL invisibly.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
df <- data.frame(SiteID = 1:100,
   Catvar = sample(LETTERS[1:5], 100, TRUE),
   Contvar = rnorm(100, 10, 1),
   Gender = rep(c("Male", "Female"), rep(50, 2)))
examine(df, ord = FALSE)
examine(df, "Gender", FALSE)
}
\keyword{data frame}
