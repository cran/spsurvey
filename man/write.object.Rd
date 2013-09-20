\name{write.object}
\alias{write.object}
\title{Write an Object to a Plot}
\description{
  This function writes the contents of an object to a plot.  The object may be
  either a data frame or a matrix.  Values in the input data frame or matrix
  must be of class numeric, character, or factor.
}
\usage{
write.object(obj, n.digits=2, r.names=TRUE, c.names=TRUE, r.cex=1, c.cex=1,
   miss="NA")
}
\arguments{
  \item{obj}{the object (either a data frame or a matrix).}
  \item{n.digits}{the number of digits after the decimal point for numeric
    values.  The default is 2.}
  \item{r.names}{a logical value that indicates whether to print the row names,
    where TRUE = print the row names and  FALSE = do not print the row names.
    The default is TRUE.}
  \item{c.names}{a logical value that indicates whether to print the column
    names, where TRUE = print the column names and  FALSE = do not print the
    column names.  The default is TRUE.}
  \item{r.cex}{character expansion parameter for the row labels.  The default is
    1.}
  \item{c.cex}{character expansion parameter for the column labels.  The default
    is 1.}
  \item{miss}{the missing value code expressed as a character string.  The
    default is "NA".}
}
\value{
  The function returns NULL.  Side effect of the function is to write contents
  of the input object to a plot.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
z <- rnorm(100)
z.mean <- c(tapply(z, rep(1:4, rep(25,4)), mean), mean(z))
z.sd <- sqrt(c(tapply(z, rep(1:4, rep(25,4)), var), var(z)))
z.upper <- z.mean+1.96*z.sd
z.lower <- z.mean-1.96*z.sd
obj <- data.frame(rbind(z.mean, z.sd, z.upper, z.lower))
dimnames(obj) <- list(c("Mean Estimate", "Standard Deviation",
  "Lower 95\% Conf. Bound", "Upper 95\% Conf. Bound"), c(
  paste("Stratum", 1:4, sep=""), "AllStrata"))
write.object(obj, n.digits=3, r.cex=0.75)

obj <- data.frame(matrix(round(5 + runif(30), 1), nrow=6))
colnames(obj) <- c("United.States", "Russia", "Germany",
  "Japan", "France")
write.object(obj, n.digits=1, r.names=FALSE)
}
\keyword{aplot}
