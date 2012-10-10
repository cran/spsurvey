\name{cdf.plot}
\alias{cdf.plot}
\title{Plot the CDF and Associated Confidence Limits}
\description{
  This function creates a CDF plot.  Input data for the plots is provided by a
  data frame utilizing the same structure as the data frame named "CDF" that
  is included in the output object produced by function cont.analysis, but the
  data frame includes only the values for a single CDF.  Confidence limits for
  the CDF also are plotted.
}
\usage{
cdf.plot(cdfest, units.cdf="Percent", type.cdf="Continuous", logx="", xlbl=NULL,
   ylbl="Percent", ylbl.r=NULL, figlab=NULL, legloc="BR", confcut=5, conflev=95, ...)
}
\arguments{
  \item{cdfest}{data frame utilizing the same structure as the data frame named
    "CDF" that is included in the output object produced by function
    cont.analysis.  The data frame must contain only a single cdf estimate.}
  \item{units.cdf}{indicator for the type of units in which the CDF is plotted,
    where "Percent" means the plot is in terms of percent of the population,
    and "Units" means the plot is in terms of units of the population.  The
    default is "Percent".}
  \item{type.cdf}{character string consisting of the value "Continuous" or
    "Ordinal" that controls the type of CDF plot for each indicator.  The
    default is "Continuous".}
  \item{logx}{character string consisting of the value "" or "x" that controls
    whether the x axis uses the original scale ("") or the base 10 logarithmic
    scale ("x").  The default is "".}
  \item{xlbl}{character string providing the x-axis label.  If this argument
    equals NULL, then the indicator name is used as the label.  The default is
    NULL.}
  \item{ylbl}{character string providing the the y-axis label.  The default is 
    "Percent".}
  \item{ylbl.r}{character string providing the label for the right side y-axis, 
    where NULL means a label is not created, and "Same" means the label is the 
    same as the left side label (i.e., argument ylbl).  The default is NULL.}
  \item{figlab}{character string providing the plot title.  The default is 
    NULL.}
  \item{legloc}{indicator for location of the plot legend, where "BR" means 
    bottom
    right, "BL" means bottom left, "TR" means top right, and "TL" means top
    left.  The default is "BR".}
  \item{confcut}{numeric value that controls plotting confidence limits at the 
    CDF extremes.  Confidence limits for CDF values (percent scale) less than 
    confcut or greater than 100 minus confcut are not plotted.  A value of 
    zero means confidence limits are plotted for the complete range of the 
    CDF.  The default is 5.}
  \item{conflev}{numeric value of the confidence level used for confidence 
    limits.  The default is 95.}
  \item{\dots}{additional arguments passed to the plot function.}
}
\value{
  A plot of the CDF and its associated confidence limits.
}
\references{
  Diaz-Ramos, S., D.L. Stevens, Jr., and A.R. Olsen. (1996).  \emph{EMAP
  Statistical Methods Manual.} EPA/620/R-96/XXX.  Corvallis, OR: U.S.
  Environmental Protection Agency, Office of Research and Development, National
  Health Effects and Environmental Research Laboratory, Western Ecology
  Division.
}
\author{
Tony Olsen \email{Olsen.Tony@epa.gov}\cr
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\seealso{
\code{\link{interp.cdf}, \link{interp.axis}} 
}
\examples{
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites",100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10, 100),
   xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
   "Stratum2"), 50))
ContVar <- rnorm(100, 10, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar)
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))
myanalysis <- cont.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cont=mydata.cont, popsize=mypopsize)
par(mfrow=c(2,1))       
keep <- myanalysis$CDF$Type == "Resource.Class" & 
        myanalysis$CDF$Subpopulation == "Good"       
cdf.plot(myanalysis$CDF[keep,], xlbl="ContVar", ylbl="Percent of Stream Length",
   ylbl.r="Stream Length (km)", figlab="Estimates for Resource Class: Good")
keep <- myanalysis$CDF$Type == "Resource.Class" & 
        myanalysis$CDF$Subpopulation == "Poor"       
cdf.plot(myanalysis$CDF[keep,], xlbl="ContVar", ylbl="Percent of Stream Length",
   ylbl.r="Same", figlab="Estimates for Resource Class: Poor")
}
\keyword{survey}
\keyword{distribution}
