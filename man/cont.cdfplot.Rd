\name{cont.cdfplot}
\alias{cont.cdfplot}
\title{Create CDF Plots from the cont.analysis Output Object}
\description{
  This function creates CDF plots.  Input data for the plots is provided by a
  data frame utilizing the same structure as the data frame named "CDF" that
  is included in the output object produced by function cont.analysis.  Plots
  are produced for every combination of Type of population, subpopulation
  within Type, and indicator.  Output from the function is placed in a PDF
  file.
}
\usage{
cont.cdfplot(pdffile="cdf2x2.pdf", cdfest, units.cdf="Percent",
   ind.type=rep("Continuous", nind), logx=rep("", nind), xlbl=NULL,
   ylbl="Percent", ylbl.r=NULL, legloc="BR", cdf.page=4, width=10, height=8,
   confcut=5, ...)
}
\arguments{
  \item{pdffile}{name of the PDF file.  The default is "cdf2x2.pdf".}
  \item{cdfest}{data frame utilizing the same structure as the data frame named
    "CDF" that is included in the output object produced by function
    cont.analysis.}
  \item{units.cdf}{indicator for the type of units in which the CDF is plotted,
    where "Percent" means the plot is in terms of percent of the population,
    and "Units" means the plot is in terms of units of the population.  The
    default is "Percent".}
  \item{ind.type}{character vector consisting of the values "Continuous" or
    "Ordinal" that controls the type of CDF plot for each indicator.  The
    default is "Continuous" for every indicator.}
  \item{logx}{character vector consisting of the values "" or "x" that controls
    whether the x axis uses the original scale ("") or the base 10 logarithmic
    scale ("x") for each indicator.  The default is "" for every indicator.}
  \item{xlbl}{character vector consisting of the x-axis label for each 
    indicator.  If this argument equals NULL, then indicator names are used as 
    the labels.  The default is NULL.}
  \item{ylbl}{character string providing the the y-axis label.  The default is 
    "Percent".}
  \item{ylbl.r}{character string providing the label for the right side y-axis, 
    where NULL means a label is not created, and "Same" means the label is the 
    same as the left side label (i.e., argument ylbl).  The default is NULL.}
  \item{legloc}{indicator for location of the plot legend, where "BR" means 
    bottom right, "BL" means bottom left, "TR" means top right, and "TL" means 
    top left.  The default is "BR".}
  \item{cdf.page}{number of CDF plots on each page, which must be chosen from 
    the values: 1, 2, 4, or 6.  The default is 4.}
  \item{width}{width of the graphic region in inches.  The default is 10.}
  \item{height}{height of the graphic region in inches.  The default is 8.}
  \item{confcut}{numeric value that controls plotting confidence limits at the 
    CDF extremes.  Confidence limits for CDF values (percent scale) less than 
    confcut or greater than 100 minus confcut are not plotted.  A value of 
    zero means confidence limits are plotted for the complete range of the 
    CDF.  The default is 5.}
  \item{\dots}{additional arguments passed to the cdf.plot function.}
}
\value{
  A PDF file containing the CDF plots.
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
\code{\link{cdf.plot}} 
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
cont.cdfplot("myanalysis.pdf", myanalysis$CDF, ylbl.r="Stream Length (km)")
}
\keyword{survey}
\keyword{distribution}
