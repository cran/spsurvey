## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey------------------------------------------------------------
library(spsurvey)

## ----load_decon---------------------------------------------------------------
data(decon_data)
nr <- nrow(decon_data)

## ----head_decon---------------------------------------------------------------
head(decon_data)

## ----tail_decon---------------------------------------------------------------
tail(decon_data)

## ----summary_decon------------------------------------------------------------
cat("\nSummarize the data structure of the species richness variable:\n")
summary(decon_data$Richness)

## ----summarize_decon_richness-------------------------------------------------
cat("\nSummarize the data structure of the species richness variable plus \n100% measurrement error:\n")
summary(decon_data$Richness_100)

## ----cdfvals------------------------------------------------------------------
cdfvals <- seq(0,40,length=25)

## ----cdf_richness-------------------------------------------------------------
CDF_org <- cdf.est(z=decon_data$Richness,
                   wgt=rep(1, nr),
                   x=decon_data$xcoord,
                   y=decon_data$ycoord,
                   cdfval=cdfvals)

## ----cdf_ests-----------------------------------------------------------------
# Calculate a CDF estimate for the variable plus 25% extraneous variance
CDF_25 <- cdf.est(z=decon_data$Richness_25,
                  wgt=rep(1, nrow(decon_data)),
                  x=decon_data$xcoord,
                  y=decon_data$ycoord,
                  cdfval=cdfvals)

# Calculate a CDF estimate for the variable plus 50% extraneous variance
CDF_50 <- cdf.est(z=decon_data$Richness_50,
                  wgt=rep(1, nrow(decon_data)),
                  x=decon_data$xcoord,
                  y=decon_data$ycoord,
                  cdfval=cdfvals)

# Calculate a CDF estimate for the variable plus 100% extraneous variance
CDF_100 <- cdf.est(z=decon_data$Richness_100,
                   wgt=rep(1, nrow(decon_data)),
                   x=decon_data$xcoord,
                   y=decon_data$ycoord,
                   cdfval=cdfvals)

## ----dens_ests----------------------------------------------------------------
Density_org <- ash1.wgt(decon_data$Richness, nbin=25)
Density_25 <- ash1.wgt(decon_data$Richness_25, nbin=25)
Density_50 <- ash1.wgt(decon_data$Richness_50, nbin=25)
Density_100 <- ash1.wgt(decon_data$Richness_100, nbin=25)

## ----cdf_dens_est, fig.cap="CDF and Density Estimates for Species Richness Variables."----
op <- par(mfrow=c(2,1), mgp=c(2.2,0.6,0), mar=c(3,3,2,0)+0.1)

plot(CDF_org$CDF$Value, CDF_org$CDF$Estimate.P, type="l", lwd=2, col="red",
     xlim=c(0, 40), ylim=c(0, 100), xaxt="n", xlab="", ylab="Percent")
title(main="CDF Estimates")
axis(side=1, at=c(0, 5, 10, 15, 20, 25, 30, 35, 40),
     labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40"))
mtext("Species Richness", side=1, line=1.75, cex=par("cex"))
lines(CDF_25$CDF$Value, CDF_25$CDF$Estimate.P, lwd=2, col="blue")
lines(CDF_50$CDF$Value, CDF_50$CDF$Estimate.P, lty=5, lwd=2.5,
      col="blue")
lines(CDF_100$CDF$Value, CDF_100$CDF$Estimate.P, lty=4, lwd=2.5,
      col="blue")
legend(x="bottomright", inset=0.025, legend=c("Original Variable",
      "25% Added Variance", "50% Added Variance",
      "100% Added Variance"), lty=c(1,1,5,4), lwd=c(2,2,2.5, 2.5),
      bty="o", cex=1, col=c("red", "blue", "blue", "blue"))

plot(Density_org$x, Density_org$y, type="l", lwd=2, col="red",
     xlim=c(0, 40), ylim=c(0, 0.075), xaxt="n", xlab="",
     ylab="Resource Density")
title(main="Density Estimates")
axis(side=1, at=c(0, 5, 10, 15, 20, 25, 30, 35, 40),
     labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40"))
mtext("Species Richness", side=1, line=1.75, cex=par("cex"))
lines(Density_25$x, Density_25$y, lwd=2, col="blue")
lines(Density_50$x, Density_50$y, lty=5, lwd=2.5, col="blue")
lines(Density_100$x, Density_100$y, lty=4, lwd=2.5, col="blue")

par(op)

## ----decon_est----------------------------------------------------------------
extvar <- var(decon_data$Richness_100) - var(decon_data$Richness)
CDF_decon <- cdf.decon(z=decon_data$Richness_100,
                       wgt=rep(1,nr),
                       sigma=extvar,
                       x=decon_data$xcoord,
                       y=decon_data$ycoord,
                       cdfval=cdfvals)

## ----deconvolution, fig.cap="Original and Deconvoluted CDF Estimates for a Species Richness Variable with Added Measurement Error."----
op <- par(mgp=c(2.2,0.6,0), mar=c(3,3,1,0)+0.1)

plot(CDF_100$CDF$Value, CDF_100$CDF$Estimate.P, type="l", lwd=2, col="red",
   xlim=c(0, 40), xaxt="n", xlab="Species Richness", ylab="Percent")
axis(side=1, at=c(0, 5, 10, 15, 20, 25, 30, 35, 40),
     labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40"))
confcut <- 5
pctval <- c(confcut, 100-confcut)
tvalue <- CDF_100$CDF$Estimate.P >= pctval[1] &
          CDF_100$CDF$Estimate.P <= pctval[2]
x <-  interp.cdf(pctval, CDF_100$CDF$Estimate.P, CDF_100$CDF$Value)
ylow <- interp.cdf(pctval, CDF_100$CDF$Estimate.P, CDF_100$CDF$LCB95Pct.P)
yhi <- interp.cdf(pctval, CDF_100$CDF$Estimate.P, CDF_100$CDF$UCB95Pct.P)
value <- c(x[1], CDF_100$CDF$Value[tvalue], x[2])
lower <- c(ylow[1], CDF_100$CDF$LCB95Pct.P[tvalue], ylow[2])
upper <- c(yhi[1], CDF_100$CDF$UCB95Pct.P[tvalue], yhi[2])
lines(value, lower, lty=3, lwd=2.5, col="red")
lines(value, upper, lty=3, lwd=2.5, col="red")
lines(CDF_decon$CDF$Value, CDF_decon$CDF$Estimate.P, lwd=2, col="blue")
tvalue <- CDF_decon$CDF$Estimate.P >= pctval[1] &
          CDF_decon$CDF$Estimate.P <= pctval[2]
x <-  interp.cdf(pctval, CDF_decon$CDF$Estimate.P, CDF_decon$CDF$Value)
ylow <- interp.cdf(pctval, CDF_decon$CDF$Estimate.P, CDF_decon$CDF$LCB95Pct.P)
yhi <- interp.cdf(pctval, CDF_decon$CDF$Estimate.P, CDF_decon$CDF$UCB95Pct.P)
value <- c(x[1], CDF_decon$CDF$Value[tvalue], x[2])
lower <- c(ylow[1], CDF_decon$CDF$LCB95Pct.P[tvalue], ylow[2])
upper <- c(yhi[1], CDF_decon$CDF$UCB95Pct.P[tvalue], yhi[2])
lines(value, lower, lty=3, lwd=2.5, col="blue")
lines(value, upper, lty=3, lwd=2.5, col="blue")
legend(x="bottomright", inset=0.05, legend=c("Original CDF",
       "Confidence Bounds", "Deconvoluted CDF", "Confidence Bounds"),
       lty=c(1,3,1,3), lwd=c(2,2.5,2,2.5), bty="o", cex=1,
       col=c("red", "red", "blue", "blue"))

par(op)

