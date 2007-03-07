### Name: cdf.test
### Title: Cumulative Distribution Function - Inference
### Aliases: cdf.test
### Keywords: survey distribution

### ** Examples

resp <- rnorm(100, 10, 1)
wgt <- runif(100, 40, 60)
sample1 <- list(z=resp, wgt=wgt)
sample2 <- list(z=resp+0.5, wgt=wgt)
bounds <- sort(c(sample1$z, sample2$z))[floor(seq(200/3, 200, length=3))]
cdf.test(sample1, sample2, bounds, vartype="SRS")

xcoord <- runif(100)
ycoord <- runif(100)
sample1 <- list(z=resp, wgt=wgt, x=xcoord, y=ycoord)
sample2 <- list(z=1.05*resp, wgt=wgt, x=xcoord, y=ycoord)
cdf.test(sample1, sample2, bounds)



