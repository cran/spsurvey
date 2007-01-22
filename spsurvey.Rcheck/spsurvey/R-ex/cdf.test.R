### Name: cdf.test
### Title: Cumulative Distribution Function - Inference
### Aliases: cdf.test
### Keywords: survey distribution

### ** Examples

z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
sample1 <- list(z=z, wgt=wgt)
sample2 <- list(z=z+2, wgt=wgt)
bounds <- seq(min(sample1$z, sample2$z), max(sample1$z,
   sample2$z), length=4)[-1]
cdf.test(sample1, sample2, bounds, vartype="SRS")

x <- runif(100)
y <- runif(100)
sample1 <- list(z=z, wgt=wgt, x=x, y=y)
sample2 <- list(z=z+rnorm(100), wgt=wgt, x=x, y=y)
bounds <- seq(min(sample1$z, sample2$z), max(sample1$z,
   sample2$z), length=4)[-1]
cdf.test(sample1, sample2, bounds)



