### Name: relrisk
### Title: Relative Risk
### Aliases: relrisk
### Keywords: survey survival

### ** Examples

dframe <- data.frame(response=sample(c("Poor", "Good"), 100, replace=TRUE),
   stressor=sample(c("Poor", "Good"), 100, replace=TRUE),
   wgt=runif(100, 10, 100))
relrisk(dframe, vartype="SRS")

dframe$xcoord <- runif(100)
dframe$ycoord <- runif(100)
relrisk(dframe)



