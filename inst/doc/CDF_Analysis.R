## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey------------------------------------------------------------
library(spsurvey)
library(sf)

## ----load_FL_lakes------------------------------------------------------------
data(FL_lakes)
nr <- nrow(FL_lakes)

## ----head_FL_lakes------------------------------------------------------------
head(FL_lakes)

## ----tail_FL_lakes------------------------------------------------------------
tail(FL_lakes)

## ----summary_do---------------------------------------------------------------
cat("\nSummarize the data structure of the dissolved oxygen variable:\n")
summary(FL_lakes$Oxygen)

## ----create_sites-------------------------------------------------------------
sites <- data.frame(siteID=FL_lakes$siteID,
                    Use=FL_lakes$Status == "Sampled")

## ----create_subpop------------------------------------------------------------
subpop <- data.frame(siteID=FL_lakes$siteID,
                     Basin=FL_lakes$Basin)

## ----create_design------------------------------------------------------------
design <- data.frame(siteID=FL_lakes$siteID,
                     wgt=FL_lakes$wgt,
                     xcoord=FL_lakes$xcoord,
                     ycoord=FL_lakes$ycoord)

## ----create_data.cont---------------------------------------------------------
data.cont <- data.frame(siteID=FL_lakes$siteID,
                        DissolvedOxygen=FL_lakes$Oxygen)

## ----framesize----------------------------------------------------------------
framesize <- c("NWFWMD-1"=451, "NWFWMD-2"=394, "SFWMD-9"=834, "SJRWMD-1"=1216,
               "SRWMD-1"=1400, "SWFWMD-4"=851)

## ----CDF----------------------------------------------------------------------
CDF_Estimates <- cont.analysis(sites, subpop, design, data.cont,
   popsize=list(Basin=as.list(framesize)))

## ----write_cdf----------------------------------------------------------------
write.csv(CDF_Estimates$CDF, file="CDF_Estimates.csv", row.names=FALSE)

## ----cdfplot------------------------------------------------------------------
cont.cdfplot("CDF_Estimates.pdf", CDF_Estimates$CDF)

## ----cdf_test-----------------------------------------------------------------
CDF_Tests <- cont.cdftest(sites, subpop, design, data.cont,
   popsize=list(Basin=as.list(framesize)))

## ----print_cdf_tests----------------------------------------------------------
print(CDF_Tests, digits=3)

## ----cdf_figure, fig.cap="Florida Basins with Significantly Different CDFs."----
n1 <- length(levels(CDF_Tests$Subpopulation_1))
n2 <- length(levels(CDF_Tests$Subpopulation_2))
plot(1:n2, 1:n1, type="n", xlab="Second Basin", ylab="First Basin", xaxt="n",
     yaxt="n")
count=1
for(i in 1:n1) {
   for(j in i:n2) {
      text(j, i, ifelse(CDF_Tests$p_Value[count] < 0.01, "X", " "))
      count <- count+1
   }
}
axis(side=1, at=1:n2, labels=levels(CDF_Tests$Subpopulation_2), cex.axis=0.75)
axis(side=2, at=1:n1, labels=levels(CDF_Tests$Subpopulation_1), cex.axis=0.75)
title("Significantly Different CDFs")
abline(1, 1, col="red", lwd=2)

## ----write_cdf_tests----------------------------------------------------------
write.csv(CDF_Tests, file="CDF_Tests.csv", row.names=FALSE)

