## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----survey-data---------------------------------------------------------
data(SC_estuaries)
nr <- nrow(SC_estuaries)

## ----head-survey-data----------------------------------------------------
head(SC_estuaries)

## ----tail-survey-data----------------------------------------------------
tail(SC_estuaries)

## ----addmargins----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the status
variable follows:\n")
addmargins(table(SC_estuaries$Status))

## ----sites2--------------------------------------------------------------
sites <- data.frame(siteID=SC_estuaries$siteID,
                    Use=rep(TRUE, nr))

## ----subpop--------------------------------------------------------------
subpop <- data.frame(siteID=SC_estuaries$siteID,
                     All_Estuaries=rep("All Estuaries", nr),
							       Estuary_Type=SC_estuaries$Stratum)

## ----design--------------------------------------------------------------
design <- data.frame(siteID=SC_estuaries$siteID,
                     wgt=SC_estuaries$wgt,
                     xcoord=SC_estuaries$xcoord,
                     ycoord=SC_estuaries$ycoord)

## ----data.cat------------------------------------------------------------
data.cat <- data.frame(siteID=SC_estuaries$siteID,
                       Status=SC_estuaries$Status)

## ----extent.estimates----------------------------------------------------
Extent_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----print.extent.estimates----------------------------------------------
print(Extent_Estimates)

## ----write.extent.estimates----------------------------------------------
write.csv(Extent_Estimates, file="Extent_Estimates.csv")

## ----addmargins2---------------------------------------------------------
cat("\nA table displaying the number of values for each level of the IBI status
variable follows:\n")
addmargins(table(SC_estuaries$IBI_status))

## ----addmargins3---------------------------------------------------------
cat("\nA table displaying the number of values for each level of the WQ status variable follows:\n")
addmargins(table(SC_estuaries$WQ_status))

## ----sites---------------------------------------------------------------
sites <- data.frame(siteID=SC_estuaries$siteID,
                    Use=SC_estuaries$Status == "Sampled")

## ----data.cat2-----------------------------------------------------------
data.cat <- data.frame(siteID=SC_estuaries$siteID,
                       IBI_Status=SC_estuaries$IBI_status,
                       WQ_Status=SC_estuaries$WQ_status)

## ----cond.est------------------------------------------------------------
Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----print.est-----------------------------------------------------------
print(Condition_Estimates[c(1:4, 13:16),])

## ----write.csv-----------------------------------------------------------
write.csv(Condition_Estimates, file="Condition_Estimates.csv")

## ----framesize-----------------------------------------------------------
framesize <- c("Open Water"=628.509298, "Tidal Creek"=105.829522)

## ----est.estimates-------------------------------------------------------
Condition_Estimates_popsize <- cat.analysis(sites, subpop, design, data.cat,
   popsize=list(All_Estuaries=sum(framesize),
                Estuary_Type=as.list(framesize)))

## ----print.est.estimates-------------------------------------------------
print(Condition_Estimates_popsize[c(1:4, 13:16),])

## ----write.est.estimates-------------------------------------------------
write.csv(Condition_Estimates_popsize, file="Condition_Estimates_popsize.csv")

## ----summary.IBI---------------------------------------------------------
cat("\nSummarize the data structure of the IBI score variable:\n")
summary(SC_estuaries$IBI_score)

## ----summary.WQ----------------------------------------------------------
cat("\nSummarize the data structure of the WQ score variable:\n")
summary(SC_estuaries$WQ_score)

## ----data.cont-----------------------------------------------------------
data.cont <- data.frame(siteID=SC_estuaries$siteID,
                        IBI_Score=SC_estuaries$IBI_score,
                        WQ_Score=SC_estuaries$WQ_score)

## ----cdf-----------------------------------------------------------------
CDF_Estimates <- cont.analysis(sites, subpop, design, data.cont,
   popsize=list(All_Estuaries=sum(framesize),
                Estuary_Type=as.list(framesize)))

## ----write.cdf-----------------------------------------------------------
write.csv(CDF_Estimates$CDF, file="CDF_Estimates.csv")

## ----cdf.pdf-------------------------------------------------------------
cont.cdfplot("CDF_Estimates.pdf", CDF_Estimates$CDF)

## ----print.cdf.est-------------------------------------------------------
print(CDF_Estimates$Pct[1:10,])

## ----write.cdf.est-------------------------------------------------------
write.csv(CDF_Estimates$Pct, file="Percentile_Estimates.csv", row.names=FALSE)

## ----cdf.test------------------------------------------------------------
CDF_Tests <- cont.cdftest(sites, subpop[,c(1,3)], design, data.cont,
   popsize=list(Estuary_Type=as.list(framesize)))

## ----print.IBI.WQ--------------------------------------------------------
print(CDF_Tests, digits=3)

## ----write.CDF.tests-----------------------------------------------------
write.csv(CDF_Tests, file="CDF_Tests.csv", row.names=FALSE)

