## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----load_INstreams------------------------------------------------------
data(IN_streams)
nr <- nrow(IN_streams)

## ----head_INstreams------------------------------------------------------
head(IN_streams)

## ----tail_INstreams------------------------------------------------------
tail(IN_streams)

## ----stat_table----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the status
variable follows:\n")
addmargins(table(IN_streams$Status))

## ----tnt_table-----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the TNT
variable follows:\n")
addmargins(table(IN_streams$TNT))

## ----create_sites--------------------------------------------------------
sites <- data.frame(siteID=IN_streams$siteID,
                    Use=rep(TRUE, nr))

## ----create_subpop-------------------------------------------------------
subpop <- data.frame(siteID=IN_streams$siteID,
                     Upper_Wabash=rep("Upper Wabash", nr), 
							       Strahler_Order=IN_streams$Strahler_Cat)

## ----create_design-------------------------------------------------------
design <- data.frame(siteID=IN_streams$siteID,
                     wgt=IN_streams$wgt,
                     xcoord=IN_streams$xcoord,
                     ycoord=IN_streams$ycoord)

## ----create_data.cat-----------------------------------------------------
data.cat <- data.frame(siteID=IN_streams$siteID,
                       Status=IN_streams$Status,
                       Target_NonTarget=IN_streams$TNT)

## ----cat.analysis--------------------------------------------------------
Extent_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----all_status----------------------------------------------------------
print(Extent_Estimates[c(1:8, 32:34),])

## ----write_ext-----------------------------------------------------------
write.csv(Extent_Estimates, file="Extent_Estimates.csv", row.names=FALSE)

## ----table_IBI-----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the IBI status
variable follows:\n")
addmargins(table(IN_streams$IBI_Status))

## ----table_QHEI----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the QHEI status
variable follows:\n")
addmargins(table(IN_streams$QHEI_Status))

## ----create_sites2-------------------------------------------------------
sites <- data.frame(siteID=IN_streams$siteID,
                    Use=IN_streams$Status == "Sampled")

## ----create_data.cat2----------------------------------------------------
data.cat <- data.frame(siteID=IN_streams$siteID,
                       IBI_Status=IN_streams$IBI_Status,
                       QHEI_Status=IN_streams$QHEI_Status)

## ----cond_ests-----------------------------------------------------------
Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----print_cond_all------------------------------------------------------
print(Condition_Estimates[c(1:3, 16:18),])

## ----write_cond_all------------------------------------------------------
write.csv(Condition_Estimates, file="Condition_Estimates.csv", row.names=FALSE)

## ----framesize-----------------------------------------------------------
framesize <- c("1st"=4514.450, "2nd"=1443.260, "3rd"=740.146, "4th"=660.294)

## ----stream_con----------------------------------------------------------
Condition_Estimates_popsize <- cat.analysis(sites, subpop, design, data.cat,
   popsize=list(Upper_Wabash=sum(framesize),
                Strahler_Order=as.list(framesize)))

## ----print_streamcon-----------------------------------------------------
print(Condition_Estimates_popsize[c(1:3, 16:18),])

## ----write_streamcon-----------------------------------------------------
write.csv(Condition_Estimates_popsize, file="Condition_Estimates_popsize.csv",
   row.names=FALSE)

## ----summarize_IBI-------------------------------------------------------
cat("\nSummarize the data structure of the IBI score variable:\n")
summary(IN_streams$IBI_Score)

## ----summarize_QHEI------------------------------------------------------
cat("\nSummarize the data structure of the QHEI score variable:\n")
summary(IN_streams$QHEI_Score)

## ----create_data.cont2---------------------------------------------------
data.cont <- data.frame(siteID=IN_streams$siteID,
                        IBI_Score=IN_streams$IBI_Score,
                        QHEI_Score=IN_streams$QHEI_Score)

## ----cdf_percentiles-----------------------------------------------------
CDF_Estimates <- cont.analysis(sites, subpop, design, data.cont,
   popsize=list(Upper_Wabash=sum(framesize),
                Strahler_Order=as.list(framesize)))

## ----write.cdf.ests------------------------------------------------------
write.csv(CDF_Estimates$CDF, file="CDF_Estimates.csv", row.names=FALSE)

## ----cdfplot-------------------------------------------------------------
cont.cdfplot("CDF_Estimates.pdf", CDF_Estimates$CDF)

## ----print_cdf_ests------------------------------------------------------
print(CDF_Estimates$Pct[1:10,])

## ----write.cdf.ests2-----------------------------------------------------
write.csv(CDF_Estimates$Pct, file="Percentile_Estimates.csv", row.names=FALSE)

## ----test_diffs----------------------------------------------------------
CDF_Tests <- cont.cdftest(sites, subpop[,c(1,3)], design, data.cont,
   popsize=list(Strahler_Order=as.list(framesize)))

## ----cdf_str_diff--------------------------------------------------------
print(CDF_Tests, digits=2)

## ----write_cdf_tests-----------------------------------------------------
write.csv(CDF_Tests, file="CDF_Tests.csv", row.names=FALSE)

