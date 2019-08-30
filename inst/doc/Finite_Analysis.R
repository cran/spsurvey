## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----load_FL_lakes-------------------------------------------------------
data(FL_lakes)
nr <- nrow(FL_lakes)

## ----head_FL_lakes-------------------------------------------------------
head(FL_lakes)

## ----tail_FL_lakes-------------------------------------------------------
tail(FL_lakes)

## ----table_status--------------------------------------------------------
cat("\nA table displaying the number of values for each level of the status
variable follows:\n")
addmargins(table(FL_lakes$Status))

## ----table_tnt-----------------------------------------------------------
cat("\nA table displaying the number of values for each level of the TNT
variable follows:\n")
addmargins(table(FL_lakes$TNT))

## ----create_sites--------------------------------------------------------
sites <- data.frame(siteID=FL_lakes$siteID,
                    Use=rep(TRUE, nr))

## ----create_subpop-------------------------------------------------------
subpop <- data.frame(siteID=FL_lakes$siteID,
                     CombinedBasins=rep("All Basins", nr), 
							       Basin=FL_lakes$Basin)

## ----create_design-------------------------------------------------------
design <- data.frame(siteID=FL_lakes$siteID,
                     wgt=FL_lakes$wgt,
                     xcoord=FL_lakes$xcoord,
                     ycoord=FL_lakes$ycoord)

## ----create_data.cat-----------------------------------------------------
data.cat <- data.frame(siteID=FL_lakes$siteID,
                       Status=FL_lakes$Status,
                       Target_NonTarget=FL_lakes$TNT)

## ----extent_est----------------------------------------------------------
# Calculate extent estimates for the site status evaluation variables
Extent_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----print_ext-----------------------------------------------------------
print(Extent_Estimates[c(1:7, 45:47),])

## ----write_ext-----------------------------------------------------------
write.csv(Extent_Estimates, file="Extent_Estimates.csv", row.names=FALSE)

## ----table_phcat---------------------------------------------------------
cat("\nA table displaying the number of values for each level of the pH category
variable follows:\n")
addmargins(table(FL_lakes$pH_Cat))

## ----table_colcat--------------------------------------------------------
cat("\nA table displaying the number of values for each level of the fecal
coliform category variable follows:\n")
addmargins(table(FL_lakes$Coliform_Cat))

## ----create_sites2-------------------------------------------------------
# Conduct an analysis of lake condition variables
# Create the sites data frame
# Note that only sampled sites are used
sites <- data.frame(siteID=FL_lakes$siteID,
                    Use=FL_lakes$Status == "Sampled")
# Note that the existing subpop and design data frames can be reused

## ----create_data.cat2----------------------------------------------------
data.cat <- data.frame(siteID=FL_lakes$siteID,
                       pHCat=FL_lakes$pH_Cat,
                       ColiformCat=FL_lakes$Coliform_Cat)

## ----cond_ests2----------------------------------------------------------
# Calculate estimates for the categorical variables
Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)

## ----print_all_cond------------------------------------------------------
print(Condition_Estimates[c(1:4, 28:32),])

## ----write_cond_ests-----------------------------------------------------
write.csv(Condition_Estimates, file="Condition_Estimates.csv", row.names=FALSE)

## ----framesize-----------------------------------------------------------
framesize <- c("NWFWMD-1"=451, "NWFWMD-2"=394, "SFWMD-9"=834, "SJRWMD-1"=1216,
               "SRWMD-1"=1400, "SWFWMD-4"=851)

## ----lake_cond_ests------------------------------------------------------
Condition_Estimates_popsize <- cat.analysis(sites, subpop, design, data.cat,
   popsize=list(CombinedBasins=sum(framesize),
                Basin=as.list(framesize)))

## ----print_lake_cond_ests------------------------------------------------
print(Condition_Estimates_popsize[c(1:4, 28:32),])

## ----write_lake_cond_ests------------------------------------------------
write.csv(Condition_Estimates_popsize, file="Condition_Estimates_popsize.csv",
   row.names=FALSE)

## ----summary_do----------------------------------------------------------
cat("\nSummarize the data structure of the dissolved oxygen variable:\n")
summary(FL_lakes$Oxygen)

## ----summary_turb--------------------------------------------------------
cat("\nSummarize the data structure of the turbidity variable:\n")
summary(FL_lakes$Turbidity)

## ----create_data.cont2---------------------------------------------------
data.cont <- data.frame(siteID=FL_lakes$siteID,
                        DissolvedOxygen=FL_lakes$Oxygen,
                        Turbidity=FL_lakes$Turbidity)

## ----cdf_ests2-----------------------------------------------------------
CDF_Estimates <- cont.analysis(sites, subpop, design, data.cont,
   popsize=list(CombinedBasins=sum(framesize),
                Basin=as.list(framesize)))

## ----write_cdf_ests2-----------------------------------------------------
write.csv(CDF_Estimates$CDF, file="CDF_Estimates.csv", row.names=FALSE)

## ----cdfplot-------------------------------------------------------------
cont.cdfplot("CDF_Estimates.pdf", CDF_Estimates$CDF, logx=c("","x"))

## ----print_cdf-----------------------------------------------------------
print(CDF_Estimates$Pct[1:10,])

## ----write_cdf-----------------------------------------------------------
write.csv(CDF_Estimates$Pct, file="Percentile_Estimates.csv")

## ----cdf_tests-----------------------------------------------------------
CDF_Tests <- cont.cdftest(sites, subpop[,c(1,3)], design, data.cont,
   popsize=list(Basin=as.list(framesize)))

## ----cdf_tests_print-----------------------------------------------------
print(CDF_Tests, digits=3)

## ----cdf_tests_write-----------------------------------------------------
write.csv(CDF_Tests, file="CDF_Tests.csv", row.names=FALSE)

