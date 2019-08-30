## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----load-data-----------------------------------------------------------
data(NRSA_2009)
nr <- nrow(NRSA_2009)

## ----head-data-----------------------------------------------------------
head(NRSA_2009)

## ----tail-data-----------------------------------------------------------
tail(NRSA_2009)

## ----sites-data-frame----------------------------------------------------
sites <- data.frame(siteID=NRSA_2009$siteID,
                    Survey1=NRSA_2009$Survey == "WSA",
                    Survey2=NRSA_2009$Survey == "NRSA")

## ----repeats-data-frame--------------------------------------------------
repeats <- data.frame(siteID_1=NRSA_2009$siteID[NRSA_2009$Survey == "WSA" & NRSA_2009$Revisit_Site == "Y"], siteID_2=NRSA_2009$siteID[NRSA_2009$Survey == "NRSA" & NRSA_2009$Revisit_Site == "Y"])

## ----subpops-data-frame--------------------------------------------------
subpop <- data.frame(siteID=NRSA_2009$siteID, Western_Mountains=rep("Western Mountains", nr), Stream_Size=NRSA_2009$Stream_Size)

## ----design-data-frame---------------------------------------------------
design <- data.frame(siteID=NRSA_2009$siteID,
                     wgt=NRSA_2009$wgt,
                     xcoord=NRSA_2009$xcoord,
                     ycoord=NRSA_2009$ycoord)

## ----data.cat-data-frame-------------------------------------------------
data.cat <- data.frame(siteID=NRSA_2009$siteID,
                       Nitrogen_Condition=NRSA_2009$NTL_Cond,
                       Phosphorus_Condition=NRSA_2009$PTL_Cond,
                       Benthic_MMI_Condition=NRSA_2009$Benthic_MMI_Cond)

## ----data.contt-data-frame-----------------------------------------------
data.cont <- data.frame(siteID=NRSA_2009$siteID,
                        Log_Total_Phosphorus=log10(NRSA_2009$PTL+1),
                        Log_Total_Nitrogen=log10(NRSA_2009$NTL+1),
                        Benthic_MMI=NRSA_2009$Benthic_MMI)

## ----change-estimates----------------------------------------------------
Change_Estimates <- change.analysis(sites, repeats, subpop, design,
                                    data.cat, data.cont, 
                                    test=c("mean", "median"))

## ----warnprnt------------------------------------------------------------
warnprnt(m = c(1, 3))

## ----change--------------------------------------------------------------
# Print Western Mountains change estimates for categorical variables
print(subset(Change_Estimates$catsum, Type == "Western_Mountains"))

# Print change estimates for continuous variables using the mean
print(Change_Estimates$contsum_mean)

# Print change estimates for continuous variables using the median
print(subset(Change_Estimates$contsum_median, Type == "Western_Mountains"))

## ----write---------------------------------------------------------------
write.csv(Change_Estimates$catsum, file="Change_Estimates_Categorical.csv", row.names=FALSE)
write.csv(Change_Estimates$contsum_mean, file="Change_Estimates_Continuous_Mean.csv", row.names=FALSE)
write.csv(Change_Estimates$contsum_median, file="Change_Estimates_Continuous_Median.csv", row.names=FALSE)

