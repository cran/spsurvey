## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----load.nla------------------------------------------------------------
data(NLA_2007)
nr <- nrow(NLA_2007)

## ----head.nla------------------------------------------------------------
head(NLA_2007)

## ----tail.nla------------------------------------------------------------
tail(NLA_2007)

## ----create.sites--------------------------------------------------------
sites <- data.frame(siteID=NLA_2007$siteID,
                    Use=rep(TRUE, nr))

## ----create.subpop-------------------------------------------------------
subpop <- data.frame(siteID=NLA_2007$siteID,
                     Western_US=rep("Western_US", nr),
                     Lake_Origin=NLA_2007$Lake_Origin)

## ----create.design-------------------------------------------------------
design <- data.frame(siteID=NLA_2007$siteID,
                     wgt=NLA_2007$wgt,
                     xcoord=NLA_2007$xcoord,
                     ycoord=NLA_2007$ycoord)

## ----create.data.risk----------------------------------------------------
data.risk <- data.frame(siteID=NLA_2007$siteID,
                        Chlorophyll_a=NLA_2007$Chla_cond,
                        MacroInvert_OE=NLA_2007$OE5_cond,
                        Total_Nitrogen=NLA_2007$NTL_cond,
                        Total_Phosphorus=NLA_2007$PTL_cond,
                        Turbidity=NLA_2007$Turbidity_cond)

## ----stress.resp---------------------------------------------------------
resp_vars <- c("Chlorophyll_a", "MacroInvert_OE")
stress_vars <- c("Total_Nitrogen", "Total_Phosphorus", "Turbidity")

## ----calc.rel.risk-------------------------------------------------------
RelRisk_Estimates <- relrisk.analysis(sites, subpop, design, data.risk,
   response.var= rep(resp_vars, each=length(stress_vars)),
   stressor.var=rep(stress_vars, length(resp_vars)))

## ----print.rel.risk.ests-------------------------------------------------
print(RelRisk_Estimates)

## ----write.rel.risk------------------------------------------------------
write.csv(RelRisk_Estimates, file="RelRisk_Estimates.csv", row.names=FALSE)

## ----att.risk.ests-------------------------------------------------------
AttRisk_Estimates <- attrisk.analysis(sites, subpop, design, data.risk,
   response.var= rep(resp_vars, each=length(stress_vars)),
   stressor.var=rep(stress_vars, length(resp_vars)))

## ----print.att.risk.ests-------------------------------------------------
print(AttRisk_Estimates)

## ----write.att.risk.ests-------------------------------------------------
write.csv(AttRisk_Estimates, file="AttRisk_Estimates.csv", row.names=FALSE)

