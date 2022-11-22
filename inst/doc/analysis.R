## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE, 
  message = FALSE
)

## ---- eval = FALSE------------------------------------------------------------
#  vignette("start-here", "spsurvey")

## -----------------------------------------------------------------------------
library(spsurvey)

## -----------------------------------------------------------------------------
cat_ests <- cat_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "NITR_COND",
  weight = "WEIGHT"
)
cat_ests

## -----------------------------------------------------------------------------
cat_ests_sp <- cat_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "NITR_COND",
  weight = "WEIGHT",
  subpop = "STATE"
)
cat_ests_sp

## -----------------------------------------------------------------------------
cat_ests_sp <- cat_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "NITR_COND",
  weight = "WEIGHT",
  subpop = "STATE",
  All_Sites = TRUE
)
cat_ests_sp

## -----------------------------------------------------------------------------
strat_cat_ests <- cat_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "NITR_COND",
  weight = "WEIGHT",
  stratumID = "URBAN"
)
strat_cat_ests

## -----------------------------------------------------------------------------
strat_cat_ests_sp <- cat_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "NITR_COND",
  weight = "WEIGHT",
  stratumID = "URBAN",
  subpop = "STATE"
)
strat_cat_ests_sp

## -----------------------------------------------------------------------------
cont_ests <- cont_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "BMMI",
  weight = "WEIGHT"
)

## -----------------------------------------------------------------------------
cont_ests$Mean

## ---- eval = FALSE------------------------------------------------------------
#  cont_ests$CDF
#  cont_ests$Pct

## -----------------------------------------------------------------------------
plot(cont_ests$CDF)

## -----------------------------------------------------------------------------
cont_ests_sp <- cont_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "BMMI",
  weight = "WEIGHT",
  subpop = "STATE"
)

## -----------------------------------------------------------------------------
cont_ests_sp$Mean

## -----------------------------------------------------------------------------
strat_cont_ests <- cont_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "BMMI",
  weight = "WEIGHT",
  stratumID = "URBAN"
)

## -----------------------------------------------------------------------------
strat_cont_ests$Mean

## -----------------------------------------------------------------------------
strat_cont_ests_sp <- cont_analysis(
  NLA_PNW,
  siteID = "SITE_ID",
  vars = "BMMI",
  weight = "WEIGHT",
  stratumID = "URBAN",
  subpop = "STATE",
)

## -----------------------------------------------------------------------------
strat_cont_ests_sp$Mean

## -----------------------------------------------------------------------------
attrisk_ests <- attrisk_analysis(
  NLA_PNW, 
  siteID = "SITE_ID",
  vars_response = "BMMI_COND",
  vars_stressor = "PHOS_COND",
  weight = "WEIGHT"
)
attrisk_ests

## -----------------------------------------------------------------------------
relrisk_ests <- relrisk_analysis(
  NLA_PNW, 
  siteID = "SITE_ID",
  vars_response = "BMMI_COND",
  vars_stressor = "PHOS_COND",
  weight = "WEIGHT"
)
relrisk_ests

## -----------------------------------------------------------------------------
diffrisk_ests <- diffrisk_analysis(
  NLA_PNW, 
  siteID = "SITE_ID",
  vars_response = "BMMI_COND",
  vars_stressor = "PHOS_COND",
  weight = "WEIGHT"
)
diffrisk_ests

## -----------------------------------------------------------------------------
change_ests <- change_analysis(
  NRSA_EPA7,
  siteID = "SITE_ID",
  vars_cont = "BMMI",
  vars_cat = "NITR_COND",
  surveyID = "YEAR",
  weight = "WEIGHT"
)

## -----------------------------------------------------------------------------
change_ests$catsum

## -----------------------------------------------------------------------------
change_ests$contsum_mean

