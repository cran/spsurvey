## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE, 
  message = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("spsurvey")

## ----setup--------------------------------------------------------------------
library(spsurvey)

## -----------------------------------------------------------------------------
citation("spsurvey")

## -----------------------------------------------------------------------------
NE_Lakes_geo <- st_as_sf(NE_Lakes_df, coords = c("XCOORD", "YCOORD"), crs = 4326)
NE_Lakes_geo

## -----------------------------------------------------------------------------
NE_Lakes <- st_transform(NE_Lakes_geo, crs = 5070)
NE_Lakes

