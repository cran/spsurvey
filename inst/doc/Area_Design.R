## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey------------------------------------------------------------
library(spsurvey)
library(sf)
library(sp)

## ----UT-ecoregions------------------------------------------------------------
data(UT_ecoregions)

## ----head-ecoregions----------------------------------------------------------
head(UT_ecoregions)

## ----summarize-ecoregions-----------------------------------------------------
temp <- with(UT_ecoregions, tapply(Area_ha, Level3_Nam, sum))
temp <- round(addmargins(temp), 0)
temp

## ----setseed------------------------------------------------------------------
set.seed(4447864)

## ----equaldesign--------------------------------------------------------------
Equaldsgn <- list(None=list(panel=c(PanelOne=50), seltype="Equal"))

## ----select_equalsites--------------------------------------------------------
Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL",
                   type.frame="area",
                   src.frame="sf.object",
                   sf.object=UT_ecoregions,
                   maxlev = 5,
                   shapefile=FALSE)

## ----head_design--------------------------------------------------------------
head(Equalsites)

## ----surveydesign_summary-----------------------------------------------------
summary(Equalsites)

## ----write_design-------------------------------------------------------------
st_write(UT_ecoregions, "UT_ecoregions.shp", quiet = TRUE, delete_dsn = TRUE)

## ----design_list--------------------------------------------------------------
Unequaldsgn <- list(None=list(panel=c(PanelOne=50),
                              seltype="Unequal",
                              caty.n=c("Central Basin and Range"=10,
                                       "Colorado Plateaus"=10,
                                       "Mojave Basin and Range"=5,
                                       "Northern Basin and Range"=5,
                                       "Southern Rockies"=5,
                                       "Wasatch and Uinta Mountains"=10,
                                       "Wyoming Basin"=5)))

## ----select_unequalsites------------------------------------------------------
Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="UNEQUAL",
                     type.frame="area",
                     src.frame="shapefile",
                     in.shape="UT_ecoregions.shp",
                     mdcaty="Level3_Nam",	
                     maxlev = 3,
                     shapefile=FALSE)

## ----head_unequalsites--------------------------------------------------------
head(Unequalsites)

## ----summary_unequalsites-----------------------------------------------------
summary(Unequalsites)

## ----create_spobject----------------------------------------------------------
UT_ecoregions_sp <- sf::as_Spatial(UT_ecoregions)
proj4string(UT_ecoregions_sp) <- sp::CRS(st_crs(UT_ecoregions)$proj4string)@projargs

## ----create_designlist--------------------------------------------------------
Stratdsgn <- list("Central Basin and Range"=list(panel=c(PanelOne=10),
                                                 seltype="Equal"),
                  "Colorado Plateaus"=list(panel=c(PanelOne=10),
                                           seltype="Equal"),
                  "Mojave Basin and Range"=list(panel=c(PanelOne=5),
                                                seltype="Equal"),
                  "Northern Basin and Range"=list(panel=c(PanelOne=5),
                                                  seltype="Equal"),
                  "Southern Rockies"=list(panel=c(PanelOne=5),
                                          seltype="Equal"),
                  "Wasatch and Uinta Mountains"=list(panel=c(PanelOne=10),
                                                     seltype="Equal"),
                  "Wyoming Basin"=list(panel=c(PanelOne=5),
                                       seltype="Equal"))

## ----select_strat_sample------------------------------------------------------
Stratsites <- grts(design=Stratdsgn,
                   DesignID="STRATIFIED",
                   type.frame="area",
                   src.frame="sp.object",
                   sp.object=UT_ecoregions_sp,
                   stratum="Level3_Nam",	
                   maxlev = 3,
                   shapefile=FALSE)

## ----head_stratsites----------------------------------------------------------
head(Stratsites)

## ----stratsites_summary-------------------------------------------------------
summary(Stratsites)

## ----create_paneldesign-------------------------------------------------------
Paneldsgn <- list(None=list(panel=c(Year1=10, Year2=10, Year3=10,
                                    Year4=10, Year5=10),
                            seltype="Unequal",
                            caty.n=c("Central Basin and Range"=10,
                                     "Colorado Plateaus"=10,
                                     "Mojave Basin and Range"=5,
                                     "Northern Basin and Range"=5,
                                     "Southern Rockies"=5,
                                     "Wasatch and Uinta Mountains"=10,
                                     "Wyoming Basin"=5),
                            over=5))

## ----select_panelsites--------------------------------------------------------
Panelsites <- grts(design=Paneldsgn,
                   DesignID="UNEQUAL",
                   type.frame="area",
                   src.frame="sf.object",
                   sf.object=UT_ecoregions,
                   maxlev = 5,
                   mdcaty="Level3_Nam",									
                   shapefile=FALSE)

## ----warnings-----------------------------------------------------------------
warnings()

## ----head_panelsites----------------------------------------------------------
head(Panelsites)

## ----summary_panelsites-------------------------------------------------------
summary(Panelsites)

