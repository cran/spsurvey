## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey-------------------------------------------------------
library(spsurvey)

## ----load_luckash--------------------------------------------------------
data(Luck_Ash_streams)

## ----head_luckash--------------------------------------------------------
head(Luck_Ash_streams)

## ----table_luckash-------------------------------------------------------
with(Luck_Ash_streams, addmargins(table("Stream Type"=Per_Int, "Strahler Order"=Strah_Cat)))

## ----summarize_luckash---------------------------------------------------
temp <- with(Luck_Ash_streams, tapply(Length_km, list(Per_Int, Strah_Cat), sum))
temp <- round(addmargins(temp), 2)
names(dimnames(temp)) <- list("Stream Type", "Strahler Order")
temp

## ----set.seed------------------------------------------------------------
set.seed(19742003)

## ----create.design.list--------------------------------------------------
Equaldsgn <- list(None=list(panel=c(PanelOne=100), seltype="Equal"))

## ----select.sample-------------------------------------------------------
Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL",
                   type.frame="linear",
                   src.frame="sf.object",
                   sf.object=Luck_Ash_streams,
                   maxlev = 5,
                   shapefile=FALSE)

## ----head.design---------------------------------------------------------
head(Equalsites)

## ----summary.equalsites--------------------------------------------------
summary(Equalsites)

## ----create_shapefile----------------------------------------------------
st_write(Luck_Ash_streams, "Luck_Ash_streams.shp", quiet = TRUE, delete_dsn = TRUE)

## ----design.list---------------------------------------------------------
Stratdsgn <- list(Perennial=list(panel=c(PanelOne=40),
                                 seltype="Equal",
                                 over=10),
                  Intermittent=list(panel=c(PanelOne=40),
                                    seltype="Equal",
                                    over=10))

## ----select.sample2------------------------------------------------------
Stratsites <- grts(design=Stratdsgn,
                   DesignID="STRATIFIED",
                   type.frame="linear",
                   src.frame="shapefile",
                   in.shape="Luck_Ash_streams.shp",
                   maxlev = 5,
                   stratum="Per_Int",
                   shapefile=FALSE)

## ----head.strat----------------------------------------------------------
head(Stratsites)

## ----summary.stratsites--------------------------------------------------
summary(Stratsites)

## ----create.sp.object----------------------------------------------------
Luck_Ash_streams_sp <- as_Spatial(Luck_Ash_streams)

## ----create.design.list2-------------------------------------------------
Unequaldsgn <- list(Perennial=list(panel=c(PanelOne=60),
                                   seltype="Unequal",
                                   caty.n=c("1st"=20, "2nd"=20, "3rd+"=20),
                                   over=10),
                    Intermittent=list(panel=c(PanelOne=30),
                                      seltype="Unequal",
                                      caty.n=c("1st"=20, "2nd"=7, "3rd+"=3),
                                      over=0))

## ----select.sample3------------------------------------------------------
Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="UNEQUAL",
                     type.frame="linear",
                     src.frame="sp.object",
                     sp.object=Luck_Ash_streams_sp,
                     stratum="Per_Int",
                     maxlev=5,
                     mdcaty="Strah_Cat",
                     shapefile=FALSE)

## ----head.unequalsites---------------------------------------------------
head(Unequalsites)

## ----summary.unequalsites------------------------------------------------
summary(Unequalsites)

## ----create.panel.design-------------------------------------------------
Paneldsgn <- list(Perennial=list(panel=c(Annual=20, Year1=20, Year2=20),
                                 seltype="Unequal",
                                 caty.n=c("1st"=25, "2nd"=20, "3rd+"=15),
                                 over=15),
                  Intermittent=list(panel=c(Annual=25),
                                    seltype="Unequal",
                                    caty.n=c("1st"=18, "2nd"=5, "3rd+"=2)))

## ----select.panel.sample-------------------------------------------------
Panelsites <- grts(design=Paneldsgn,
                   DesignID="UNEQUAL",
                   type.frame="linear",
                   src.frame="sf.object",
                   sf.object=Luck_Ash_streams,
                   stratum="Per_Int",
                   maxlev = 5,
                   mdcaty="Strah_Cat",
                   shapefile=FALSE)

## ----head.panel.sites----------------------------------------------------
head(Panelsites)

## ----summary.panel.sites-------------------------------------------------
summary(Panelsites)

