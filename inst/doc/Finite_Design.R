## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-spsurvey------------------------------------------------------------
library(spsurvey)
library(sf)

## ----load_NElakes-------------------------------------------------------------
data(NE_lakes)

## ----head_NElakes-------------------------------------------------------------
head(NE_lakes)

## ----cross-class_NElakes------------------------------------------------------
with(NE_lakes, addmargins(table("State"=State, "Lake Area Category"=Area_Cat)))

## ----set.seed-----------------------------------------------------------------
set.seed(4447864)

## ----design.list--------------------------------------------------------------
Equaldsgn <- list(None=list(panel=c(PanelOne=100), seltype="Equal"))

## ----select.sample------------------------------------------------------------
Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL",
                   type.frame="finite",
                   src.frame="sf.object",
                   sf.object=NE_lakes,
                   shapefile=FALSE)

## ----head.design--------------------------------------------------------------
head(Equalsites)

## ----summary.design-----------------------------------------------------------
summary(Equalsites)

## ----create_df----------------------------------------------------------------
geom_name <- attr(NE_lakes, "sf_column")
NE_lakes_df <- subset(NE_lakes, select=names(NE_lakes) != geom_name, drop = TRUE)

## ----create_designlist--------------------------------------------------------
Stratdsgn <- list(CT=list(panel=c(PanelOne=40), seltype="Equal"),
                  MA=list(panel=c(PanelOne=40), seltype="Equal"),
                  RI=list(panel=c(PanelOne=20), seltype="Equal"))

## ----select.sample2-----------------------------------------------------------
Stratsites <- grts(design=Stratdsgn,
                   DesignID="STRATIFIED",
                   type.frame="finite",
                   src.frame="att.frame",
                   att.frame=NE_lakes_df,
                   xcoord="xcoord",
                   ycoord="ycoord",
                   stratum="State",
                   shapefile=FALSE)

## ----head.stratsites----------------------------------------------------------
head(Stratsites)

## ----summary.stratsites-------------------------------------------------------
summary(Stratsites)

## ----create_sp----------------------------------------------------------------
NE_lakes_sp <- as_Spatial(NE_lakes)

## ----create_design_list-------------------------------------------------------
Unequaldsgn <- list(None=list(panel=c(PanelOne=90),
                              seltype="Unequal",
                              caty.n=c("(0,1]"=15, "(1,5]"=30, "(5,10]"=15,
                                       "(10,50]"=15, "(50,500]"=10,
                                       "(500,1e+04]"=5),
                              over=10))

## ----select_sample------------------------------------------------------------
Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="UNEQUAL",
                     type.frame="finite",
                     src.frame="sp.object",
                     sp.object=NE_lakes_sp,
                     mdcaty="Area_Cat",
                     shapefile=FALSE)

## ----head_design--------------------------------------------------------------
head(Unequalsites)

## ----summary_design-----------------------------------------------------------
summary(Unequalsites)

## ----create_shapefile---------------------------------------------------------
st_write(NE_lakes, "NE_lakes.shp", quiet = TRUE, delete_dsn = TRUE)

## ----create_design2-----------------------------------------------------------
Paneldsgn <- list(None=list(panel=c(Annual=15, Year1=15, Year2=15, Year3=15,
                                    Year4=15, Year5=15),
                            seltype="Unequal",
                            caty.n=c("(0,1]"=15, "(1,5]"=30, "(5,10]"=15,
                                     "(10,50]"=15, "(50,500]"=10,
                                     "(500,1e+04]"=5),
                            over=10))

## ----select_sample2-----------------------------------------------------------
Panelsites <- grts(design=Paneldsgn,
                   DesignID="UNEQUAL",
                   type.frame="finite",
                   src.frame="shapefile",
                   in.shape="NE_lakes.shp",
                   mdcaty="Area_Cat",
                   shapefile=FALSE)

## ----warnings-----------------------------------------------------------------
warnings()

## ----head_design2-------------------------------------------------------------
head(Panelsites)

## ----summary_design2----------------------------------------------------------
summary(Panelsites)

