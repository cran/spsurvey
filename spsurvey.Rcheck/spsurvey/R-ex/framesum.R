### Name: framesum
### Title: Summarize Frame Size for a Survey Design
### Aliases: framesum
### Keywords: survey

### ** Examples

## Not run: 
##D test.attframe <- read.dbf("test.shapefile")
##D test.design <- list(Stratum1=list(panel=c(PanelOne=50),
##D    seltype="Equal", over=10), Stratum2=list(panel=c(PanelOne=50,
##D    PanelTwo=50), seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25,
##D    CatyThree=25, CatyFour=25), over=75)
##D framesum(att.frame=test.attframe, design=test.design, type.frame="area",
##D    stratum="test.stratum", mdcaty="test.mdcaty", auxvar=c("test.ecoregion",
##D    "test.state"), units.in="Meters", scale=1000, units.out="Kilometers")
## End(Not run)



