### Name: dsgnsum
### Title: Summarize the Sites Selected for a Survey Design
### Aliases: dsgnsum
### Keywords: survey

### ** Examples

## Not run: 
##D test.design <- list(Stratum1=list(panel=c(PanelOne=50),
##D    seltype="Equal", over=10), Stratum2=list(panel=c(PanelOne=50,
##D    PanelTwo=50), seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25,
##D    CatyThree=25, CatyFour=25), over=75)
##D test.attframe <- read.dbf("test.shapefile")
##D test.sample <- grts(design=test.design, DesignID="Test.Site", type.frame="area",
##D    src.frame="shapefile", in.shape="test.shapefile", att.frame=test.attframe,
##D    stratum="test.stratum", mdcaty="test.mdcaty", shapefile=TRUE,
##D    shapefilename="test.sample")
##D dsgnsum(test.sample, auxvar=c("test.ecoregion", "test.state"))
## End(Not run)



