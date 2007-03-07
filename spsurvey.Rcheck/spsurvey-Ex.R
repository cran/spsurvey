### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
              outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           pushViewport(viewport(width=unit(1, "npc") - unit(1, "lines"),
                                 x=0, just="left"))
           grid.text(sprintf("help(\"%s\")", nameEx()),
                     x=unit(1, "npc") + unit(0.5, "lines"),
                     y=unit(0.8, "npc"), rot=90,
                     gp=gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
grDevices::postscript("spsurvey-Ex.ps")
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"), pager="console")
options(warn = 1)    
library('spsurvey')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("adjwgt");
### * adjwgt

flush(stderr()); flush(stdout())

### Name: adjwgt
### Title: Adjust Initial Survey Design Weights
### Aliases: adjwgt
### Keywords: survey misc

### ** Examples

sites <- as.logical(rep(rep(c("TRUE","FALSE"), c(9,1)), 5))
wgt <- runif(50, 10, 100)
wtcat <- rep(c("A","B"), c(30, 20))
framesize <- c(15, 10)
names(framesize) <- c("A","B")
adjwgt(sites, wgt, wtcat, framesize)



cleanEx(); nameEx("cat.analysis");
### * cat.analysis

flush(stderr()); flush(stdout())

### Name: cat.analysis
### Title: Categorical Data Analysis for Probability Survey Data
### Aliases: cat.analysis
### Keywords: survey univar

### ** Examples

# Categorical variable example for two resource classes
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10, 100),
   xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
   "Stratum2"), 50))
mydata.cat <- data.frame(siteID=mysiteID, CatVar=rep(c("north", "south",
   "east", "west"), 25))
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))
cat.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, popsize=mypopsize)

# Exclude category "south" from the analysis
mysites <- data.frame(siteID=mysiteID, Active=rep(c(TRUE, FALSE, TRUE,
   TRUE), 25))
cat.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, popsize=mypopsize)



cleanEx(); nameEx("category.est");
### * category.est

flush(stderr()); flush(stdout())

### Name: category.est
### Title: Category Proportion and Size Estimates
### Aliases: category.est
### Keywords: survey univar

### ** Examples

catvar <- rep(c("north", "south", "east", "west"), rep(25, 4))
wgt <- runif(100, 10, 100)
category.est(catvar, wgt, vartype="SRS")

x <- runif(100)
y <- runif(100)
category.est(catvar, wgt, x, y)



cleanEx(); nameEx("cdf.decon");
### * cdf.decon

flush(stderr()); flush(stdout())

### Name: cdf.decon
### Title: Cumulative Distribution Function - Deconvolution
### Aliases: cdf.decon
### Keywords: survey distribution

### ** Examples

z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
cdfval <- seq(min(z), max(z), length=20)
cdf.decon(z, wgt, sigma=0.25, var.sigma=0.1, vartype=
  "SRS", cdfval=cdfval)

x <- runif(100)
y <- runif(100)
cdf.decon(z, wgt, sigma=0.25, var.sigma=0.1, x, y, cdfval=
  cdfval)



cleanEx(); nameEx("cdf.est");
### * cdf.est

flush(stderr()); flush(stdout())

### Name: cdf.est
### Title: Cumulative Distribution Function - Estimation
### Aliases: cdf.est
### Keywords: survey distribution

### ** Examples

z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
cdfval <- seq(min(z), max(z), length=20)
cdf.est(z, wgt, vartype="SRS", cdfval=cdfval)

x <- runif(100)
y <- runif(100)
cdf.est(z, wgt, x, y, cdfval=cdfval)



cleanEx(); nameEx("cdf.test");
### * cdf.test

flush(stderr()); flush(stdout())

### Name: cdf.test
### Title: Cumulative Distribution Function - Inference
### Aliases: cdf.test
### Keywords: survey distribution

### ** Examples

resp <- rnorm(100, 10, 1)
wgt <- runif(100, 40, 60)
sample1 <- list(z=resp, wgt=wgt)
sample2 <- list(z=resp+0.5, wgt=wgt)
bounds <- sort(c(sample1$z, sample2$z))[floor(seq(200/3, 200, length=3))]
cdf.test(sample1, sample2, bounds, vartype="SRS")

xcoord <- runif(100)
ycoord <- runif(100)
sample1 <- list(z=resp, wgt=wgt, x=xcoord, y=ycoord)
sample2 <- list(z=1.05*resp, wgt=wgt, x=xcoord, y=ycoord)
cdf.test(sample1, sample2, bounds)



cleanEx(); nameEx("cont.analysis");
### * cont.analysis

flush(stderr()); flush(stdout())

### Name: cont.analysis
### Title: Continuous Data Analysis for Probability Survey Data
### Aliases: cont.analysis
### Keywords: survey distribution univar

### ** Examples

# Continuous variable example
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites",100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10, 100),
   xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
   "Stratum2"), 50))
ContVar <- rnorm(100, 10, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar)
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))
cont.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cont=mydata.cont, popsize=mypopsize)

# Include deconvolution estimates
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar,
   ContVar.1=ContVar + rnorm(100, 0, sqrt(0.25)),
   ContVar.2=ContVar + rnorm(100, 0, sqrt(0.50)))
mysigma <- c(NA, 0.25, 0.50)
names(mysigma) <- c("ContVar", "ContVar.1", "ContVar.2")
cont.analysis(sites=mysites, subpop=mysubpop[,1:2], design=mydesign,
   data.cont=mydata.cont, sigma=mysigma, popsize=mypopsize[1])



cleanEx(); nameEx("dsgnsum");
### * dsgnsum

flush(stderr()); flush(stdout())

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



cleanEx(); nameEx("eco.l3.ut");
### * eco.l3.ut

flush(stderr()); flush(stdout())

### Name: eco.l3.ut
### Title: Example Polygons Dataset
### Aliases: eco.l3.ut
### Keywords: datasets

### ** Examples

# This example converts the dataset to an sp package object
data(eco.l3.ut)
n <- length(eco.l3.ut)
nparts <- rep(1, n)
ringdir <- rep(1, n)
IDs <- as.character(1:n)
shapes <- vector(mode="list", length=n)
for(i in 1:n) {
   shapes[[i]] <- list(Pstart=0, verts=eco.l3.ut[[i]], 
      nVerts=nrow(eco.l3.ut[[i]]), nParts=nparts[i])
   attr(shapes[[i]], "RingDir") <- ringdir[i]
}
PolygonsList <- vector(mode="list", length=n)
for(i in 1:n) {
  PolygonsList[[i]] <- shape2spList(shape=shapes[[i]], shp.type="poly",
     ID=IDs[i])
}
att.data <- data.frame(id=1:n, area=1:n)
for(i in 1:n) {
   att.data$area[i] <- PolygonsList[[i]]@area
}
rownames(att.data) <- IDs
sp.obj <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=PolygonsList),
   data=att.data)
# To convert the sp package object to a shapefile use the following code: 
# sp2shape(sp.obj, "eco.l3.ut")



cleanEx(); nameEx("framesum");
### * framesum

flush(stderr()); flush(stdout())

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



cleanEx(); nameEx("grts");
### * grts

flush(stderr()); flush(stdout())

### Name: grts
### Title: Generalized Random-Tessellation Stratified (GRTS) Survey Design
### Aliases: grts
### Keywords: survey

### ** Examples

## Not run: 
##D The following example will select a sample from an area resource.  The design
##D includes two strata.  For Stratum 1, an equal probability sample of size 50
##D will be selected for a single panel.  For Stratum 2, an unequal probability
##D sample of size 50 will be selected for each of two panels.  The sample for
##D Stratum 2 will be approportioned into samples of size 25 for each of four
##D unequal probability categories.  In addition both strata will include
##D oversamples (size 10 for Stratum 1 and size 75 for Stratum 2).  It is assumed
##D that a shapefile defining the polygons for the area resource is located in the
##D folder from which R is started.  Attribute data for the design will be read
##D from the dbf file of the shapefile, which is assumed to have variables named
##D "test.stratum" and "test.mdcaty" that specify stratum membership value and
##D unequal probability category, respectively, for each record in the shapefile.
##D A shapefile named "test.sample" containing the survey design information will
##D be created.
##D test.design <- list("Stratum 1"=list(panel=c(Panel=50), seltype="Equal",
##D    over=10), "Stratum 2"=list(panel=c("Panel One"=50, "Panel Two"=50),
##D    seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25,
##D    CatyFour=25), over=75))
##D test.attframe <- read.dbf("test.shapefile")
##D test.sample <- grts(design=test.design, DesignID="Test.Site", type.frame="area",
##D    src.frame="shapefile", in.shape="test.shapefile", att.frame=test.attframe,
##D    stratum="test.stratum", mdcaty="test.mdcaty", shapefile=TRUE,
##D    out.shape="test.sample")
## End(Not run)



cleanEx(); nameEx("irs");
### * irs

flush(stderr()); flush(stdout())

### Name: irs
### Title: Independent Random Sample (IRS) Survey Design
### Aliases: irs
### Keywords: survey

### ** Examples

## Not run: 
##D # The following example will select a sample from an area resource.  The design
##D # includes two strata.  For Stratum 1, a sample of size 50 will be selected for
##D # a single panel.  For Stratum 2, a sample of size 50 will be selected for each
##D # of two panels.  In addition both strata will include oversamples (size 10 for
##D # Stratum 1 and size 75 for Stratum 2).  It is assumed that a shapefile defining
##D # the polygons for the area resource is located in the folder from which R is
##D # started.  Attribute data for the design will be read from the dbf file of the
##D # shapefile, which is assumed to have a variable named "test.stratum" that
##D # specifies stratum membership value for each record in the shapefile. A
##D # shapefile named "test.sample" containing the survey design information will be
##D # created.
##D test.design <- list("Stratum 1"=list(panel=c(Panel=50), seltype="Equal",
##D    over=10), "Stratum 2"=list(panel=c("Panel One"=50, "Panel Two"=50),
##D    seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25,
##D    CatyFour=25), over=75))
##D test.attframe <- read.dbf("test.shapefile")
##D test.sample <- irs(design=test.design, DesignID="Test.Site", type.frame="area",
##D    src.frame="shapefile", in.shape="test.shapefile", att.frame=test.attframe,
##D    stratum="test.stratum", mdcaty="test.mdcaty", shapefile=TRUE,
##D    out.shape="test.sample")
## End(Not run)



cleanEx(); nameEx("luck.ash");
### * luck.ash

flush(stderr()); flush(stdout())

### Name: luck.ash
### Title: Example Polygons Dataset
### Aliases: luck.ash
### Keywords: datasets

### ** Examples

# This example converts the dataset to an sp package object
data(luck.ash)
n <- length(luck.ash)
nparts <- rep(1, n)
IDs <- as.character(1:n)
shapes <- vector(mode="list", length=n)
for(i in 1:n) {
   shapes[[i]] <- list(Pstart=0, verts=luck.ash[[i]], 
      nVerts=nrow(luck.ash[[i]]), nParts=nparts[i])
}
PolylinesList <- vector(mode="list", length=n)
for(i in 1:n) {
  PolylinesList[[i]] <- shape2spList(shape=shapes[[i]], shp.type="arc",
     ID=IDs[i])
}
att.data <- data.frame(id=1:n, length=rep(NA, n))
rownames(att.data) <- IDs
sp.obj <- SpatialLinesDataFrame(sl=SpatialLines(LinesList=PolylinesList),
   data=att.data)
# To convert the sp package object to a shapefile use the following code: 
# sp2shape(sp.obj, "luck.ash")



cleanEx(); nameEx("marinus");
### * marinus

flush(stderr()); flush(stdout())

### Name: marinus
### Title: Convert Coordinates from Latitude/Longitude to the Equidistant,
###   Cylindric Map Projection
### Aliases: marinus
### Keywords: misc

### ** Examples

lat <- 45 + runif(100, -5, 5)
lon <- 120 + runif(100, -10, 10)
marinus(lat, lon)



cleanEx(); nameEx("psurvey.analysis");
### * psurvey.analysis

flush(stderr()); flush(stdout())

### Name: psurvey.analysis
### Title: Create an Object of Class psurvey.analysis
### Aliases: psurvey.analysis
### Keywords: survey

### ** Examples

# Categorical variable example
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10,
   100), xcoord=runif(100), ycoord=runif(100), stratum= rep(c("Stratum1",
   "Stratum2"), 50))
mydata.cat <- data.frame(siteID=mysiteID, CatVar= rep(c("north", "south",
   "east", "west"), 25))
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))
psurvey.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, popsize=mypopsize)

# Continuous variable example - including deconvolution estimates
mydesign <- data.frame(ID=mysiteID, wgt=runif(100, 10, 100),
   xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
   "Stratum2"), 50))
ContVar <- rnorm(100, 10, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar,
   ContVar.1=ContVar + rnorm(100, 0, sqrt(0.25)),
   ContVar.2=ContVar + rnorm(100, 0, sqrt(0.50)))
mysigma <- c(ContVar=NA, ContVar.1=0.25, ContVar.2=0.50)
psurvey.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cont=mydata.cont, siteID=~ID, sigma=mysigma,
   popsize=mypopsize)



cleanEx(); nameEx("read.dbf");
### * read.dbf

flush(stderr()); flush(stdout())

### Name: read.dbf
### Title: Read the Attribute (dbf) File of an ESRI Shapefile
### Aliases: read.dbf
### Keywords: survey

### ** Examples

  ## Not run: 
##D   read.shape("my.dbffile")
##D   
## End(Not run)



cleanEx(); nameEx("read.shape");
### * read.shape

flush(stderr()); flush(stdout())

### Name: read.shape
### Title: Read an ESRI Shapefile
### Aliases: read.shape
### Keywords: IO

### ** Examples

  ## Not run: 
##D   read.shape("my.shapefile")
##D   
## End(Not run)



cleanEx(); nameEx("relrisk");
### * relrisk

flush(stderr()); flush(stdout())

### Name: relrisk
### Title: Relative Risk
### Aliases: relrisk
### Keywords: survey survival

### ** Examples

dframe <- data.frame(response=sample(c("Poor", "Good"), 100, replace=TRUE),
   stressor=sample(c("Poor", "Good"), 100, replace=TRUE),
   wgt=runif(100, 10, 100))
relrisk(dframe, vartype="SRS")

dframe$xcoord <- runif(100)
dframe$ycoord <- runif(100)
relrisk(dframe)



cleanEx(); nameEx("sp2shape");
### * sp2shape

flush(stderr()); flush(stdout())

### Name: sp2shape
### Title: Convert an sp Package Object to an ESRI Shapefile
### Aliases: sp2shape
### Keywords: IO

### ** Examples

  ## Not run: 
##D   sp2shape(my.sp.object, "my.shapefile")
##D   
## End(Not run)



cleanEx(); nameEx("total.est");
### * total.est

flush(stderr()); flush(stdout())

### Name: total.est
### Title: Population Total, Mean, Variance, and Standard Deviation
### Aliases: total.est
### Keywords: survey univar

### ** Examples

z <- rnorm(100, 10, 1)
wgt <- runif(100, 10, 100)
total.est(z, wgt, vartype="SRS")

x <- runif(100)
y <- runif(100)
total.est(z, wgt, x, y)



cleanEx(); nameEx("write.object");
### * write.object

flush(stderr()); flush(stdout())

### Name: write.object
### Title: Write an Object to a Plot
### Aliases: write.object
### Keywords: aplot

### ** Examples

z <- rnorm(100)
z.mean <- c(tapply(z, rep(1:4, rep(25,4)), mean), mean(z))
z.sd <- sqrt(c(tapply(z, rep(1:4, rep(25,4)), var), var(z)))
z.upper <- z.mean+1.96*z.sd
z.lower <- z.mean-1.96*z.sd
obj <- data.frame(rbind(z.mean, z.sd, z.upper, z.lower))
dimnames(obj) <- list(c("Mean Estimate", "Standard Deviation",
  "Lower 95% Conf. Bound", "Upper 95% Conf. Bound"), c(
  paste("Stratum", 1:4, sep=""), "AllStrata"))
write.object(obj, n.digits=3, r.cex=0.75)

obj <- data.frame(matrix(round(5 + runif(30), 1), nrow=6))
colnames(obj) <- c("United.States", "Russia", "Germany",
  "Japan", "France")
write.object(obj, n.digits=1, r.names=FALSE)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
