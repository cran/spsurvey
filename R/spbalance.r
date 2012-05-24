spbalance <- function(sp.sample, shapefilename=NULL, tess.ind=TRUE,
   sbc.ind=FALSE, nrows=5, dxdy=TRUE, vartype="Local") {

################################################################################
# Function: spbalance
# Purpose: Calculate spatial balance metrics for a survey design
# Programmer: Tom Kincaid
# Date: February 17, 2012
# Description:      
#   This function calculates spatial balance metrics for a survey design.    Two
#  options for calculation of spatial balance metrics are available: (1) use
#  proportions obtained from the intersection of Dirichlet tesselation polygons
#  for the sample points with the frame object and (2) use proportions obtained
#  from a rectangular grid superimposed on the sample points and the frame
#  object.  In both cases the proportions are used to calculate the spatial
#  balance metrics.  Two metrics are calculated: (1) the Pielou evenness measure
#  and (2) the chi-square statistic.
# Arguments:
#   sp.sample = the sp package object of class "SpatialPointsDataFrame" created
#     by the grts or irs functions.
#   shapefilename = name of the input shapefile.  If shapefilename equal NULL,
#     then the shapefile or shapefiles in the working directory are read.  The
#     default is NULL.
#   tess.ind = a logical variable indicating whether spatial balance metrics are
#     calculated using proportions obtained from the intersection of Dirichlet
#     tesselation polygons for the sample points with the frame object.  TRUE
#     means calculate the metrics.  FALSE means do not calculate the metrics.
#     The default is TRUE. 
#   sbc.ind = a logical variable indicating whether spatial balance metrics are
#     calculated using proportions obtained from a rectangular grid superimposed
#     on the sample points and the frame.  TRUE means calculate the metrics.
#     FALSE means do not calculate the metrics. The default is FALSE. 
#   nrows = number of rows (and columns) for the grid of cells.  The default is
#     5.
#   dxdy = indicator for equal x-coordinate and y-coordinate grid cell
#     increments, where TRUE means the increments are equal and FALSE means the
#     increments are not equal.  The default is TRUE.
#   vartype = the choice of variance estimator, where "Local" = local mean
#     estimator and "SRS" = SRS estimator.  The default is "Local".
# Results: 
#   A list containing the following components:
#     (1) tess - results for spatial balance metrics using tesselation polygons
#     (2) sbc - results for spatial balance metrics using a rectangular grid
#   If either the tess.ind or sbc.ind arguments are set to FALSE, the
#   corresponding component in the list is set to NULL.  Otherwise, each
#   components of the list is a lists that contains the following components:
#     (1) J_subp - Pielou evenness measure
#     (2) chi_sq - chi-cquare statistic
#     (3) extent - frame extent for each  Dirichlet tesselation polygon or
#                  rectangular grid cell
#     (4) prop - frame proportion for each Dirichlet tesselation polygon or
#                rectangular grid cell
# Other Functions Required:
#   deldir - deldir package function that computes the Delaunay triangulation
#     and Dirichlet tesselation of a set of points.
#   tile.list - deldir package function that extracts coordinates of the
#     Dirichlet tesselation polygons from the object produced by the deldir
#     function.
#   gIntersection - rgeos package function that determines the intersection
#     between two sp package objects
#   LinesLength - sp package function that determines length of the line
#     segemnts in a class Lines object
# Example:
#   design <- list(Stratum1=list(panel=c(PanelOne=50), seltype="Equal",
#      over=10), Stratum2=list(panel=c(PanelOne=50, PanelTwo=50),
#      seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25,
#      CatyFour=25), over=75))
#   attframe <- read.dbf("shapefile")
#   samp <- grts(design=design, DesignID="Test.Site", type.frame="area",
#      src.frame="shapefile", in.shape="shapefile", att.frame=attframe,
#      stratum="stratum", mdcaty="mdcaty", shapefile=TRUE,
#      shapefilename="sample")
#   spbalance(samp, shapefilename="shapefile", sbc.ind=TRUE)
################################################################################

# Obtain the sample x-coordinates, y-coordinates, survey design weights,
# multidensity category values, and  stratum names from the sp.sample object
xcoord <- sp.sample@data$xcoord
ycoord <- sp.sample@data$ycoord
wgt <- sp.sample@data$wgt
mdcaty <- sp.sample@data$mdcaty
stratum <- sp.sample@data$stratum
n <- nrow(sp.sample@data)

# Determine the strata names
strata.names <- unique(stratum)

#
# Section for metrics calculted using Dirichlet tesselation polygons
#

if(tess.ind) {

# Read the shapefile into an object named sp.frame
   if(!is.null(shapefilename)) {
      nc <- nchar(shapefilename)
      if(substr(shapefilename, nc-3, nc) == ".shp") {
         shapefilename <- substr(shapefilename, 1, nc-4)
      }
   }
   sp.frame <- read.shape(shapefilename)

# Obtain the bounding box from the sp.frame object
   bbox <- c(sp.frame@bbox[1,], sp.frame@bbox[2,])

# Create an sp object containing the Dirichlet tesselation polygons for the
# sample points
   tiles <- tile.list(deldir(xcoord, ycoord, rw=bbox))
   sp.tess <- rep(list(NA), n)
   for(i in 1:n) {
      nv <- length(tiles[[i]]$x)
      sp.tess[[i]] <- SpatialPolygons(list(Polygons(list(Polygon(cbind(
         c(tiles[[i]]$x[1], tiles[[i]]$x[nv:1]),
         c(tiles[[i]]$y[1], tiles[[i]]$y[nv:1])))), 1)))
   }

# Determine the type of frame object
   temp <- class(sp.frame)
   ftype <- substr(temp, 8, nchar(temp) - 9)

# Intersect each Dirichlet tesselation polygon with the frame object and
# calculate extent and proportion
   extent <- numeric(n)
   for(i in 1:n) {
      if(ftype == "Points") {
         temp <- gIntersection(sp.tess[[i]], sp.frame)
         extent[i] <- nrow(temp@coords)
      } else if(ftype == "Lines") {
         temp <- sapply(sp.frame@lines, function(x) length(x@Lines))
         if(any(temp > 1)) {
            for(j in 1:length(sp.frame@lines)) {
               temp <- gIntersection(sp.tess[[i]],
                                     SpatialLines(list(sp.frame@lines[[j]])))
               extent[i] <- extent[i] + ifelse(is.null(temp), 0,
                  LinesLength(temp@lines[[1]]))
            }
         } else {
            temp <- gIntersection(sp.tess[[i]], sp.frame)
            extent[i] <- LinesLength(temp@lines[[1]])
         }
      } else if(ftype == "Polygons") {
         temp <- sapply(sp.frame@polygons, function(x) length(x@Polygons))
         if(any(temp > 1)) {
            for(j in 1:length(sp.frame@polygons)) {
               temp <- gIntersection(sp.tess[[i]],
                                  SpatialPolygons(list(sp.frame@polygons[[j]])))
               extent[i] <- extent[i] + ifelse(is.null(temp), 0,
                  temp@polygons[[1]]@area)
            }
         } else {
            temp <- gIntersection(sp.tess[[i]], sp.frame)
            extent[i] <- temp@polygons[[1]]@area
         }
      } else {
         stop(paste("'Spatial", ftype, "DataFrame' is not a known class of sp object.\n", sep=""))
      }
   }
   prop <- extent/sum(extent)

# Calculate the spatial balance metrics
   prob <- wgt/sum(wgt)
   J_subp <- sum(prop * log(prop))/sum(prob * log(prob))
   J_subp <- min(J_subp, 1)
   chi_sq <- sum(((prop - prob)^2)/prob)

# Create the output list
   tess <- list(J_subp=J_subp, chi_sq=chi_sq, extent=extent, prop=prop)

# Metrics calculated using Dirichlet tesselation polygons were not requested
} else {
   tess <- NULL
}

#
# Section for metrics calculted using a rectangular grid
#

if(sbc.ind) {

# Calculate grid cell extent and proportion for the frame
   sbc.frame <- sbcframe(shapefilename, nrows, dxdy)

# Calculate grid cell extent and proportion for the sample
   sbc.sample <- sbcsamp(sp.sample, sbc.frame)      

# Calculate the spatial balance metrics
   ind <- sbc.sample$prop != 0
   prop_f <- sbc.frame$prop[ind]
   prop_s <- sbc.sample$prop[ind]
   J_subp <- sum(prop_s * log(prop_s))/sum(prop_f * log(prop_f))
   J_subp <- min(J_subp, 1)
   ind <- sbc.frame$prop != 0
   prop_f <- sbc.frame$prop[ind]
   prop_s <- sbc.sample$prop[ind]
   chi_sq <- sum(((prop_s - prop_f)^2)/prop_f)

# Create the output list
   sbc <- list(J_subp=J_subp, chi_sq=chi_sq, extent=sbc.sample$extent,
               prop=sbc.sample$prop)

# Metrics calculated using a rectangular grid were not requested
} else {
   sbc <- NULL
}

# Return results
list(tess=tess, sbc=sbc)
}
