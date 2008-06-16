grtsarea <- function (shapefilename=NULL, areaframe, samplesize=100,
   SiteBegin=1, shift.grid=TRUE, startlev=NULL, maxlev=11, maxtry=1000,
   acept=1){

################################################################################
# Function: grtsarea
# Purpose: Select a generalized random-tesselation stratified (GRTS) sample of
#    an area resource
# Programmers: Tony Olsen, Tom Kincaid, Don Stevens, Christian Platt,
#   			Denis White, Richard Remington
# Date: May 19, 2004
# Last Revised: December 18, 2006
# Description:      
#   This function select a GRTS sample of an area resource.  The function uses
#   hierarchical randomization to ensure that the sample will include no more
#   than one point per cell and then picks a point in selected cells.  
# Arguments:
#   shapefilename = name of the input shapefile.  If shapefilename equals NULL,
#     then the shapefile or shapefiles in the working directory are used.  The
#     default is NULL.
#   areaframe = a data frame containing id, mdcaty and mdm.
#   samplesize = number of points to select in the sample.  The default is 100.
#   SiteBegin = first number to start siteID numbering.  The default is 1.
#   shift.grid = the option to randomly shift the hierarchical grid.  The
#     default is TRUE.
#   startlev = initial number of hierarchical levels to use for the GRTS grid,
#     which must be less than or equal to maxlev (if maxlev is specified) and
#     cannot be greater than 11.  The default is NULL.
#   maxlev = maximum number of hierarchical levels to use for the GRTS grid,
#     which cannot be greater than 11.  The default is 11.
#   maxtry = maximum number of iterations for randomly generating a point within
#     a grid cell to select a site when type.frame equals "area".  The default
#     is 1000.
#   acept = parameter in function ftnt.inv.fcn that controls peakedness of a
#     center-peaked distribution, which must be between 0 and 1.  A value of 0
#     gives a triangular distribution, and a value of 1 gives a uniform
#     distribution.  The default is 1.
# Results: 
#   A data frame of sample points containing: siteID, id, x, y, mdcaty,
#     and weight.
# Other Functions Required:
#   pointInPolygonObj - C function to determine which points in a set of points
#     are located within a specified polygon
#   numLevels - C function to determine the number of levels for hierarchical
#     randomization
#   constructAddr - C function to construct the hierarchical address for all
#     points
#   ranho - C function to construct the randomized hierarchical address for all
#     points
#   ftnt.inv.fcn - calculates the inverse distribution function
#   pickGridCells - C function to select grid cells that get a sample point
#   pointInPolygonFile - C function to determine the polygon IDs and the
#     probability values associated with a set of point, where polygons are
#     specified by a shapefile
################################################################################

# Determine the number of levels for hierarchical randomization

   temp <- .Call("numLevels", shapefilename, samplesize, shift.grid,
      startlev, maxlev, areaframe$id, areaframe$mdm)
   if(is.null(temp[[1]]))
      stop("\nAn error occured while determining the number of levels for hierarchical \nrandomization.") 
   nlev <- temp$nlev
   dx <- temp$dx
   dy <- temp$dy
   xc <- temp$xc
   yc <- temp$yc
   cel.wt <- temp$cel.wt
   sint <- temp$sint

# Remove cells with zero weight

   indx <- cel.wt > 0
   xc <- xc[indx]
   yc <- yc[indx]
   cel.wt <- cel.wt[indx]

# Construct the hierarchical address for all cells

   hadr <- .Call("constructAddr", xc, yc, dx, dy, nlev)

# Construct randomized hierarchical addresses

   ranhadr <- .C("ranho", hadr, as.integer(length(hadr)))[[1]]

# Determine the reverse hierarchical ordering for the randomized hierarchical
# addresses

   rord <- order(ranhadr)

# Select grid cells that get a sample point
        
   rstrt <- runif(1, 0, sint)
   ttl.wt <- c(0, cumsum(cel.wt[rord]))
   idx <- ceiling((ttl.wt - rstrt)/sint)
   smpdx <- .Call("pickGridCells", samplesize, as.integer(idx))
   rdx <- rord[smpdx]
   n.cells <- length(unique(rdx))
   if(length(rdx) > n.cells) {
      temp <- sum(sapply(split(rdx, rdx), length) > 1)
      warning(paste("\nOf the ", n.cells, " grid cells from which sample points were selected,\n", temp, " (", round(100*temp/n.cells, 1), "%) of the cells contained more than one sample point.\n", sep=""))
   }

# Pick a sample point in selected cells

   xcs <- ycs <- id <- prb <-numeric(samplesize)
   bp <- !logical(samplesize)
   ntry <- 0    
   while(any(bp) & ntry < maxtry) {
      xcs[bp] <- xc[rdx[bp]] - ftnt.inv.fcn(runif(sum(bp)), acept)*dx
      ycs[bp] <- yc[rdx[bp]] - ftnt.inv.fcn(runif(sum(bp)), acept)*dy
      temp <- .Call("pointInPolygonFile", shapefilename, xcs[bp], ycs[bp],
         areaframe$id, areaframe$mdm)
      id[bp] <- temp$id
      prb[bp] <- temp$mdm
      ntry <- ntry + 1
      bp <- prb == 0
   }

# When the achieved sample size is less than the desired sample size, remove
# values from the output vectors

   if(sum(!bp) < samplesize) {
      xcs <- xcs[!bp]
      ycs <- ycs[!bp]
      id <- id[!bp]
      prb <- prb[!bp]
      samplesize <- sum(!bp)
   }

# Construct sample line in reverse hierarchical order
 
   nlv4 <- ceiling(logb(samplesize, 4))
   rho <- matrix(0, 4^nlv4, nlv4)
   rv4 <- 0:3
   pwr4 <- 4^(0:(nlv4 - 1))
   for(i in 1:nlv4)
      rho[, i] <- rep(rep(rv4, rep(pwr4[i], 4)),pwr4[nlv4]/pwr4[i])
   rho4 <- rho%*%matrix(rev(pwr4), nlv4, 1)

# Place weighted points on line in reverse hierarchical order 

   rh.ord <- unique(floor(rho4 * samplesize/4^nlv4)) + 1
   id <- id[rh.ord]
   x <- xcs[rh.ord]
   y <- ycs[rh.ord]
   mdcaty <- areaframe$mdcaty[match(id, areaframe$id)]
   mdm <- prb[rh.ord]

# Assign Site ID

   siteID <- SiteBegin - 1 + 1:length(rh.ord)


# Place Site ID as first column and add weights

   rho <- data.frame(siteID=siteID, id=id, xcoord=x, ycoord=y, mdcaty=mdcaty,
      wgt=1/mdm)
   row.names(rho) <- 1:nrow(rho)

# Assign the final number of levels as an attribute of the output data frame

   attr(rho, "nlev") <- nlev - 1

# Return the sample

   rho
}
