################################################################################
# Demo: Linear
# Purpose: Example GRTS Survey Designs for a Linear Resource
# Programmers: Tony Olsen, Tom Kincaid
# Date: April 29, 2005
# Last Revised: May 9, 2006
# Description: This demonstration provides example GRTS survey designs for a
#   linear resource: Lukiamute Watershed Council streams.  The stream network is
#   in Albers projection (see the projection file).
################################################################################

cat("\nThis demonstration presents example GRTS survey designs for a finite resource:\n   Lukiamute Watershed Council streams\n\n")

# Determine the path to the shapefile

shpfile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\luck-ash", sep="")

# Read the attribute table from the shapefile

att <- read.dbf(shpfile)
names(att) <- tolower(names(att))

# Display the initial six lines in the attribute data frame

cat("The initial six lines in the attribute data frame follow:\n\n")
print(head(att))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Examine attributes that will be used to define strata and unequal probability
# categories

cat("\nA table displaying the number of streams for each stream type follows\n   Note: 0=perennial stream and 610=intermittent stream\n")
print(table(att$minor2))

cat("\nA table displaying the number of streams for each Strahler stream order follows\n   Note: 1=headwater stream\n")
print(table(att$strahler))

# Summarize frame stream length (km) by mdcaty

allsize <- sum(att$length)
strsize <- tapply(att$length, att$strahler, sum)
mnsize <- tapply(att$length, att$minor2, sum)
strmnsize <- tapply(att$length, list(att$strahler, att$minor2), sum)
rslt <- rbind(cbind(strmnsize, Sum=strsize), Sum=c(mnsize, allsize))/1000
names(dimnames(rslt)) <- list("Strahler", "Stream Type")
cat("\nA table displaying the cross-tabulation of sum of stream length for Strahler \nstream order and stream type follows:\n\n")
print(rslt)
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

#  Four example survey designs follow

cat("\nFour example GRTS survey designs can be presented subject to your choice.\n\n")

# Equal probability GRTS survey design

cat("Example One: Equal probability GRTS survey design\n\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Equaldsgn <- list(None=list(panel=c(Panel_1=50), seltype='Equal'))\n\n")
Equaldsgn <- list(None=list(panel=c(Panel_1=50), seltype='Equal'))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(19742003)  # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Equalsites <- grts(design=Equaldsgn,
                   DesignID='EQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Equalsites <- grts(design=Equaldsgn,
                   DesignID='EQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(as.data.frame(Equalsites@data@att)))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Equalsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Stratified GRTS survey design with an oversample

cat("\nExample Two: Stratified GRTS survey design with an oversample\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the strata variable

cat("\nThe code that creates the strata variable follows:\n
att$PerInt <- as.factor(att$minor2)
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')\n\n")
att$PerInt <- as.factor(att$minor2)
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')

# Create the design list

cat("\nThe code that creates the design list follows:\n
Stratdsgn <- list(Perennial=list(panel=c(Panel_1=50),
                                 seltype='Equal',
                                 over=50),
                  Intermittent=list(panel=c(Panel_1=50),
                                    seltype='Equal',
                                    over=50))\n\n")
Stratdsgn <- list(Perennial=list(panel=c(Panel_1=50),
                                 seltype='Equal',
                                 over=50),
                  Intermittent=list(panel=c(Panel_1=50),
                                    seltype='Equal',
                                    over=50))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(99333079) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Stratsites <- grts(design=Stratdsgn,
                   DesignID='STRATIFIED',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Stratsites <- grts(design=Stratdsgn,
                   DesignID='STRATIFIED',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(as.data.frame(Stratsites@data@att)))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Stratsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Unequal probability GRTS survey design with an oversample and stratification

cat("\nExample Three: Unequal probability GRTS survey design with an oversample and stratification\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the strata variable

cat("\nThe code that creates the strata variable follows:\n
att$PerInt <- as.factor( att$minor2 )
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')\n\n")
att$PerInt <- as.factor( att$minor2 )
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')

# Create Strahler categories for an unequal probability design

cat("\nThe code that creates the Strahler categories for an unequal probability design\n follows:\n
att$strahcat <- as.factor(att$strahler)
levels(att$strahcat) <- list('1st'=c('0','1'), '2nd'='2', '3rd+'=c('3','4','5'))\n\n")
att$strahcat <- as.factor(att$strahler)
levels(att$strahcat) <- list('1st'=c('0','1'), '2nd'='2', '3rd+'=c('3','4','5'))

# Create the design list

cat("\nThe code that creates the design list follows:\n
Unequaldsgn <- list(Perennial=list(panel=c(Panel_1=75),
                                   seltype='Unequal',
                                   caty.n=c('1st'=25, '2nd'=25, '3rd+'=25),
                                   over=36),
                    Intermittent=list(panel=c(Panel_1=25),
                                      seltype='Unequal',
                                      caty.n=c('1st'=17, '2nd'=5, '3rd+'=3),
                                      over=0))\n\n")
Unequaldsgn <- list(Perennial=list(panel=c(Panel_1=75),
                                   seltype='Unequal',
                                   caty.n=c('1st'=25, '2nd'=25, '3rd+'=25),
                                   over=36),
                    Intermittent=list(panel=c(Panel_1=25),
                                      seltype='Unequal',
                                      caty.n=c('1st'=17, '2nd'=5, '3rd+'=3),
                                      over=0))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(22193811) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Unequalsites <- grts(design=Unequaldsgn,
                   DesignID='UNEQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   mdcaty='strahcat',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Unequalsites <- grts(design=Unequaldsgn,
                   DesignID='UNEQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   mdcaty='strahcat',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(as.data.frame(Unequalsites@data@att)))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Unequalsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Unequal probability GRTS survey design with an oversample and a panel
# structure for survey over time

cat("\nExample Four: Unequal probability GRTS survey design with an oversample and a\n              panel structure for survey over time\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the strata variable

cat("\nThe code that creates the strata variable follows:\n
att$PerInt <- as.factor( att$minor2 )
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')\n\n")
att$PerInt <- as.factor( att$minor2 )
levels(att$PerInt) <- list(Perennial='0', Intermittent='610')

# Create Strahler categories for an unequal probability design

cat("\nThe code that creates the Strahler categories for an unequal probability design\n follows:\n
att$strahcat <- as.factor(att$strahler)
levels(att$strahcat) <- list('1st'=c('0','1'), '2nd'='2', '3rd+'=c('3','4','5'))\n\n")
att$strahcat <- as.factor(att$strahler)
levels(att$strahcat) <- list('1st'=c('0','1'), '2nd'='2', '3rd+'=c('3','4','5'))

# Create the design list

cat("\nThe code that creates the design list follows:\n
Paneldsgn <- list(Perennial=list(panel=c(Year1=17, Year2=17, YearAll=16),
                                 seltype='Unequal',
                                 caty.n=c('1st'=15, '2nd'=15, '3rd+'=20 ),
                                 over=50),
                  Intermittent=list(panel=c(YearOnce=25),
                                    seltype='Unequal',
                                    caty.n=c('1st'=17, '2nd'=5, '3rd+'=3 ),
                                    over=0))\n\n")
Paneldsgn <- list(Perennial=list(panel=c(Year1=17, Year2=17, YearAll=16),
                                 seltype='Unequal',
                                 caty.n=c('1st'=15, '2nd'=15, '3rd+'=20 ),
                                 over=50),
                  Intermittent=list(panel=c(YearOnce=25),
                                    seltype='Unequal',
                                    caty.n=c('1st'=17, '2nd'=5, '3rd+'=3 ),
                                    over=0))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(22193811) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Panelsites <- grts(design=Paneldsgn,
                   DesignID='UNEQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   mdcaty='strahcat',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Panelsites <- grts(design=Paneldsgn,
                   DesignID='UNEQUAL',
                   type.frame='linear',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='PerInt',
                   mdcaty='strahcat',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(as.data.frame(Panelsites@data@att)))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Panelsites))

}


