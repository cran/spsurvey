################################################################################
# Demo: Finite
# Purpose: Example GRTS Survey Designs for a Finite Resource
# Programmers: Tony Olsen, Tom Kincaid
# Date: April 29, 2005
# Last Revised: May 9, 2006
# Description: This demonstration provides example GRTS survey designs for a
#   finite resource: New England lakes.  Lake locations are given by their
#   centroid.  Data is originally from the National Hydrology Database.
################################################################################

cat("\nThis demonstration presents example GRTS survey designs for a finite resource:\n   New England lakes\n\n")

# Determine the path to the shapefile

shpfile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\reg1_lakes", sep="")

# Read the attribute table from the shapefile

att <- read.dbf(shpfile)
names(att) <- tolower(names(att))

# Display the initial six lines in the attribute data frame

cat("The initial six lines in the attribute data frame follow:\n\n")
print(head(att))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Most column names in the attribute data frame are self-explanatory:
#    lakres_alb_ - unique ID for each lake
#    ftype - identifies as lake or reservoir
#    sq_km - lake area in sq km
#    x_coord - Albers projection x-coordinate
#    y_coord - Albers projection y-coordinate
#    level3 - Omernik Level 3 ecoregion
#    level2 - Omernik Level 2 ecoregion
#    level1 - Omernik Level 1 ecoregion

# Convert lake area to hectares, print summary information for lake area, and
# provide histograms of lake area

att$area.ha <- att$sq_km*100

cat("\nA summary of lake area (hectares) follows:\n\n")
print(summary(att$area.ha))

cat("\nThe histogram displays the log of lake area\n")
hist(log10(att$area.ha), breaks=100, main="Histogram of log (Lake Area)",
     xlim=c(-1, 5), xlab="log (Lake Area (ha))")
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Produce tables for lake type, spatial pattern, and ecoregion

cat("\nA table displaying the number of each lake type follows:\n")
print(table(att$ftype))

cat("\nA table displaying the number of lakes for each state follows:\n")
print(table(att$st))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

cat("\nA table displaying the number of lakes for each Omernik Level 1 ecoregion follows:\n")
print(table(att$level1))

cat("\nA table displaying the number of lakes for each Omernik Level 2 ecoregion follows:\n")
print(table(att$level2))

cat("\nA table displaying the number of lakes for each Omernik Level 3 ecoregion follows:\n")
print(table(att$level3_nam))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

#  Four example survey designs follow

cat("\nFour example GRTS survey designs can be presented subject to your choice.\n\n")

# Equal Probability GRTS survey design for all lakes and reservoirs

cat("Example One: Equal probability GRTS survey design for all lakes and\n             reservoirs\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Equaldsgn <- list(None=list(panel=c(PanelOne=300), seltype='Equal'))\n\n")
Equaldsgn <- list(None=list(panel=c(PanelOne=300), seltype='Equal'))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(8953106) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Equalsites <- grts(design=Equaldsgn,
                   DesignID='EQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Equalsites <- grts(design=Equaldsgn,
                   DesignID='EQUAL',
                   type.frame='finite',
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

# Stratified GRTS survey design for all lakes and reservoirs
# Stratify by state

cat("\nExample Two: Stratified GRTS survey design for all lakes and reservoirs\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Stratdsgn <- list(CT=list(panel=c(PanelOne=50), seltype='Equal'),
                  MA=list(panel=c(PanelOne=50), seltype='Equal'),
                  ME=list(panel=c(PanelOne=50), seltype='Equal'),
                  NH=list(panel=c(PanelOne=50), seltype='Equal'),
                  RI=list(panel=c(PanelOne=50), seltype='Equal'),
                  VT=list(panel=c(PanelOne=50), seltype='Equal'))\n\n")
Stratdsgn <- list(CT=list(panel=c(PanelOne=50), seltype='Equal'),
                  MA=list(panel=c(PanelOne=50), seltype='Equal'),
                  ME=list(panel=c(PanelOne=50), seltype='Equal'),
                  NH=list(panel=c(PanelOne=50), seltype='Equal'),
                  RI=list(panel=c(PanelOne=50), seltype='Equal'),
                  VT=list(panel=c(PanelOne=50), seltype='Equal'))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(5540615) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Stratsites <- grts(design=Stratdsgn,
                   DesignID='STRATIFIED',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='st',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Stratsites <- grts(design=Stratdsgn,
                   DesignID='STRATIFIED',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   stratum='st',
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

# Unequal probability GRTS survey design based on lake area with an oversample

cat("\nExample Three: Unequal probability GRTS survey design based on lake area with an\n               oversample\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Calculate lake area categories

cat("\nThe code that creates the lake area categoriese follows:\n
att$lake.area.cat <- cut(att$area.ha, breaks=c(0,1,5,10,50,500,70000),
   include.lowest=TRUE)\n\n")
att$lake.area.cat <- cut(att$area.ha, breaks=c(0,1,5,10,50,500,70000),
   include.lowest=TRUE)

cat("\nA summary of lake area categories follows:\n\n")
print(summary(att$lake.area.cat))


# Create the design list

cat("\nThe code that creates the design list follows:\n
Unequaldsgn <- list(None=list(panel=c(PanelOne=300),
                           seltype='Unequal',
                           caty.n=c('[0,1]'=50, '(1,5]'=50, '(5,10]'=50,
                                    '(10,50]'=50, '(50,500]'=50,
                                    '(500,7e+04]'=50),
                           over=120))\n\n")
Unequaldsgn <- list(None=list(panel=c(PanelOne=300),
                              seltype='Unequal',
                              caty.n=c('[0,1]'=50, '(1,5]'=50, '(5,10]'=50,
                                       '(10,50]'=50, '(50,500]'=50,
                                       '(500,7e+04]'=50),
                              over=120))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(1524726) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Unequalsites <- grts(design=Areadsgn,
                   DesignID='UNEQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   mdcaty='lake.area.cat',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Unequalsites <- grts(design=Areadsgn,
                   DesignID='UNEQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   mdcaty='lake.area.cat',
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

# Calculate lake area categories

cat("\nThe code that creates the lake area categoriese follows:\n
att$lake.area.cat <- cut(att$area.ha, breaks=c(0,1,5,10,50,500,70000),
   include.lowest=TRUE)\n\n")
att$lake.area.cat <- cut(att$area.ha, breaks=c(0,1,5,10,50,500,70000),
   include.lowest=TRUE)

cat("\nA summary of lake area categories follows:\n\n")
print(summary(att$lake.area.cat))

# Create the design list

cat("\nThe code that creates the design list follows:\n
Paneldsgn <- list(None=list(panel=c(Annual=50, Year1=50, Year2=50, Year3=50,
                               Year4=50, Year5=50),
                            seltype='Unequal',
                            caty.n=c('[0,1]'=50, '(1,5]'=50, '(5,10]'=50,
                                     '(10,50]'=50, '(50,500]'=50,
                                     '(500,7e+04]'=50),
                            over=120))\n\n")
Paneldsgn <- list(None=list(panel=c(Annual=50, Year1=50, Year2=50, Year3=50,
                               Year4=50, Year5=50),
                            seltype='Unequal',
                            caty.n=c('[0,1]'=50, '(1,5]'=50, '(5,10]'=50,
                                     '(10,50]'=50, '(50,500]'=50,
                                     '(500,7e+04]'=50),
                            over=120))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(9955615) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Panelsites <- grts(design=Paneldsgn,
                   DesignID='UNEQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   mdcaty='lake.area.cat',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Panelsites <- grts(design=Paneldsgn,
                   DesignID='UNEQUAL',
                   type.frame='finite',
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   mdcaty='lake.area.cat',
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
