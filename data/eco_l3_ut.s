################################################################################
# Demo: Area
# Purpose: Example GRTS Survey Designs for an Area Resource
# Programmers: Tony Olsen, Tom Kincaid
# Date: April 29, 2005
# Last Revised: July 17, 2008
# Description: This demonstration provides example GRTS survey designs for an
#   area resource - Omernik level 3 ecoregions within Utah.
################################################################################

cat("\nThis demonstration presents example GRTS survey designs for an area resource:\n   Omernik level 3 ecoregions within Utah\n\n")

# Determine the path to the shapefile

shpfile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\eco_l3_ut", sep="")

# Read the attribute table from the shapefile

att <- read.dbf(shpfile)
names(att) <- tolower(names(att))

# Display the initial six lines in the attribute data frame

cat("The initial six lines in the attribute data frame follow:\n\n")
print(head(att))

# Display names of the ecoregions attribute that will be used to define
# multidensity categories (mdcaty) and strata

cat("\nNames of the Omernik Level 3 ecoregions follow:\n\n")
cat(vecprint(levels(att$level3_nam)), "\n")
invisible(readline(prompt="Press Enter to continue."))

#  Four example survey designs follow

cat("\nFour example GRTS survey designs can be presented subject to your choice.\n\n")

# Equal probability GRTS survey design

cat("Example One: Equal probability GRTS survey design\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Equaldsgn <- list(None=list(panel=c(PanelOne=115), seltype='Equal'))\n\n")
Equaldsgn <- list(None=list(panel=c(PanelOne=115), seltype='Equal'))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(4447864)  # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Equalsites <- grts(design=Equaldsgn,
                   src.frame='shapefile',
                   in.shape=shpfile,
                   att.frame=att,
                   type.frame='area',
                   DesignID='UTEco3EQ',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Equalsites <- grts(design=Equaldsgn,
                   src.frame='shapefile',
                   in.shape=shpfile, 
                   att.frame=att,
                   type.frame='area',
                   DesignID='UTEco3EQ',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(Equalsites@data))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Equalsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Unequal probability GRTS survey design

cat("\nExample Two: Unequal probability GRTS survey design\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Unequaldsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype='Unequal',
                              caty.n=c('Central Basin and Range'=25,
                                       'Colorado Plateaus'=25,
                                       'Mojave Basin and Range'=10,
                                       'Northern Basin and Range'=10,
                                       'Southern Rockies'=10,
                                       'Wasatch and Uinta Mountains'=25,
                                       'Wyoming Basin'=10)))\n\n")
Unequaldsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype='Unequal',
                              caty.n=c('Central Basin and Range'=25,
                                       'Colorado Plateaus'=25,
                                       'Mojave Basin and Range'=10,
                                       'Northern Basin and Range'=10,
                                       'Southern Rockies'=10,
                                       'Wasatch and Uinta Mountains'=25,
                                       'Wyoming Basin'=10)))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(99333079) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Unequalsites <- grts(design=Unequaldsgn,
                     src.frame='shapefile',
                     in.shape=shpfile,  
                     att.frame=att,
                     type.frame='area',
                     mdcaty='level3_nam',									
                     DesignID='UTEco3UN',
                     shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Unequalsites <- grts(design=Unequaldsgn,
                     src.frame='shapefile',
                     in.shape=shpfile,  
                     att.frame=att,
                     type.frame='area',
                     mdcaty='level3_nam',									
                     DesignID='UTEco3UN',
                     shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(Unequalsites@data))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Unequalsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Stratified GRTS survey design

cat("\nExample Three: Stratified GRTS survey design\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Stratdsgn <- list('Central Basin and Range'=list(panel=c(PanelOne=25),
                                                 seltype='Equal'),
                  'Colorado Plateaus'=list(panel=c(PanelOne=25),
                                           seltype='Equal'),
                  'Mojave Basin and Range'=list(panel=c(PanelOne=10),
                                                seltype='Equal'),
                  'Northern Basin and Range'=list(panel=c(PanelOne=10),
                                                  seltype='Equal'),
                  'Southern Rockies'=list(panel=c(PanelOne=10),
                                          seltype='Equal'),
                  'Wasatch and Uinta Mountains'=list(panel=c(PanelOne=25),
                                                     seltype='Equal'),
                  'Wyoming Basin'=list(panel=c(PanelOne=10),
                                       seltype='Equal'))\n\n")
Stratdsgn <- list('Central Basin and Range'=list(panel=c(PanelOne=25),
                                                 seltype='Equal'),
                  'Colorado Plateaus'=list(panel=c(PanelOne=25),
                                           seltype='Equal'),
                  'Mojave Basin and Range'=list(panel=c(PanelOne=10),
                                                seltype='Equal'),
                  'Northern Basin and Range'=list(panel=c(PanelOne=10),
                                                  seltype='Equal'),
                  'Southern Rockies'=list(panel=c(PanelOne=10),
                                          seltype='Equal'),
                  'Wasatch and Uinta Mountains'=list(panel=c(PanelOne=25),
                                                     seltype='Equal'),
                  'Wyoming Basin'=list(panel=c(PanelOne=10),
                                       seltype='Equal'))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(99333079) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Stratsites <- grts(design=Stratdsgn,
                   src.frame='shapefile',
                   in.shape=shpfile,  
                   att.frame=att,
                   type.frame='area',
                   stratum='level3_nam',									
                   DesignID='UTEco3ST',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Stratsites <- grts(design=Stratdsgn,
                   src.frame='shapefile',
                   in.shape=shpfile,  
                   att.frame=att,
                   type.frame='area',
                   stratum='level3_nam',									
                   DesignID='UTEco3ST',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(Stratsites@data))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Stratsites))
invisible(readline(prompt="Press Enter to continue."))

}

# Unequal probability GRTS survey design with an oversample and a panel
# structure for survey over time

cat("\nExample Four: Unequal probability GRTS survey design with an oversample and a\n              panel structure for survey over time\n")
switch(menu(c("Select the survey design", "Do not select the survey design")),
   ind <- TRUE, ind <- FALSE)

if(ind) {

# Create the design list

cat("\nThe code that creates the design list follows:\n
Paneldsgn <- list(None=list(panel=c(Panel_1=50, Panel_2=50, Panel_3=50,
                                    Panel_4=50, Panel_5=50),
                            seltype='Unequal',
                            caty.n=c('Central Basin and Range'=64,
                                     'Colorado Plateaus'=63,
                                     'Mojave Basin and Range'=15,
                                     'Northern Basin and Range'=15,
                                     'Southern Rockies'=15,
                                     'Wasatch and Uinta Mountains'=63,
                                     'Wyoming Basin'=15),
                            over=100))\n\n")
Paneldsgn <- list(None=list(panel=c(Panel_1=50, Panel_2=50, Panel_3=50,
                                    Panel_4=50, Panel_5=50),
                            seltype='Unequal',
                            caty.n=c('Central Basin and Range'=64,
                                     'Colorado Plateaus'=63,
                                     'Mojave Basin and Range'=15,
                                     'Northern Basin and Range'=15,
                                     'Southern Rockies'=15,
                                     'Wasatch and Uinta Mountains'=63,
                                     'Wyoming Basin'=15),
                            over=100))

# Select the sample

dsgntime <- proc.time()[3]
set.seed(68718760) # Use set seed so that the same sample always is selected
cat("The call to function 'grts' follows:\n
Panelsites <- grts(design=Paneldsgn,
                   src.frame='shapefile',
                   in.shape=shpfile,  
                   att.frame=att,
                   type.frame='area',
                   mdcaty='level3_nam',									
                   DesignID='UTEco3Pan',
                   shapefile=FALSE)\n\n")
invisible(readline(prompt="Press Enter to continue."))
cat("\nThe following information is printed by 'grts' while it is executing:\n")
Panelsites <- grts(design=Paneldsgn,
                   src.frame='shapefile',
                   in.shape=shpfile,  
                   att.frame=att,
                   type.frame='area',
                   mdcaty='level3_nam',									
                   DesignID='UTEco3Pan',
                   shapefile=FALSE)
dsgntime <- round((proc.time()[3] - dsgntime)/60, 2)
cat("\nThe time in minutes to complete the design:", dsgntime, "\n\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the initial six lines of the survey design

cat("\nThe initial six lines of the survey design follow:\n\n")
print(head(Panelsites@data))
cat("\n")
invisible(readline(prompt="Press Enter to continue."))

# Print the survey design summary

cat("\nThe survey design summary follows:\n\n")
print(dsgnsum(Panelsites))

}
