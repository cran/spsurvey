################################################################################
# Demo: Area
# Purpose: Example GRTS Survey Designs for an Area Resource
# Programmer: Tom Kincaid
# Date: May 20, 2005
# Last Revised: May 10, 2006
# Description: This demonstration provides example GRTS survey designs for an
#   area resource: Omernik level 3 ecoregions within Utah.
################################################################################

# Determine the path to the source file for the demo

invisible(sourcefile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\eco_l3_ut.s", sep=""))

# Source the file

invisible(source(sourcefile))
