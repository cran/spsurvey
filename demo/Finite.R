################################################################################
# Demo: Finite
# Purpose: Example GRTS Survey Designs for a Finite Resource
# Programmers: Tony Olsen, Tom Kincaid
# Date: May 20, 2005
# Last Revised: May 10, 2006
# Description: This demonstration provides example GRTS survey designs for a
#   finite resource: New England lakes.  Lake locations are given by their
#   centroid.  Data is originally from the National Hydrology Database.
################################################################################

# Determine the path to the source file for the demo

invisible(sourcefile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\reg1_lakes.s", sep=""))

# Source the file

invisible(source(sourcefile, print.eval=TRUE))
