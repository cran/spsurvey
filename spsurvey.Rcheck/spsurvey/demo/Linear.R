################################################################################
# Demo: Linear
# Purpose: Example GRTS Survey Designs for a Linear Resource
# Programmers: Tony Olsen, Tom Kincaid
# Date: May 20, 2005
# Last Revised: May 10, 2006
# Description: This demonstration provides example GRTS survey designs for a
#   linear resource: Lukiamute Watershed Council streams.  Thestream network is
#   in Albers projection (see the projection file).
################################################################################

# Determine the path to the source file for the demo

invisible(sourcefile <- paste(Sys.getenv("R_HOME"),
   "\\library\\spsurvey\\data\\luck-ash.s", sep=""))

# Source the file

invisible(source(sourcefile))
