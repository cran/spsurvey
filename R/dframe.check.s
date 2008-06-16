dframe.check <- function(sites, design, subpop, data.cat, data.cont,
   design.names) {

################################################################################
# Function: dframe.check
# Programmer: Tom Kincaid
# Date: September 26, 2003
# Last Revised: December 5, 2007
# Description:
#   This function checks site IDs, the sites data frame, the subpop data
#      frame, the data.cat data frame, and the data.cont data frame to assure
#      valid contents.  If they do not exist, then the sites data frame and the
#      subpop data frame are created.
#   Input:
#      design = the design data frame.
#      sites = the sites data frame.
#      subpop = the subpop data frame.
#      data.cat = the data.cat data frame of categorical response variables.
#      data.cont = the data.cont data frame of continuous response variables.
#      design.names = names for the design data frame.
#   Output:
#      A list consisting of the sites data frame, design data frame, subpop data
#      frame, data.cat data frame, and data.cont data frame.
#   Other Functions Required:
#      uniqueID - creates unique site IDs by appending a unique number to
#         each occurrence of a site ID
#      vecprint - takes an input vector and outputs a character string with
#         line breaks inserted
################################################################################

# Check the sites data frame for contents

   if(is.null(sites)) {
      sites <- data.frame(siteID=design$siteID, use.sites=rep(TRUE, dim(design)[1]))
   } else {
      if(!is.data.frame(sites))
         stop("\nThe sites argument must be a data frame.")
      if(dim(sites)[2] != 2)
         stop("\nThe sites argument must contain exactly two variables.")
      if(!is.logical(sites[,2]))
         stop("\nThe second variable in the sites data frame was not a logical variable.")
   }
   names(sites)[1] <- design.names[1]

# Check the sites data frame to determine whether there are multiple
# occurrences of site IDs among the sites that will be analyzed and, as
# necessary, create unique site IDs

   repeatID.ind <- FALSE
   siteID.r <- sapply(split(sites$siteID, sites$siteID), length)
   if(any(siteID.r > 1)) {
      repeatID.ind <- TRUE
      siteID.u <- uniqueID(sites$siteID)
   } else {
      siteID <- sites$siteID[sites[,2]]
   }

# Check the design data frame for contents

   if(repeatID.ind) {
      temp <- match(siteID.u, uniqueID(design$siteID), nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not have the same number \nof occurrences as the site ID values in the design data frame:\n", temp.str, sep=""))
      }
      design <- design[temp,]
      design <- design[sites[,2],]
   } else {
      temp <- match(siteID, design$siteID, nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the design data frame:\n", temp.str, sep=""))
      }
      design <- design[temp,]
   }

# Check the subpop data frame for contents

   if(is.null(subpop)) {
      subpop <- data.frame(siteID=sites$siteID[sites[,2]],
        all.sites=factor(rep("All Sites", sum(sites[,2]))))
   } else {
      if(!is.data.frame(subpop))
         stop("\nThe subpop argument must be a data frame.")
      if(dim(subpop)[2] < 2)
         stop("\nThe subpop argument must contain at least two variables.")
      if(repeatID.ind) {
         temp <- match(siteID.u, uniqueID(subpop[,1]), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not have the same number \nof occurrences as the site ID values in the subpop data frame:\n", temp.str, sep=""))
         }
         subpop <- subpop[temp,]
         subpop <- subpop[sites[,2],]
      } else {
         temp <- match(siteID, subpop[,1], nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the subpop data frame:\n", temp.str, sep=""))
         }
         subpop <- subpop[temp,]
      }
   }
   names(subpop)[1] <- design.names[1]

# Check the data.cat data frame for contents

   if(!is.null(data.cat)) {
      if(!is.data.frame(data.cat))
         stop("\nThe data.cat argument must be a data frame.")
      if(repeatID.ind) {
         temp <- match(siteID.u, uniqueID(data.cat[,1]), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not have the same number \nof occurrences as the site ID values in the data.cat data frame:\n", temp.str, sep=""))
         }
         data.cat <- data.cat[temp,]
         data.cat <- data.cat[sites[,2],]
      } else {
         temp <- match(siteID, data.cat[,1], nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the data.cat data frame:\n", temp.str, sep=""))
         }
         data.cat <- data.cat[temp,]
      }
      names(data.cat)[1] <- design.names[1]
   }

# Check the data.cont data frame for contents

   if(!is.null(data.cont)) {
      if(!is.data.frame(data.cont))
         stop("\nThe data.cont argument must be a data frame.")
      if(repeatID.ind) {
         temp <- match(siteID.u, uniqueID(data.cont[,1]), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not have the same number \nof occurrences as the site ID values in the data.cont data frame:\n", temp.str, sep=""))
         }
         data.cont <- data.cont[temp,]
         data.cont <- data.cont[sites[,2],]
      } else {
         temp <- match(siteID, data.cont[,1], nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(unique(siteID[temp == 0]))
            stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the data.cont data frame:\n", temp.str, sep=""))
         }
         data.cont <- data.cont[temp,]
      }
      names(data.cont)[1] <- design.names[1]
   }

# Return the list

   list(sites=sites[sites[,2],], design=design, subpop=subpop,
        data.cat=data.cat, data.cont=data.cont)
}
