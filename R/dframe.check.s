dframe.check <- function(sites, design, subpop, data.cat, data.cont,
   data.rr, design.names) {

################################################################################
# Function: dframe.check
# Programmer: Tom Kincaid
# Date: September 26, 2003
# Last Revised: January 20, 2009
# Description:
#   This function checks site IDs, the sites data frame, the subpop data
#      frame, the data.cat data frame, the data.cont data frame, and the data.rr
#      data frame to assure valid contents.  If they do not exist, then the
#      sites data frame and the subpop data frame are created.
#   Input:
#      design = the design data frame.
#      sites = the sites data frame.
#      subpop = the subpop data frame.
#      data.cat = the data.cat data frame of categorical response variables.
#      data.cont = the data.cont data frame of continuous response variables.
#      data.rr = the data.rr data frame of categorical response and stressor
#                variables.
#      design.names = names for the design data frame.
#   Output:
#      A list consisting of the sites data frame, design data frame, subpop data
#      frame, data.cat data frame, and data.cont data frame.
#   Other Functions Required:
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
      temp <- is.na(sites[,1])
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(sites))[temp])
         stop(paste("\nThe following rows in the sites data frame contain missing site ID values:\n", temp.str, sep=""))
      }
      temp <- sapply(split(sites[,1], sites[,1]), length)
      if(any(temp > 1)) {
         temp.str <- vecprint(names(temp)[temp > 1])
         stop(paste("The following site ID values in the sites data frame occur more than \nonce:\n", temp.str, sep=""))
      }
      if(!is.logical(sites[,2]))
         stop("\nThe second variable in the sites data frame is not a logical variable.")
   }
   names(sites)[1] <- design.names[1]
   siteID <- sites$siteID[sites[,2]]

# Check the design data frame for contents

   temp <- is.na(design$siteID)
   if(any(temp)) {
      temp.str <- vecprint(seq(nrow(design))[temp])
      stop(paste("\nThe following rows in the design data frame contain missing site ID values:\n", temp.str, sep=""))
   }
   temp <- sapply(split(design$siteID, design$siteID), length)
   if(any(temp > 1)) {
      temp.str <- vecprint(names(temp)[temp > 1])
      stop(paste("The following site ID values in the design data frame occur more than \nonce:\n", temp.str, sep=""))
   }
   temp <- match(siteID, design$siteID, nomatch=0)
   if(any(temp == 0)) {
      temp.str <- vecprint(unique(siteID[temp == 0]))
      stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the design data frame:\n", temp.str, sep=""))
   }
   design <- design[temp,]

# Check the subpop data frame for contents

   if(is.null(subpop)) {
      subpop <- data.frame(siteID=sites$siteID[sites[,2]],
        all.sites=factor(rep("All Sites", sum(sites[,2]))))
   } else {
      if(!is.data.frame(subpop))
         stop("\nThe subpop argument must be a data frame.")
      if(dim(subpop)[2] < 2)
         stop("\nThe subpop argument must contain at least two variables.")
      temp <- is.na(subpop[,1])
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(subpop))[temp])
         stop(paste("\nThe following rows in the subpop data frame contain missing site ID values:\n", temp.str, sep=""))
      }
      temp <- sapply(split(subpop[,1], subpop[,1]), length)
      if(any(temp > 1)) {
         temp.str <- vecprint(names(temp)[temp > 1])
         stop(paste("The following site ID values in the subpop data frame occur more than \nonce:\n", temp.str, sep=""))
      }
      temp <- match(siteID, subpop[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the subpop data frame:\n", temp.str, sep=""))
      }
      subpop <- subpop[temp,]
   }
   names(subpop)[1] <- design.names[1]

# Check the data.cat data frame for contents

   if(!is.null(data.cat)) {
      if(!is.data.frame(data.cat))
         stop("\nThe data.cat argument must be a data frame.")
      temp <- is.na(data.cat[,1])
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(data.cat))[temp])
         stop(paste("\nThe following rows in the data.cat data frame contain missing site ID values:\n", temp.str, sep=""))
      }
      temp <- sapply(split(data.cat[,1], data.cat[,1]), length)
      if(any(temp > 1)) {
         temp.str <- vecprint(names(temp)[temp > 1])
         stop(paste("The following site ID values in the data.cat data frame occur more than \nonce:\n", temp.str, sep=""))
      }
      temp <- match(siteID, data.cat[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the data.cat data frame:\n", temp.str, sep=""))
      }
      data.cat <- data.cat[temp,]
      names(data.cat)[1] <- design.names[1]
   }

# Check the data.cont data frame for contents

   if(!is.null(data.cont)) {
      if(!is.data.frame(data.cont))
         stop("\nThe data.cont argument must be a data frame.")
      temp <- is.na(data.cont[,1])
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(data.cont))[temp])
         stop(paste("\nThe following rows in the data.cont data frame contain missing site ID values:\n", temp.str, sep=""))
      }
      temp <- sapply(split(data.cont[,1], data.cont[,1]), length)
      if(any(temp > 1)) {
         temp.str <- vecprint(names(temp)[temp > 1])
         stop(paste("The following site ID values in the data.cont data frame occur more than \nonce:\n", temp.str, sep=""))
      }
      temp <- match(siteID, data.cont[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the data.cont data frame:\n", temp.str, sep=""))
      }
      data.cont <- data.cont[temp,]
      names(data.cont)[1] <- design.names[1]
   }

# Check the data.rr data frame for contents

   if(!is.null(data.rr)) {
      if(!is.data.frame(data.rr))
         stop("\nThe data.rr argument must be a data frame.")
      temp <- is.na(data.rr[,1])
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(data.rr))[temp])
         stop(paste("\nThe following rows in the data.rr data frame contain missing site ID values:\n", temp.str, sep=""))
      }
      temp <- sapply(split(data.rr[,1], data.rr[,1]), length)
      if(any(temp > 1)) {
         temp.str <- vecprint(names(temp)[temp > 1])
         stop(paste("The following site ID values in the data.rr data frame occur more than \nonce:\n", temp.str, sep=""))
      }
      temp <- match(siteID, data.rr[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(unique(siteID[temp == 0]))
         stop(paste("\nThe following site ID values in the sites data frame do not occur among the \nsite ID values in the data.rr data frame:\n", temp.str, sep=""))
      }
      data.rr <- data.rr[temp,]
      names(data.rr)[1] <- design.names[1]
   }

# Return the list

   list(sites=sites[sites[,2],], design=design, subpop=subpop,
        data.cat=data.cat, data.cont=data.cont, data.rr=data.rr)
}
