dsgnsum <- function(sp.obj, auxvar=NULL) {

################################################################################
# Function: dsgnsum
# Purpose: Summarize the sites selected for a survey design
# Programmers: Tony Olsen, Tom Kincaid
# Date: April 26, 2005
# Last Revised: October 15, 2008
# Description:
#   This function summarizes the sites selected for a survey design by producing
#   contingency tables containing the cross-tabluation of number of sites for
#   survey design variables and, optionally, for auxiliary variables.
# Arguments:
#   sp.obj = the sp package object of class "SpatialPointsDataFrame" produced by
#     the grts function that contains survey design information and additional
#     attribute (auxiliary) variables.
#   auxvar = a vector containing the names of columns from sites that identify
#      auxiliary variables to be used to summarize the survey design. 
# Results:
#   A list containing two components named DesignSum and AuxVarSum.  DesignSum
#   is a list of contingency tables containing the cross-tabulation of number of
#   sites for the following combinations of survey design variables:
#     (1) multidensity category (mdcaty) and stratum
#     (2) stratum and panel 
#     (3) mdcaty, panel, and stratum
#   AuxVarSum is a list of contingency tables containing the cross-tabulation of
#   number of sites for each auxiliary variable and the design variables mdcaty,
#   panel, and stratum.
#   In addition the output list plus labeling information is printed to the
#   console.
# Other Functions Required:
#   vecprint - takes an input vector and outputs a character string with
#     line breaks inserted
# Examples:
#   test.design <- list(Stratum1=list(panel=c(PanelOne=50),
#      seltype="Equal", over=10), Stratum2=list(panel=c(PanelOne=50,
#      PanelTwo=50), seltype="Unequal", caty.n=c(CatyOne=25, CatyTwo=25,
#      CatyThree=25, CatyFour=25), over=75)
#   test.attframe <- read.dbf("test.shapefile")
#   test.sample <- grts(design=test.design, DesignID="Test.Site",
#      type.frame="area", src.frame="shapefile", in.shape="test.shapefile",
#      att.frame=test.attframe, stratum="test.stratum", mdcaty="test.mdcaty",
#      shapefile=TRUE, shapefilename="test.sample")
#   dsgnsum(test.sample, auxvar=c("test.ecoregion", "test.state"))
################################################################################

# Assign the sites data frame and the survey design information

   sites <- sp.obj@data
   design <- attributes(sp.obj@data)$design

# Determine whether multiple multidensity categories are present, whether the
# design is stratified, and whether multiple panels are present

   mdcaty.ind <- ifelse(length(unique(sites$mdcaty)) > 1, TRUE, FALSE)
   stratum.ind <- ifelse(length(unique(sites$stratum)) > 1, TRUE, FALSE)
   panel.ind <- ifelse(length(unique(sites$panel)) > 1, TRUE, FALSE)

# Determine whether the type of random selection is "Continuous" for any stratum

   seltype.ind <- FALSE
   for(i in 1:length(design)) {
      if(design[[i]]$seltype == "Continuous")
         seltype.ind <- TRUE
   }

# AS nesessary, adjust the indicator variable for presence of multidensity
# categories to account for any strata with "Continuous" type of random
# selection

   mdcaty.ind <- mdcaty.ind && !seltype.ind

# Produce tables for the design variables

   if(mdcaty.ind) {
      if(panel.ind) {
         if(stratum.ind) {
            comb1 <- addmargins(table(sites$mdcaty, sites$stratum,
               dnn=c("mdcaty", "stratum")))
            cat("Design Summary: Number of Sites Classified by mdcaty (Multidensity Category) \nand stratum\n\n")
            print(comb1)
            comb2 <- addmargins(table(sites$panel, sites$stratum, dnn=c("panel",
               "stratum")))
            cat("\n\nDesign Summary: Number of Sites Classified by panel and stratum\n\n")
            print(comb2)
            comb3 <- addmargins(table(sites$mdcaty, sites$panel, sites$stratum,
               dnn=c("mdcaty", "panel", "stratum")))
            cat("\n\nDesign Summary: Number of Sites Classified by mdcaty (Multidensity Category), \npanel, and stratum\n\n")
            print(comb3)
         } else {
            comb3 <- addmargins(table(sites$mdcaty, sites$panel,
               dnn=c("mdcaty", "panel")))
            cat("\n\nDesign Summary: Number of Sites Classified by mdcaty (Multidensity Category) \n and panel\n\n")
            print(comb3)
         }
      } else {
         if(stratum.ind) {
            comb1 <- addmargins(table(sites$mdcaty, sites$stratum,
               dnn=c("mdcaty", "stratum")))
            cat("Design Summary: Number of Sites Classified by mdcaty (Multidensity Category) \nand stratum\n\n")
            print(comb1)
         } else {
            comb1 <- addmargins(table(sites$mdcaty, dnn=c("mdcaty")))
            cat("Design Summary: Number of Sites Classified by mdcaty (Multidensity Category)\n\n")
            print(comb1)
         }
      }
   } else {
      if(panel.ind) {
         if(stratum.ind) {
            comb2 <- addmargins(table(sites$panel, sites$stratum, dnn=c("panel",
               "stratum")))
            cat("\n\nDesign Summary: Number of Sites Classified by panel and stratum\n\n")
            print(comb2)
         } else {
            comb2 <- addmargins(table(sites$panel, dnn=c("panel")))
            cat("\n\nDesign Summary: Number of Sites Classified by panel\n\n")
            print(comb2)
         }
      } else {
         comb3 <- addmargins(table(sites$stratum, dnn=c("stratum")))
         cat("\n\nDesign Summary: Number of Sites\n\n")
         print(comb3)
      }
   }

# Produce tables for the auxiliary variables

   if(is.null(auxvar)) {
      AuxVarSum <- NULL
   } else {
      temp <- match(auxvar, names(sites), nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(auxvar[temp == 0])
         stop(paste("\nThe following values in the vector of auxiliary variable names do not occur \namong the columns in the survey design data frame:\n", temp.str, sep=""))
      }

      AuxVarSum <- list()
      for(i in auxvar) {
         if(!is.factor(sites[,i]))
            sites[,i] <- as.factor(sites[,i])
         if(!seltype.ind) {
            AuxVarSum[[i]] <- addmargins(table(sites$mdcaty, sites$panel,
               sites$stratum, sites[,i], dnn=c("mdcaty", "panel",
               "stratum", i)))
            cat(paste("\n\nDesign Summary: Number of Sites Classified by mdcaty, (Multidensity Category), \nstratum, panel, and ", i, " (Auxiliary Variable)\n\n", sep=""))
            print(AuxVarSum[[i]])
         } else {
            AuxVarSum[[i]] <- addmargins(table(sites$panel, sites$stratum,
               sites[,i], dnn=c("panel", "stratum", i)))
            cat(paste("\n\nDesign Summary: Number of Sites Classified by stratum, panel, and \n", i, " (Auxiliary Variable)\n\n", sep=""))
            print(AuxVarSum[[i]])
         }
      }
   }

# Create the output list

   if(mdcaty.ind) { 
      if(panel.ind) {
         if(stratum.ind) {
            rslt <- list(DesignSum=list("mdcaty by stratum"=comb1,
               "panel by stratum"=comb2, "mdcaty by panel by stratum"=comb3),
                AuxVarSum=AuxVarSum)
         } else {
            rslt <- list(DesignSum=list("mdcaty by panel"=comb3),
               AuxVarSum=AuxVarSum)
         }
      } else {
         if(stratum.ind) {
            rslt <- list(DesignSum=list("mdcaty by stratum"=comb1),
               AuxVarSum=AuxVarSum)
         } else {
            rslt <- list(DesignSum=list("mdcaty"=comb1),
               AuxVarSum=AuxVarSum)
         }
      }
   } else {
      if(panel.ind) {
         if(stratum.ind) {
            rslt <- list(DesignSum=list("panel by stratum"=comb2),
               AuxVarSum=AuxVarSum)
         } else {
            rslt <- list(DesignSum=list("panel"=comb2),
               AuxVarSum=AuxVarSum)
         }
      } else {
         rslt <- list(DesignSum=list("stratum"=comb3),
            AuxVarSum=AuxVarSum)
      }
   }

# Return the list

   invisible(rslt)
}
