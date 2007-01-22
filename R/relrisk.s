relrisk <- function(dframe, response="response", stressor="stressor",
   response.levels=c("Poor", "Good"), stressor.levels=c("Poor", "Good"),
   wgt="wgt", xcoord="xcoord", ycoord="ycoord", stratum=NULL, cluster=NULL,
   N.cluster=NULL, wgt1=NULL, xcoord1=NULL, ycoord1=NULL, popsize=NULL,
   stage1size=NULL, support=NULL, swgt=NULL, swgt1=NULL, unitsize=NULL,
   vartype="Local", conf=95, check.ind=TRUE) {

################################################################################
# Function: relrisk
# Purpose: Compute the relative risk estimate
# Programmers: Tom Kincaid, Tony Olsen, John Vansickle
# Date: May 4, 2004
# Last Revised: June 29, 2006
# Description:
#   This function calculates the relative risk estimate for a 2x2 table of cell
#   counts defined by a categorical response variable and a categorical
#   explanatory (stressor) variable for an unequal probability design.  Relative
#   risk is the ratio of two probabilities: the numerator is the probability
#   that the first level of the response variable is observed given occurrence
#   of the first level of the stressor variable, and the denominator is the
#   probability that the first level of the response variable is observed given
#   occurrence of the second level of the stressor variable.  The numerator
#   probability and denominator probability are estimated using cell and
#   marginal totals from a 2x2 table of cell counts defined by a categorical
#   response variable and a categorical stressor variable. An estimate of the
#   numerator probability is provided by the ratio of the cell total defined by
#   the first level of response variable and the first level of the stressor
#   variable to the marginal total for the first level of the stressor variable.
#   An estimate of the denominator probability is provided by the ratio of the
#   cell total defined by the first level of response variable and the second
#   level of the stressor variable to the marginal total for the second level of
#   the stressor variable.  Cell and marginal totals are estimated using the
#   Horvitz-Thompson estimator.  The standard error of the log of the relative
#   risk estimate and confidence limits for the estimate also are calculated.
#   The standard error is calculated using a first-order Taylor series
#   linearization (Sarndal et al., 1992).
# Arguments:
#   dframe = a data frame containing the variables required for the analysis.
#     If variable names are not provided in the corresponding arguments, then
#     variables should be named as follows: 
#       response = the categorical response variable values
#       stressor = the categorical explanatory (stressor) variable values
#       wgt = the final adjusted weights
#       xcoord = the x-coordinates for location
#       ycoord = the y-coordinates for location
#       stratum = the stratum codes
#   response = name of the column in dframe containing the categorical response 
#     variable.  The default is "response".
#   stressor = name of the column in dframe containing the categorical stressor 
#     variable.  The default is "stressor".
#   response.levels = category values (levels) for the categorical response 
#     variable, where the first level is used for calculating the relative risk 
#     estimate.  If response.levels is not supplied, then values "Poor" and
#     "Good" are used for the first level and second level of the response
#     variable, respectively.  The default is c("Poor", "Good").
#   stressor.levels = category values (levels) for the categorical stressor 
#     variable, where the first level is used for calculating the numerator of 
#     the relative risk estimate and the second level is used for calculating 
#     the denominator of the estimate.  If stressor.levels is not supplied, then 
#     values "Poor" and "Good" are used for the first level and second level of 
#     the stressor variable, respectively.  The default is c("Poor", "Good").
#   wgt = name of the column in dframe containing the final adjusted weights. 
#      The default is "wgt".
#   xcoord = name of the column in dframe containing the x-coordinates for 
#     location.  The default is "xcoord".
#   ycoord = name of the column in dframe containing the y-coordinates for 
#     location.  The default is "ycoord".
#   stratum = name of the column in dframe containing the stratum codes.  The 
#     default is NULL.
#   cluster = the stage one sampling unit (primary sampling unit or cluster) 
#     code for each site.  The default is NULL.
#   N.cluster = the number of stage one sampling units in the resource, which is 
#     required for calculation of finite and continuous population correction 
#     factors for a two-stage sample.  For a stratified sample this variable 
#     must be a vector containing a value for each stratum and must have the 
#     names attribute set to identify the stratum codes.  The default is NULL.
#   wgt1 = the final adjusted stage one weight for each site.  The default is 
#     NULL.
#   xcoord1 = the stage one x-coordinate for location for each site.  The 
#     default is NULL.
#   ycoord1 = the stage one y-coordinate for location for each site.  The 
#     default is NULL.
#   popsize = the known size of the resource - the total number of sampling 
#     units of a finite resource or the measure of an extensive resource, which 
#     is required for calculation of finite and continuous population correction 
#     factors for a single-stage sample.  This variable is also used to adjust 
#     estimators for the known size of a resource.  For a stratified sample this 
#     variable must be a vector containing a value for each stratum and must 
#     have the names attribute set to identify the stratum codes.  The default 
#     is NULL.
#   stage1size = the known size of the stage one sampling units of a two-stage 
#     sample, which is required for calculation of finite and continuous 
#     population correction factors for a two-stage sample and must have the 
#     names attribute set to identify the stage one sampling unit codes.  For a 
#     stratified sample, the names attribute must be set to identify both 
#     stratum codes and stage one sampling unit codes using a convention where 
#     the two codes are separated by the # symbol, e.g., "Stratum 1#Cluster 1". 
#     The default is NULL.
#   support = the support value for each site - the value one (1) for a site 
#     from a finite resource or the measure of the sampling unit associated with 
#     a site from an extensive resource, which is required for calculation of 
#     finite and continuous population correction factors.  The default is NULL.
#   swgt = the size-weight for each site, which is the stage two size-weight for 
#     two-stage sample.  The default is NULL.
#   swgt1 = the stage one size-weight for each site.  The default is NULL.
#   unitsize = the known sum of the size-weights of the resource, which for a 
#     stratified sample must be a vector containing a value for each stratum and 
#     must have the names attribute set to identify the stratum codes.  The 
#     default is NULL.
#   vartype = the choice of variance estimator, where "Local" = local mean 
#     estimator and "SRS" = SRS estimator.  The default is "Local".
#   conf = the confidence level.  The default is 95%.
#   check.ind = a logical value that indicates whether compatability checking of 
#     the input values is conducted, where TRUE = conduct compatibility checking 
#     and FALSE = do not conduct compatibility checking.  The default is TRUE.
# Results:
#   An object in list format containing the following components:
#     RelRisk - the relative risk estimate
#     RRnum - numerator ("elevated" risk) of the relative risk estimate
#     RRdenom - denominator ("baseline" risk) of the relative risk estimate
#     RRlog.se - standard error for the log of the relative risk estimate
#     ConfLimits - confidence limits for the relative risk estimate
#     WeightTotal - sum of the final adjusted weights
#     CellCounts - cell and margin counts for the 2x2 table
#     CellProportions - estimated cell proportions for the 2x2 table
# Other Functions Required:
#   vecprint - takes an input vector and outputs a character string with line 
#     breaks inserted
#   input.check - check input values for errors, consistency, and compatibility 
#     with psurvey.analysis analytical functions
#   wnas - remove missing values
#   relrisk.var - calculate values required for estimating variance of the
#     relative risk estimate
# Examples:
#   dframe <- data.frame(response=sample(c("Poor", "Good"), 100, replace=TRUE),
#      stressor=sample(c("Poor", "Good"), 100, replace=TRUE),
#   wgt=runif(100, 10, 100))
#   relrisk(dframe, vartype="SRS")
#
#   dframe$xcoord <- runif(100)
#   dframe$ycoord <- runif(100)
#   relrisk(dframe)
################################################################################

# Create a data frame for warning messages

   warn.ind <- FALSE
   warn.df <- NULL
   warn.vec <- rep(NA, 3)
   fname <- "relrisk"

# Assign variables from the input data frame

   if(vartype == "Local") {
      temp <- match(c(response, stressor, wgt, xcoord, ycoord),
         names(dframe))
      if(any(is.na(temp))) {
         temp.str <- vecprint(c(response, stressor, wgt, xcoord,
            ycoord)[is.na(temp)])
         stop(paste("\nThe following names were not found among the variables in the input data frame:\n", temp.str, sep=""))
      }
      response <- dframe[,response]
      stressor <- dframe[,stressor]
      wgt <- dframe[,wgt]
      xcoord <- dframe[,xcoord]
      ycoord <- dframe[,ycoord]
   } else {
      temp <- match(c(response, stressor, wgt), names(dframe))
      if(any(is.na(temp))) {
         temp.str <- vecprint(c(response, stressor, wgt)[is.na(temp)])
         stop(paste("\nThe following names were not found among the variables in the input data frame:\n", temp.str, sep=""))
      }
      response <- dframe[,response]
      stressor <- dframe[,stressor]
      wgt <- dframe[,wgt]
   }
   nresp <- length(response)

# Determine whether the sample is stratified

   stratum.ind <- length(unique(stratum)) > 1

# If the sample is stratified, convert stratum to a factor, determine stratum 
# levels, and calculate number of strata,

   if(stratum.ind) {
      if(is.null(dframe[,stratum]))
         stop(paste("\nThe name provided for the column in the input data frame containing the stratum \ncodes, ", stratum, ", does not exist among the variables in the data frame.", sep=""))
      stratum <- factor(dframe[,stratum])
      stratum.levels <- levels(stratum)
      nstrata <- length(stratum.levels)
   } else {
      stratum.levels <- NULL
      nstrata <- NULL
   }

# Determine whether the sample has two stages

   cluster.ind <- length(unique(cluster)) > 1

# Determine whether the population correction factor is to be used

   pcfactor.ind <- !is.null(support)

# Determine whether the sample uses size-wgts

   swgt.ind <- length(unique(swgt)) > 1

# Begin the section that checks for compatibility of input values

   if(check.ind) {

# If the sample has two stages, convert cluster to a factor, determine cluster 
# levels, and calculate number of clusters

   if(cluster.ind) {
      if(stratum.ind) {
         cluster.in <- cluster
         cluster <- tapply(cluster, stratum, factor)
         cluster.levels <- sapply(cluster, levels, simplify=FALSE)
         ncluster <- sapply(cluster.levels, length)
      } else {
         cluster <- factor(cluster)
         cluster.levels <- levels(cluster)
         ncluster <- length(cluster.levels)
      }
   }

# Check for compatibility of input values

   temp <- input.check(nresp, wgt, NULL, NULL, xcoord, ycoord, stratum.ind,
      stratum, stratum.levels, nstrata, cluster.ind, cluster, cluster.levels,
      ncluster, N.cluster, wgt1, xcoord1, ycoord1, popsize, stage1size,
      pcfactor.ind, support, swgt.ind, swgt, swgt1, unitsize, vartype, conf)
   N.cluster <- temp$N.cluster
   popsize <- temp$popsize
   stage1size <- temp$stage1size
   unitsize <- temp$unitsize

# If the sample was stratified and had two stages, then reset cluster to its 
# input value

   if(stratum.ind && cluster.ind)
      cluster <- cluster.in

# End the section that checks for compatibility of input values

   }

# Remove missing values

   if(vartype == "Local") {
      if(swgt.ind) {
         if(stratum.ind) {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, stratum=stratum,
                  cluster=cluster, wgt1=wgt1, xcoord1=xcoord1, ycoord1=ycoord1,
                  swgt=swgt, swgt1=swgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, stratum=stratum,
                  swgt=swgt))
         } else {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, cluster=cluster,
                  wgt1=wgt1, xcoord1=xcoord1, ycoord1=ycoord1, swgt=swgt,
                  swgt1=swgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, swgt=swgt))
         }
         response <- temp$response
         stressor <- temp$stressor
         wgt <- temp$wgt
         xcoord <- temp$xcoord
         ycoord <- temp$ycoord
         if(stratum.ind)
            stratum <- temp$stratum
         if(cluster.ind) {
            cluster <- temp$cluster
            wgt1 <- temp$wgt1
            xcoord1 <- temp$xcoord1
            ycoord1 <- temp$ycoord1
            swgt1 <- temp$swgt1
         }
         swgt <- temp$swgt
      } else {
         if(stratum.ind) {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, stratum=stratum,
                  cluster=cluster, wgt1=wgt1, xcoord1=xcoord1, ycoord1=ycoord1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, stratum=stratum))
         } else {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord, cluster=cluster,
                  wgt1=wgt1, xcoord1=xcoord1, ycoord1=ycoord1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, xcoord=xcoord, ycoord=ycoord))
         }
         response <- temp$response
         stressor <- temp$stressor
         wgt <- temp$wgt
         xcoord <- temp$xcoord
         ycoord <- temp$ycoord
         if(stratum.ind)
            stratum <- temp$stratum
         if(cluster.ind) {
            cluster <- temp$cluster
            wgt1 <- temp$wgt1
            xcoord1 <- temp$xcoord1
            ycoord1 <- temp$ycoord1
         }
      }
   } else {
      if(swgt.ind) {
         if(stratum.ind) {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, stratum=stratum, cluster=cluster, wgt1=wgt1,
                  swgt=swgt, swgt1=swgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, stratum=stratum, swgt=swgt))
         } else {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, cluster=cluster, wgt1=wgt1, swgt=swgt,
                  swgt1=swgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, swgt=swgt))
         }
         response <- temp$response
         stressor <- temp$stressor
         wgt <- temp$wgt
         if(stratum.ind)
            stratum <- temp$stratum
         if(cluster.ind) {
            cluster <- temp$cluster
            wgt1 <- temp$wgt1
            swgt1 <- temp$swgt1
         }
         swgt <- temp$swgt
      } else {
         if(stratum.ind) {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, stratum=stratum, cluster=cluster, wgt1=wgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, stratum=stratum))
         } else {
            if(cluster.ind)
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt, cluster=cluster, wgt1=wgt1))
            else
               temp <- wnas(list(response=response, stressor=stressor,
                  wgt=wgt))
         }
         response <- temp$response
         stressor <- temp$stressor
         wgt <- temp$wgt
         if(stratum.ind)
            stratum <- temp$stratum
         if(cluster.ind) {
            cluster <- temp$cluster
            wgt1 <- temp$wgt1
         }
      }
   }

# Determine levels of the response and stressor variables

   if(!is.factor(response))
      response <- as.factor(response)
   temp <- match(levels(response), response.levels)
      if(any(is.na(temp)))
         stop("\nThe category values in response.levels must match the categorical response variable \ncodes.")
   response <- factor(as.vector(response), levels=response.levels)

   if(!is.factor(stressor))
      stressor <- as.factor(stressor)
   temp <- match(levels(stressor), stressor.levels)
      if(any(is.na(temp)))
         stop("\nThe category values in stressor.levels must match the categorical stressor variable \ncodes.")
   stressor <- factor(as.vector(stressor), levels=stressor.levels)

# For a stratified sample, check for strata that no longer contain any values,
# as necesssary adjust popsize and unitsize, remove strata that contain a single
# value, and output a warning message

   if(stratum.ind) {
      stratum <- factor(stratum)
      stratum.levels.old <- stratum.levels
      stratum.levels <- levels(stratum)
      nstrata.old <- nstrata
      nstrata <- length(stratum.levels)
      if(nstrata < nstrata.old) {
         warn.ind <- TRUE
         temp <- match(stratum.levels, stratum.levels.old)
         temp.str <- vecprint(stratum.levels.old[-temp])
         warn <- paste("The following strata no longer contain any values and were removed from the \nanalysis:\n", temp.str, sep="")
         act <- "Strata were removed from the analysis.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
         if(!is.null(popsize))
            popsize <- popsize[temp]
         if(!is.null(unitsize))
            unitsize <- unitsize[temp]
      }

      ind <- FALSE
      for(i in 1:nstrata) {
         stratum.i <- stratum == stratum.levels[i]
         if(sum(stratum.i) == 1) {
            warn.ind <- TRUE
            warn <- paste("The stratum named", stratum.levels[i], "contains a single value and was removed from the analysis.\n")
            act <- "Stratum was removed from the analysis.\n"
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
            response <- response[!stratum.i]
            stressor <- stressor[!stratum.i]
            wgt <- wgt[!stratum.i]
            if(vartype == "Local") {
               xcoord <- xcoord[!stratum.i]
               ycoord <- ycoord[!stratum.i]
            }
            stratum <- stratum[!stratum.i]
            if(cluster.ind) {
               cluster <- cluster[!stratum.i]
               wgt1 <- wgt1[!stratum.i]
               if(vartype == "Local") {
                  xcoord1 <- xcoord1[!stratum.i]
                  ycoord1 <- ycoord1[!stratum.i]
               }
            }
            if(swgt.ind) {
               swgt <- swgt[!stratum.i]
               if(cluster.ind)
                  swgt1 <- swgt1[!stratum.i]
            }
            if(!is.null(popsize))
               popsize <- popsize[names(popsize) != stratum.levels[i]]
            if(!is.null(unitsize))
               unitsize <- unitsize[names(unitsize) != stratum.levels[i]]
            ind <- TRUE
         }
      }
      if(ind) {
         stratum <- factor(stratum)
         stratum.levels <- levels(stratum)
         nstrata <- length(stratum.levels)
      }

# Check whether the number of strata is one

      if(nstrata == 1) {
         warn.ind <- TRUE
         warn <- "Only a single stratum was available for the analysis.\n"
         act <- "An unstratified data analysis was used.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
         stratum.ind <- FALSE
      }
   }

# Check whether the vector of response values is empty

   nresp <- length(response)
   if(nresp == 0)
      stop("\nEstimates cannot be calculated since the vector of response values is empty.")

# If the sample has two stages, determine whether there are a sufficient number
# of sites in each stage one sampling unit to allow variance calculation

   if(cluster.ind) {
      temp <- sapply(split(cluster, cluster), length) == 1
      if(any(temp)) {
         temp.str <- vecprint(names(temp)[temp])
         stop(paste("\nA variance estimate cannot be calculated since the following stage one sampling \nunit(s) contain a single site:\n", temp.str, sep=""))
      }
   }

# Calculate the confidence bound multiplier

   mult <- qnorm(0.5 + (conf/100)/2)

# Calculate additional required values

   if(swgt.ind) {
      if(!is.null(unitsize)) {
         sum.unitsize <- sum(unitsize)
      } else {
         if(stratum.ind) {
            if(cluster.ind) {
               unitsize.hat <- tapply(wgt*swgt*wgt1*swgt1, stratum, sum)
               sum.unitsize.hat <- sum(wgt*swgt*wgt1*swgt1)
            } else {
               unitsize.hat <- tapply(wgt*swgt, stratum, sum)
               sum.unitsize.hat <- sum(wgt*swgt)
            }
         } else {
            if(cluster.ind)
               unitsize.hat <- sum(wgt*swgt*wgt1*swgt1)
            else
               unitsize.hat <- sum(wgt*swgt)
         }
      }
   } else {
      if(!is.null(popsize)) {
         sum.popsize <- sum(popsize)
      } else {
         if(stratum.ind) {
            if(cluster.ind) {
               popsize.hat <- tapply(wgt*wgt1, stratum, sum)
               sum.popsize.hat <- sum(wgt*wgt1)
            } else {
               popsize.hat <- tapply(wgt, stratum, sum)
               sum.popsize.hat <- sum(wgt)
            }
         } else {
            if(cluster.ind)
               popsize.hat <- sum(wgt*wgt1)
            else
               popsize.hat <- sum(wgt)
         }
      }
   }

# Branch to handle stratified and unstratified data

   if(stratum.ind) {

# Begin the section for stratified data

# Initialize variables for all strata combined

      wgt.total <- 0
      rr <- 0
      rr.num <- 0
      rr.denom <- 0
      rrlog.var <- 0

# Begin the subsection for individual strata

      for(i in 1:nstrata) {

# Calculate required values

         stratum.i <- stratum == stratum.levels[i]
         response.st <- response[stratum.i]
         stressor.st <- stressor[stratum.i]
         if(swgt.ind) {
            if(cluster.ind) {
               wgt.st <- wgt[stratum.i]*swgt[stratum.i]
               wgt1.st <- wgt1[stratum.i]*swgt1[stratum.i]
            } else {
               wgt.st <- wgt[stratum.i]*swgt[stratum.i]
            }
         } else {
            if(cluster.ind) {
               wgt.st <- wgt[stratum.i]
               wgt1.st <- wgt1[stratum.i]
            } else {
               wgt.st <- wgt[stratum.i]
            }
         }

# Compute the 2x2 table of weight totals

         if(cluster.ind) {
            wgt.total.st <- tapply(wgt*wgt1, list(response=response.st,
               stressor=stressor.st), sum)
         } else {
            wgt.total.st <- tapply(wgt, list(response=response.st,
               stressor=stressor.st), sum)
         }
         wgt.total.st[is.na(wgt.total.st)] <- 0

# Calculate required cell and marginal weight totals
   
         total1 <- wgt.total.st[response.levels[1], stressor.levels[1]]
         total2 <- sum(wgt.total.st[,stressor.levels[1]])
         total3 <- wgt.total.st[response.levels[1], stressor.levels[2]]
         total4 <- sum(wgt.total.st[,stressor.levels[2]])
   
# Calculate the estimate of relative risk
   
         if(total2 == 0 || total4 == 0) {
            rr.st <- NA
            rr.num.st <- NA
            rr.denom.st <- NA
            warn.ind <- TRUE
            temp <- ifelse(total2 == 0, stressor.levels[1], stressor.levels[2])
            warn <- paste("Since there are no observations for level \"", temp, "\" of the stressor \nvariable, the relative risk estimate and its standard error cannot be \ncalculated for stratum \"", stratum.levels[i], "\".  Also, the stratum \nwas removed from the analysis.\n", sep="")
            act <- paste("The relative risk estimate and its standard error were not calculated for \nstratum \"", stratum.levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         } else if(total1 == 0 && total3 != 0) {
            rr.st <- 0
            rr.num.st <- 0
            rr.denom.st <- total3/total4
            warn.ind <- TRUE
            warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[1], "\" of the stressor \nvariable, the relative risk estimate is zero and standard error of the relative \nrisk estimate cannot be calculated for stratum \"", stratum.levels[i], "\".  \nAlso, the stratum was removed from the analysis.\n", sep="")
            act <- paste("Standard error of the relative risk estimate was not calculated for stratum \n\"", stratum.levels[i], "\".  Also, the stratum was removed from the \nanalysis.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         } else if(total1 == 0 && total3 == 0) {
            rr.st <- NA
            rr.num.st <- total1/total2
            rr.denom.st <- total3/total4
            warn.ind <- TRUE
            warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[1], "\" of the stressor \nvariable and for the cell defined by level \"", response.levels[1], "\" of the \nresponse variable and level \"", stressor.levels[2], "\" of the stressor variable, \nthe relative risk estimate and its standard error cannot be calculated for \nstratum \"", stratum.levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep="")
            act <- paste("The relative risk estimate and its standard error were not calculated for \nstratum \"", stratum.levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         } else if(total3 == 0) {
            rr.st <- NA
            rr.num.st <- total1/total2
            rr.denom.st <- total3/total4
            warn.ind <- TRUE
            warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[2], "\" of the stressor \nvariable, the relative risk estimate and its standard error cannot be \ncalculated for stratum \"", stratum.levels[i], "\".  Also, the stratum \nwas removed from the analysis.\n", sep="")
            act <- paste("The relative risk estimate and its standard error were not calculated for \nstratum \"", stratum.levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         } else {
            rr.st <- (total1*total4) / (total2*total3)
            rr.num.st <- total1/total2
            rr.denom.st <- total3/total4
         }

# Determine whether the stratum is being used in the analysis

         if(all(c(total1, total2, total3, total4) != 0)) {

# Calculate the variance-covariance estimate for the cell and marginal totals

            if(cluster.ind) {
               temp <- relrisk.var(response.st, stressor.st, response.levels,
                  stressor.levels, wgt.st, xcoord[stratum.i], ycoord[stratum.i],
                  stratum.ind, stratum.levels[i], cluster.ind,
                  cluster[stratum.i], N.cluster[i], wgt1.st, xcoord1[stratum.i],
                  ycoord1[stratum.i], popsize[i], pcfactor.ind, stage1size[[i]],
                  support[stratum.i], vartype, warn.ind, warn.df, warn.vec)
            } else {
               temp <- relrisk.var(response.st, stressor.st, response.levels,
                  stressor.levels, wgt.st, xcoord[stratum.i], ycoord[stratum.i],
                  stratum.ind, stratum.levels[i], cluster.ind,
                  popsize=popsize[i], pcfactor.ind=pcfactor.ind,
                  support=support[stratum.i], vartype=vartype,
                  warn.ind=warn.ind, warn.df=warn.df, warn.vec=warn.vec)
            }
            varest.st <- temp$varest
            warn.ind <- temp$warn.ind
            warn.df <- temp$warn.df

# Calculate the variance estimate of the log of relative risk
   
            temp <- (1/c(total1, -total2, -total3, total4)) %o% (1/c(total1,
               -total2, -total3, total4))
            rrlog.var.st <- sum(temp*varest.st)

# Add estimates to the variables for all strata combined

            if(swgt.ind) {
               if(!is.null(unitsize)) {
                  wgt.total <- wgt.total + (unitsize[i]/sum.unitsize) *
                     wgt.total.st
                  rr <- rr + (unitsize[i]/sum.unitsize)*rr.st
                  rr.num <- rr.num + (unitsize[i]/sum.unitsize)*rr.num.st
                  rr.denom <- rr.denom + (unitsize[i]/sum.unitsize)*rr.denom.st
                  rrlog.var <- rrlog.var + ((unitsize[i]/sum.unitsize)^2)*
                     rrlog.var.st
               } else {
                  wgt.total <- wgt.total + (unitsize.hat[i]/sum.unitsize.hat)*
                     wgt.total.st
                  rr <- rr + (unitsize.hat[i]/sum.unitsize.hat)*rr.st
                  rr.num <- rr.num + (unitsize.hat[i]/sum.unitsize.hat) *
                     rr.num.st
                  rr.denom <- rr.denom + (unitsize.hat[i]/sum.unitsize.hat) *
                     rr.denom.st
                  rrlog.var <- rrlog.var + ((unitsize.hat[i]/sum.unitsize)^2)*
                     rrlog.var.st
               }
            } else {
               if(!is.null(popsize)) {
                  wgt.total <- wgt.total + (popsize[i]/sum.popsize)*wgt.total.st
                  rr <- rr + (popsize[i]/sum.popsize)*rr.st
                  rr.num <- rr.num + (popsize[i]/sum.popsize)*rr.num.st
                  rr.denom <- rr.denom + (popsize[i]/sum.popsize)*rr.denom.st
                  rrlog.var <- rrlog.var + ((popsize[i]/sum.popsize)^2)*
                     rrlog.var.st
               } else {
                  wgt.total <- wgt.total + (popsize.hat[i]/sum.popsize.hat)*
                     wgt.total.st
                  rr <- rr + (popsize.hat[i]/sum.popsize.hat)*rr.st
                  rr.num <- rr.num + (popsize.hat[i]/sum.popsize.hat)*rr.num.st
                  rr.denom <- rr.denom + (popsize.hat[i]/sum.popsize.hat) *
                     rr.denom.st
                  rrlog.var <- rrlog.var + ((popsize.hat[i]/sum.popsize)^2)*
                     rrlog.var.st
               }
            }
         }

# End the subsection for individual strata

      }

# Calculate the standard error estimate of the log of relative risk for all
# strata combined

      if(rrlog.var == 0) {
         rrlog.se <- NA
      } else {
         rrlog.se <- sqrt(rrlog.var)
      }

# End the section for stratified data

   } else {

# Begin the section for unstratified data

# Check whether the vector of response values contains a single element

      if(nresp == 1)
         stop("\nEstimates cannot be calculated since the vector of response values contains a \nsingle element.")

# If the sample is size-weighted, calculate combined weights

   if(swgt.ind) {
      if(cluster.ind) {
         wgt <- wgt*swgt
         wgt1 <- wgt1*swgt1
      } else {
         wgt <- wgt*swgt
      }
   }

# Compute the 2x2 table of weight totals

      if(cluster.ind) {
         wgt.total <- tapply(wgt*wgt1, list(response=response,
            stressor=stressor), sum)
      } else {
         wgt.total <- tapply(wgt, list(response=response, stressor=stressor),
            sum)
      }
      wgt.total[is.na(wgt.total)] <- 0

# Calculate required cell and marginal weight totals
   
      total1 <- wgt.total[response.levels[1], stressor.levels[1]]
      total2 <- sum(wgt.total[,stressor.levels[1]])
      total3 <- wgt.total[response.levels[1], stressor.levels[2]]
      total4 <- sum(wgt.total[,stressor.levels[2]])
   
# Calculate the estimate of relative risk
   
      if(total2 == 0 || total4 == 0) {
         rr <- NA
         rr.num <- NA
         rr.denom <- NA
         warn.ind <- TRUE
         temp <- ifelse(total2 == 0, stressor.levels[1], stressor.levels[2])
         warn <- paste("Since there are no observations for level \"", temp, "\" of the stressor \nvariable, the relative risk estimate and its standard error cannot be \ncalculated.\n", sep="")
         act <- "The relative risk estimate and its standard error were not calculated.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
      } else if(total1 == 0 && total3 != 0) {
         rr <- 0
         rr.num <- 0
         rr.denom <- total3/total4
         warn.ind <- TRUE
         warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[1], "\" of the stressor variable, \nthe relative risk estimate is zero and standard error of the relative risk \nestimate cannot be calculated.\n", sep="")
         act <- "Standard error of the relative risk estimate was not calculated.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
      } else if(total1 == 0 && total3 == 0) {
         rr <- NA
         rr.num <- total1/total2
         rr.denom <- total3/total4
         warn.ind <- TRUE
         warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[1], "\" of the stressor \nvariable and for the cell defined by level \"", response.levels[1], "\" of the \nresponse variable and level \"", stressor.levels[2], "\" of the stressor variable, \nthe relative risk estimate and its standard error cannot be calculated.\n", sep="")
         act <- "The relative risk estimate and its standard error were not calculated.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
      } else if(total3 == 0) {
         rr <- NA
         rr.num <- total1/total2
         rr.denom <- total3/total4
         warn.ind <- TRUE
         warn <- paste("Since there are no observations for the cell defined by level \"", response.levels[1], "\" \nof the response variable and level \"", stressor.levels[2], "\" of the stressor \nvariable, the relative risk estimate and its standard error cannot be \ncalculated.\n", sep="")
         act <- "The relative risk estimate and its standard error were not calculated.\n"
         warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
      } else {
         rr <- (total1*total4) / (total2*total3)
         rr.num <- total1/total2
         rr.denom <- total3/total4
      }

# Determine whether the standard error can be calculated

      if(any(c(total1, total2, total3, total4) == 0)) {
         rrlog.se <- NA
      } else {

# Calculate the variance-covariance estimate for the cell and marginal totals

         if(cluster.ind) {
            temp <- relrisk.var(response, stressor, response.levels,
               stressor.levels, wgt, xcoord, ycoord, stratum.ind, NULL,
               cluster.ind, cluster, N.cluster, wgt1, xcoord1, ycoord1, popsize,
               pcfactor.ind, stage1size, support, vartype, warn.ind, warn.df,
               warn.vec)
         } else {
            temp <- relrisk.var(response, stressor, response.levels,
               stressor.levels, wgt, xcoord, ycoord, stratum.ind, NULL,
               cluster.ind, popsize=popsize, pcfactor.ind=pcfactor.ind,
               support=support, vartype=vartype, warn.ind=warn.ind,
               warn.df=warn.df, warn.vec=warn.vec)
         }
         varest <- temp$varest
         warn.ind <- temp$warn.ind
         warn.df <- temp$warn.df

# Calculate the standard error estimate of the log of relative risk
   
         temp <- (1/c(total1, -total2, -total3, total4)) %o% (1/c(total1,
            -total2, -total3, total4))
         rrlog.se <- sqrt(sum(temp*varest))
      }

# End section for unstratified data

   }

# Calculate confidence limits for the estimate of relative risk

   if(is.na(rrlog.se)) {
      cl <- NA
   } else {
      cl <- c(exp(log(rr) - rrlog.se * mult), exp(log(rr) + rrlog.se * mult) )
   }

# Calculate the table of cell and margin counts
   
   cc <- ftable(addmargins(table(list(response=response,
      stressor=stressor))))
   
# Calculate the table of cell and margin proportion estimates
   
   cp <- ftable(addmargins(wgt.total/sum(wgt.total)))
   
# Create the output list
   
   relrisk <- list(RelRisk=rr, RRnum=rr.num, RRdenom=rr.denom,
      RRlog.se=rrlog.se, ConfLimits=cl, WeightTotal=sum(wgt.total),
      CellCounts=cc, CellProportions=cp )
   
# As necessary, output a message indicating that warning messages were generated
# during execution of the program

   if(warn.ind) {
      warn.df <<- warn.df
      if(nrow(warn.df) == 1)
         cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn.df'.  Enter the following command \nto view the warning message: warnprnt()\n")
      else
         cat(paste("During execution of the program,", nrow(warn.df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn.df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
   }

# Return the estimates
   
   relrisk
}
