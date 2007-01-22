input.check <- function(nresp, wgt, sigma, var.sigma, xcoord, ycoord,
   stratum.ind, stratum, stratum.levels, nstrata, cluster.ind, cluster,
   cluster.levels, ncluster, N.cluster, wgt1, xcoord1, ycoord1, popsize,
   stage1size, pcfactor.ind, support, swgt.ind, swgt, swgt1, unitsize, vartype,
   conf, cdfval=NULL, pctval=NULL, subpop=NULL) {

################################################################################
# Function: input.check
# Programmer: Tom Kincaid
# Date: September 25, 2003
# Last Revised: October 31, 2006
# Description:
#   This function checks input values for errors, consistency, and compatibility
#   with psurvey.analysis analytical functions.
#   Input:
#      nresp = the number of response values.
#      wgt = the final adjusted weights.
#      sigma = measurement error variance.
#      var.sigma = variance of the measurement error variance.
#      xcoord = the x-coordinates for location.
#      ycoord = the y-coordinates for location.
#      stratum.ind = a logical value that indicates whether the sample is
#         stratified, where TRUE = a stratified sample and FALSE = not a
#         stratified sample.
#      stratum = the stratum codes.
#      stratum.levels = levels of the stratum variable.
#      nstrata = the number of strata.
#      cluster.ind = a logical value that indicates whether the sample is a two-
#         stage sample, where TRUE = a two-stage sample and FALSE = not a two-
#         stage sample.
#      cluster = the stage one sampling unit codes.
#      N.cluster = the number of stage one sampling units in the resource.
#      ncluster = the number of stage one sampling units in the sample.
#      wgt1 = the final adjusted stage one weights.
#      xcoord1 = the stage one x-coordinates for location.
#      ycoord1 = the stage one y-coordinates for location.
#      popsize = the known size of the resource.
#      stage1size = the known size of the stage one sampling units.
#      pcfactor.ind = a logical value that indicates whether the population
#         correction factor is used during variance estimation, where TRUE = use
#         the population correction factor and FALSE = do not use the factor.
#      support = the support for each sampling unit.
#      swgt.ind = a logical value that indicates whether the sample is a size-
#         weighted sample, where TRUE = a size-weighted sample and FALSE = not a
#         size-weighted sample.
#      swgt = the size-weight for each site.
#      swgt1 = the stage one size-weight for each site.
#      unitsize = the known sum of the size-weights of the resource.
#      vartype = the choice of variance estimator, where "Local" = local mean
#         estimator and "SRS" = SRS estimator.
#      conf = the confidence level.
#      cdfval = the set of values at which the CDF is estimated.
#      pctval = the set of values at which percentiles are estimated.
#      subpop = a data frame describing sets of populations and subpopulations 
#         for which estimates will be calculated.
#   Output:
#      A list consisting of N.cluster, popsize, stage1size, and unitsize.
#   Other Functions Required:
#      vecprint - takes an input vector and outputs a character string with
#         line breaks inserted
################################################################################

# Check measurement error arguments

if(!is.null(sigma)) {
   if(length(sigma) > 1) {
      if(!is.numeric(sigma))
         stop("\nThe values provided for measurement error variance must be numeric.")
      temp <- sigma[!is.na(sigma)] <= 0
      if(any(temp)) {
         temp.str <- vecprint(names(sigma)[temp])
         stop(paste("\nA positive value for measurement error variance was not provided for the \nfollowing response variables:\n", temp.str, sep=""))
      }
      if(!is.null(var.sigma)) {
         if(!is.numeric(var.sigma))
            stop("\nThe values provided for variance of the measurement error variance must be \nnumeric.")
         temp <- var.sigma[!is.na(var.sigma)] <= 0
         if(any(temp)) {
            temp.str <- vecprint(names(var.sigma)[temp])
            stop(paste("\nA positive value for variance of the estimated measurement error variance was \nnot provided for the following response variables:\n", temp.str, sep=""))
         }
      }
   } else {
      if(!is.numeric(sigma))
         stop("\nThe value provided for measurement error variance must be numeric.")
      if(sigma <= 0)
         stop("\nThe value provided for measurement error variance must be positive.")
      if(!is.null(var.sigma)) {
         if(!is.numeric(var.sigma))
            stop("\nThe value provided for variance of the measurement error variance must be \nnumeric.")
         if(var.sigma <= 0)
            stop("\nThe value provided for variance of the measurement error variance must be positive.")
      }
   }
}

# Check weight arguments

if(cluster.ind) {
   if(min(wgt) <= 0)
      stop("\nStage two weights must be positive.")
   if(is.null(wgt1))
      stop("\nArgument wgt1 was not supplied.")
   if(min(wgt1) <= 0)
      stop("\nStage one weights must be positive.")
   if(stratum.ind) {
      temp.wgt1 <- split(wgt1, stratum)
      if(swgt.ind)
         temp.swgt1 <- split(swgt1, stratum)
      for(i in 1:nstrata) {
         if(any(sapply(tapply(temp.wgt1[[i]], cluster[[i]], unique), length) > 1))
            stop(paste("\nThe stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum.levels[i], ".\n\n", sep=""))
         if(swgt.ind && any(sapply(tapply(temp.swgt1[[i]], cluster[[i]], unique), length) > 1))
            stop(paste("\nThe stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit of stratum ", stratum.levels[i], ".\n\n", sep=""))
      }
   } else {
      if(any(sapply(tapply(wgt1, cluster, unique), length) > 1))
         stop("\nThe stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit.")
      if(swgt.ind && any(sapply(tapply(swgt1, cluster, unique), length) > 1))
         stop("\nThe stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit.")
   }
} else {
   if(min(wgt) <= 0)
      stop("\nWeights must be positive.")
}

# Check coordinate arguments


if(vartype == "Local") {
   if(cluster.ind) {
      if(is.null(xcoord) || is.null(ycoord))
         stop("\nStage two x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.")
      if(is.null(xcoord1) || is.null(ycoord1))
         stop("\nStage one x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.")
   } else {
      if(is.null(xcoord) || is.null(ycoord))
         stop("\nx-coordinates and y-coordinates for location are required for the local mean \nvariance estimator.")
   }
}

# Check the known size of the resource and the size-weight arguments

if(stratum.ind) {
   if(!swgt.ind && !is.null(popsize)) {
      if(is.list(popsize)) {
         npop <- dim(subpop)[2] - 1
         if(length(popsize) != npop)
            stop("\nThe known size of the resource must be provided for each population.")
         if(is.null(names(popsize)))
            stop("\nThe list of known size of the resource for each population must be named.")
         popnames <- names(subpop)[-1]
         temp <- match(popnames, names(popsize))
         if(any(is.na(temp))) 
            stop("\nThe names for the list of known size of the resource for each population must \nmatch the population names.")
         popsize <- popsize[temp]
         for(ipop in 1:npop) {
            if(!is.null(popsize[[ipop]]) && is.list(popsize[[ipop]])) {
               subpopnames <- levels(factor(subpop[,ipop+1]))
               if(is.null(names(popsize[[ipop]])))
                  stop(paste("\nThe list of known size of the resource for each subpopulation of \npopulation ", popnames[ipop], " must be named.", sep=""))
               temp <- match(subpopnames, names(popsize[[ipop]]))
               if(any(is.na(temp))) 
                  stop(paste("\nThe names for the list of known size of the resource for each subpopulation of \npopulation ", popnames[ipop], " must match the subpopulation codes.", sep=""))
               popsize[[ipop]] <- popsize[[ipop]][temp]
               for(isubpop in 1:length(subpopnames)) {
                  if(is.null(names(popsize[[ipop]][[isubpop]])))
                     stop(paste("\nThe vector of known size of the resource for each stratum for subpopulation ", subpopnames[isubpop], "\nof population ", popnames[ipop], " must be named.", sep=""))
                  subpop.ind <- subpop[,ipop+1] == subpopnames[isubpop]
                  temp <- match(levels(factor(stratum[subpop.ind])), names(popsize[[ipop]][[isubpop]]))
                  if(any(is.na(temp)))
                     stop(paste("\nThe names for the vector of known size of the resource for each stratum for \nsubpopulation ", subpopnames[isubpop], " of population ", popnames[ipop], "\nmust include the stratum codes for that subpopulation.", sep=""))
                  popsize[[ipop]][[isubpop]] <- popsize[[ipop]][[isubpop]][temp]
                  if(any(popsize[[ipop]][[isubpop]] <= 0))
                     stop(paste("\nThe known size of the resource must be positive for each stratum for subpopulation\n", subpopnames[isubpop], " of population ", popnames[i], ".", sep=""))
               }
            } else if(!is.null(popsize[[ipop]])) {
               if(is.null(names(popsize[[ipop]])))
                  stop(paste("\nThe vector of known size of the resource for each stratum in population\n", popnames[ipop], " must be named.", sep=""))
               temp <- match(stratum.levels, names(popsize[[ipop]]))
               if(any(is.na(temp)))
                  stop(paste("\nThe names for the vector of known size of the resource for each stratum in \npopulation", popnames[ipop], " must match the stratum codes \nfor that population.", sep=""))
               popsize[[ipop]] <- popsize[[ipop]][temp]
               if(any(popsize[[ipop]] <= 0))
                  stop(paste("\nThe known size of the resource must be positive for each stratum in population\n", popnames[i], ".", sep=""))
            }
         }
      } else {
         if(is.null(names(popsize)))
            stop("\nThe vector of known size of the resource for each stratum must be named.")
         temp <- match(stratum.levels, names(popsize))
         if(any(is.na(temp)))
            stop("\nThe names for the vector of known size of the resource for each stratum must \nmatch the stratum codes.")
         popsize <- popsize[temp]
         if(any(popsize <= 0))
            stop("\nThe known size of the resource must be positive for each stratum.")
      }
   } else if(swgt.ind) {
      if(!is.null(popsize))
         stop("\nThe known size of the resource and the sum of the size-weights cannot be provided \nsimultaneously.")
      if(cluster.ind) {
         if(length(swgt) != nresp)
            stop("\nThe number of stage two size-weights does not match the number of response \nvalues.")
         if(length(swgt1) != nresp)
            stop("\nThe number of stage one size-weights does not match the number of response \nvalues.")
         if(min(swgt) <= 0)
            stop("\nStage two size-weights must be positive.")
         if(min(swgt1) <= 0)
            stop("\nStage one size-weights must be positive.")
      } else {
         if(length(swgt) != nresp)
            stop("\nThe number of size-weights does not match the number of response values.")
         if(min(swgt) <= 0)
            stop("\nSize-weights must be positive.")
      }
      if(!is.null(unitsize)) {
         if(is.list(unitsize)) {
            npop <- dim(subpop)[2] - 1
            if(length(unitsize) != npop)
               stop("\nThe known sum of the size-weights must be provided for each population.")
            if(is.null(names(unitsize)))
               stop("\nThe vector of known sum of the size-weights for each population must be named.")
            popnames <- names(subpop)[-1]
            temp <- match(popnames, names(unitsize))
            if(any(is.na(temp))) 
               stop("\nThe names for the list of known sum of the size-weights for each population must \nmatch the population names.")
           unitsize <- unitsize[temp]
            for(ipop in 1:npop) {
               if(!is.null(unitsize[[ipop]]) && is.list(unitsize[[ipop]])) {
                  subpopnames <- levels(factor(subpop[,ipop+1]))
                  if(is.null(names(unitsize[[ipop]])))
                     stop(paste("\nThe list of known sum of the size-weights for each subpopulation of \npopulation ", popnames[ipop], " must be named.", sep=""))
                  temp <- match(subpopnames, names(unitsize[[ipop]]))
                  if(any(is.na(temp))) 
                     stop(paste("\nThe names for the list of known sum of the size-weights for each subpopulation of \npopulation ", popnames[ipop], " must match the subpopulation codes.", sep=""))
                  unitsize[[ipop]] <- unitsize[[ipop]][temp]
                  for(isubpop in 1:length(subpopnames)) {
                     if(is.null(names(unitsize[[ipop]][[isubpop]])))
                        stop(paste("\nThe vector of known sum of the size-weights for each stratum for subpopulation ", subpopnames[isubpop], "\nof population ", popnames[ipop], " must be named.", sep=""))
                     subpop.ind <- subpop[,ipop+1] == subpopnames[isubpop]
                     temp <- match(levels(factor(stratum[subpop.ind])), names(unitsize[[ipop]][[isubpop]]))
                     if(any(is.na(temp)))
                        stop(paste("\nThe names for the vector of known sum of the size-weights for each stratum for \nsubpopulation ", subpopnames[isubpop], " of population ", popnames[ipop], "\nmust match the stratum codes for that subpopulation.", sep=""))
                     unitsize[[ipop]][[isubpop]] <- unitsize[[ipop]][[isubpop]][temp]
                     if(any(unitsize[[ipop]][[isubpop]] <= 0))
                        stop(paste("\nThe known sum of the size-weights must be positive for each stratum for subpopulation\n", subpopnames[isubpop], " of population ", popnames[i], ".", sep=""))
                  }
               } else if(!is.null(unitsize[[ipop]])) {
                  if(is.null(names(unitsize[[ipop]])))
                     stop(paste("\nThe vector of known sum of the size-weights for each stratum in population\n", popnames[ipop], " must be named.", sep=""))
                  temp <- match(stratum.levels, names(unitsize[[ipop]]))
                  if(any(is.na(temp)))
                     stop(paste("\nThe names for the vector of known sum of the size-weights for each stratum in \npopulation", popnames[ipop], " must match the stratum codes \nfor that population.", sep=""))
                  unitsize[[ipop]] <- unitsize[[ipop]][temp]
                  if(any(unitsize[[ipop]] <= 0))
                     stop(paste("\nThe known sum of the size-weights must be positive for each stratum in population\n", popnames[i], ".", sep=""))
               }
            }
         } else {
            if(is.null(names(unitsize)))
               stop("\nThe vector of known sum of the size-weights of the resource for each stratum \nmust be named.")
            temp <- match(stratum.levels, names(unitsize))
            if(any(is.na(temp)))
               stop("\nThe names for the vector of known sum of the size-weights of the resource for \neach stratum must match the stratum codes.")
            unitsize <- unitsize[temp]
            if(min(unitsize) <= 0)
               stop("\nThe known sum of the size-weights for the resource must be positive for each \nstratum.")
         }
      }
   }
} else {
   if(!swgt.ind && !is.null(popsize)) {
      if(is.list(popsize)) {
         npop <- dim(subpop)[2] - 1
         if(length(popsize) != npop)
            stop("\nThe known size of the resource must be provided for each population.")
         if(is.null(names(popsize)))
            stop("\nThe list of known size of the resource for each population must be named.")
         popnames <- names(subpop)[-1]
         temp <- match(popnames, names(popsize))
         if(any(is.na(temp))) 
            stop("\nThe names for the list of known size of the resource for each population must \nmatch the population names.")
         for(ipop in 1:npop) {
            if(!is.null(popsize[[ipop]]) && is.list(popsize[[ipop]])) {
               subpopnames <- levels(factor(subpop[,ipop+1]))
               if(is.null(names(popsize[[ipop]])))
                  stop(paste("\nThe list of known size of the resource for each subpopulation of \npopulation ", popnames[ipop], " must be named.", sep=""))
               temp <- match(subpopnames, names(popsize[[ipop]]))
               if(any(is.na(temp))) 
                  stop("\nThe names for the list of known size of the resource for each subpopulation of \npopulation ", popnames[ipop], " must match the subpopulation codes.")
               for(isubpop in 1:length(subpopnames)) {
                  if(length(popsize[[ipop]][[isubpop]]) != 1)
                     stop(paste("\nOnly a single value should be provided for the  known size of the resource for \nsubpopulation ", subpopnames[isubpop], " of population ", popnames[i], ".", sep=""))
                  if(popsize[[ipop]][[isubpop]] <= 0)
                     stop(paste("\nThe known size of the resource must be positive for subpopulation ", subpopnames[isubpop], "\nof population ", popnames[i], ".", sep=""))
               }
            } else if(!is.null(popsize[[ipop]])) {
               if(length(popsize[[ipop]]) != 1)
                  stop(paste("\nOnly a single value should be provided for the  known size of the resource for \npopulation ", popnames[ipop], ".", sep=""))
               if(any(popsize[[ipop]] <= 0))
                  stop(paste("\nThe known size of the resource must be positive for population\n", popnames[i], ".", sep=""))
            }
         }
      } else {
         if(length(popsize) != 1)
            stop("\nOnly a single value should be provided for the known size of the resource.")
         if(popsize <= 0)
            stop("\nThe known size of the resource must be positive.")
      }
   } else if(swgt.ind) {
      if(!is.null(popsize))
         stop("\nThe known size of the resource and the sum of the size-weights cannot be provided \nsimultaneously.")
      if(cluster.ind) {
         if(length(swgt) != nresp)
            stop("\nThe number of stage two size-weights does not match the number of response \nvalues.")
         if(length(swgt1) != nresp)
            stop("\nThe number of stage one size-weights does not match the number of response \nvalues.")
         if(min(swgt) <= 0)
            stop("\nStage two size-weights must be positive.")
         if(min(swgt1) <= 0)
            stop("\nStage one size-weights must be positive.")
      } else {
         if(length(swgt) != nresp)
            stop("\nThe number of size-weights does not match the number of response values.")

         if(min(swgt) <= 0)
            stop("\nSize-weights must be positive.")
      }
      if(!is.null(unitsize)) {
         if(is.list(unitsize)) {
            npop <- dim(subpop)[2] - 1
            if(length(unitsize) != npop)
               stop("\nThe known sum of the size-weights must be provided for each population.")
            if(is.null(names(unitsize)))
               stop("\nThe vector of known sum of the size-weights for each population must be named.")
            popnames <- names(subpop)[-1]
            temp <- match(popnames, names(unitsize))
            if(any(is.na(temp))) 
               stop("\nThe names for the list of known sum of the size-weights for each population must \nmatch the population names.")
            for(ipop in 1:npop) {
               if(!is.null(unitsize[[ipop]]) && is.list(unitsize[[ipop]])) {
                  subpopnames <- levels(factor(subpop[,ipop+1]))
                  if(is.null(names(unitsize[[ipop]])))
                     stop(paste("\nThe list of known sum of the size-weights for each subpopulation in \npopulation ", popnames[ipop], " must be named.", sep=""))
                  temp <- match(subpopnames, names(unitsize[[ipop]]))
                  if(any(is.na(temp))) 
                     stop("\nThe names for the list of known sum of the size-weights for each subpopulation in \npopulation ", popnames[ipop], " must match the subpopulation codes.")
                  for(isubpop in 1:length(subpopnames)) {
                     if(length(unitsize[[ipop]][[isubpop]]) != 1)
                        stop(paste("\nOnly a single value should be provided for the  known sum of the size-weights for \nsubpopulation ", subpopnames[isubpop], " in population ", popnames[i], ".", sep=""))
                     if(unitsize[[ipop]][[isubpop]] <= 0)
                        stop(paste("\nThe known sum of the size-weights must be positive for subpopulation ", subpopnames[isubpop], "\nin population ", popnames[i], ".", sep=""))
                  }
               } else if(!is.null(unitsize[[ipop]])) {
                  if(length(unitsize[[ipop]]) != 1)
                     stop(paste("\nOnly a single value should be provided for the  known sum of the size-weights for \npopulation ", popnames[ipop], ".", sep=""))
                  if(any(unitsize[[ipop]] <= 0))
                     stop(paste("\nThe known sum of the size-weights must be positive for population\n", popnames[i], ".", sep=""))
               }
            }
         } else {
            if(length(unitsize) != 1)
               stop("\nOnly a single value should be provided for the known sum of the size-weights \nfor the resource.")
            if(unitsize <= 0)
               stop("\nThe known sum of the size-weights of the resource must be positive.")
         }
      }
   }
}

# Check the population correction factor arguments

if(pcfactor.ind) {
   if(length(support) != nresp)
      stop("\nThe number of support values does not match the number of response values.")
   if(stratum.ind) {
      if(cluster.ind) {
         if(is.null(N.cluster))
            stop("\nThe known number of stage one sampling units must be provided in order to \ncalculate the finite and continuous population correction factors for variance \nestimation in a two-stage sample.")
         if(length(N.cluster) != nstrata)
            stop("\nThe known number of stage one sampling units must be provided for each stratum \nin order to calculate the finite and continuous population correction factors \nfor variance estimation in a two-stage sample.")
         if(any(N.cluster <= 0))
            stop("\nThe known number of stage one sampling units for each stratum must be a \npositive value.")
         if(is.null(names(N.cluster)))
            stop("\nThe vector of known number of stage one sampling units for each stratum must be \nnamed.")
         N.cluster <- N.cluster[order(names(N.cluster))]
         if(sum(names(N.cluster) == stratum.levels) != nstrata) 
            stop("\nThe names for the vector of known number of stage one sampling units for each \nstratum must match the stratum codes.")
         temp <- ncluster > N.cluster
         if(any(temp)) {
            temp.str <- vecprint(names(N.cluster)[temp])
            stop(paste("\nThe number of sampled stage one sampling units exceeded the known number of \nstage one sampling units for the following strata:\n", temp.str, sep=""))
         }
         if(length(stage1size) != sum(ncluster))
            stop("\nThe known size of the stage one sampling units must be provided for each \nsampling unit in order to calculate the finite and continuous population \ncorrection factors for variance estimation in a two-stage sample.")
         if(any(stage1size <= 0))
            stop("\nThe known size of the stage one sampling units must be positive values.")
         if(is.null(names(stage1size)))
            stop("\nThe vector of known size of the stage one sampling units must be named.")
         temp <- regexpr("&", names(stage1size))
         if(any(temp < 0)) {
            temp.str <- vecprint(names(stage1size)[temp < 0])
            stop(paste("\nThe following names for the known size of the stage one sampling units were not \nvalid names (Note that names must contain a stratum code and a stage one \nsampling unit code separated by &):\n", temp.str, sep=""))
         }
         stage1stratum <- substring(names(stage1size), 1, temp-1)
         names(stage1size) <- substring(names(stage1size), temp+1)
         stage1size <- split(stage1size, stage1stratum)
         stage1size <- stage1size[order(names(stage1size))]
         if(sum(names(stage1size) == stratum.levels) != nstrata) 
            stop("\nThe names for the list of known number of stage one sampling units for each \nstratum do not match the stratum codes")
         for(i in 1:nstrata) {
            stage1size[[i]] <- stage1size[[i]][order(names(stage1size[[i]]))]
            if(sum(names(stage1size[[i]]) == cluster.levels[[i]]) != ncluster[i]) 
               stop(paste("\nThe names for the vector of known size of the stage one sampling units did not \nmatch the stage one sampling unit codes for stratum ", stratum.levels[i], ".\n\n", sep=""))
            stratum.i <- stratum == stratum.levels[i]
            temp <- tapply(support[stratum.i], cluster[[i]], sum) > stage1size[[i]]
            if(any(temp)) {
               temp.str <- vecprint(names(stage1size[[i]])[temp])
               stop(paste("\nThe sum of support values exceeded the known size of the stage one sampling \nunit for the following sampling units in stratum ", stratum.levels[i], ":\n", temp.str, sep=""))
            }
         }
      } else {
         if(is.null(popsize))
            stop("\nThe known size of the resource must be provided in order to calculate \nfinite and continuous population correction factors for variance estimation in \na single-stage sample.")
         if(is.list(popsize)) {
            for(ipop in 1:npop) {
               if(!is.null(popsize[[ipop]]) && is.list(popsize[[ipop]])) {
                  subpopnames <- levels(factor(subpop[,ipop+1]))
                  for(isubpop in 1:length(subpopnames)) {
                     subpop.ind <- subpop[,ipop+1] == subpopnames[isubpop]
                     temp <- tapply(support[subpop.ind], stratum[subpop.ind], sum) > popsize[[ipop]][[isubpop]]
                     if(any(temp)) {
                        temp.str <- vecprint(stratum.levels[temp])
                        stop(paste("\nThe sum of support values exceeded the known size of the resource for subpopulation\n", subpopnames[isubpop], " of population ", popnames[ipop], "\nfor the following strata:\n", temp.str, sep=""))
                     }
                  }
               } else if(!is.null(popsize[[ipop]])) {
                  temp <- tapply(support, stratum, sum) > popsize[[ipop]]
                  if(any(temp)) {
                     temp.str <- vecprint(stratum.levels[temp])
                     stop(paste("\nThe sum of support values exceeded the known size of the resource for population\n", popnames[ipop], " for the following strata:\n", temp.str, sep=""))
                  }
               }
            }
         } else {
            temp <- tapply(support, stratum, sum) > popsize
            if(any(temp)) {
               temp.str <- vecprint(stratum.levels[temp])
               stop(paste("\nThe sum of support values exceeded the known size of the resource for the \nfollowing strata:\n", temp.str, sep=""))
            }
         }
      }
   } else {
      if(cluster.ind) {
         if(is.null(N.cluster))
            stop("\nThe known number of stage one sampling units must be provided in order to \ncalculate the finite and continuous population correction factors for variance \nestimation in a two-stage sample.")
         if(length(N.cluster) != 1)
            stop("\nOnly a single value of the known number of stage one sampling units should be \nprovided in order to calculate the finite and continuous population correction \nfactors for variance estimation in a two-stage sample.")
         if(N.cluster <= 0)
            stop("\nThe known number of stage one sampling units must be a positive value.")
         if(ncluster > N.cluster)
            stop("\nThe number of sampled stage one sampling units exceeded the known number of \nstage one sampling units.")
         if(is.null(stage1size))
            stop("\nThe known size of the stage one sampling units must be provided in order to \ncalculate the finite and continuous population correction factors for variance \nestimation in a two-stage sample.")
         if(any(stage1size <= 0))
            stop("\nThe known size of the stage one sampling units must be positive values.")
         if(length(stage1size) != ncluster)
            stop("\nThe number of values of known size of the stage one sampling units must equal \nthe number of stage one sampling units.")
         if(is.null(names(stage1size)))
            stop("\nThe vector of known size of the stage one sampling units must be named.")
         stage1size <- stage1size[order(names(stage1size))]
         if(sum(names(stage1size) == cluster.levels) != ncluster) 
            stop("\nThe names for the vector of known size of the stage one sampling units must \nmatch the stage one sampling unit codes.")
         temp <- tapply(support, cluster, sum) > stage1size
         if(any(temp)) {
            temp.str <- vecprint(names(stage1size)[temp])
            stop(paste("\nThe sum of support values exceeded the known size of the stage one sampling \nunit for the following sampling units:\n", temp.str, sep=""))
         }
      } else {
         if(is.null(popsize))
            stop("\nThe known size of the resource must be provided in order to calculate \nfinite and continuous population correction factors for variance estimation in \na single-stage sample.")
         if(is.list(popsize)) {
            for(ipop in 1:npop) {
               if(is.list(popsize[[ipop]])) {
                  subpopnames <- levels(factor(subpop[,ipop+1]))
                  for(isubpop in 1:length(subpopnames)) {
                     subpop.ind <- subpop[,ipop+1] == subpopnames[isubpop]
                     if(sum(support[subpop.ind]) > popsize[[ipop]][[isubpop]])
                        stop(paste("\nThe sum of support values exceeded the known size of the resource for subpopulation \n", subpopnames[isubpop], " of population ", popnames[ipop], ".", sep=""))
                  }
               } else {
                  if(sum(support) > popsize[[ipop]])
                     stop(paste("\nThe sum of support values exceeded the known size of the resource for population\n", popnames[ipop], ".", sep=""))
               }
            }
         } else {
            if(sum(support) > popsize)
               stop("\nThe sum of support values exceeded the known size of the resource.")
         }
      }
   }
}

if(!is.numeric(conf))
   stop("\nThe confidence level must be a numeric value.")

if(!is.null(cdfval) && !is.numeric(cdfval))
   stop("\nThe set of value at which the CDF is estimated must be numeric values.")

if(!is.null(pctval) && !is.numeric(pctval))
   stop("\nThe set of value at which percentiles are estimated must be numeric values.")

# Return the list

list(N.cluster=N.cluster, popsize=popsize, stage1size=stage1size,
   unitsize=unitsize)
}
