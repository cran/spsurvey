###############################################################################
# Function: dsgn_check (not exported)
# Programmer: Tony Olsen
# Date: January 22, 2021
#'
#' Check the input associated with the survey design list object and the sample frame
#'
#' @param sframe Sample frame as an \code{sf} object.
#'
#' @param sf_type Geometry type for sample frame: point, linear, or area.
#'
#' @param legacy_sites An \code{sf} object of any legacy sites to be included in the survey
#'   design.
#'
#' @param legacy_option Logical variable where if equal to \code{TRUE}, legacy sites are
#'   to be incorporated into survey design.
#'
#' @param stratum Single character value or character vector that identifies the strata for
#'   the design.  Default is \code{NULL} which means no stratification and all elements in the sample
#'   frame are assumed to be included.
#'
#' @param seltype Single character value or character vector that identifies the type of
#'   random selection, which must be one of following: \code{"equal"} for equal probability selection,
#'   \code{"unequal"} for unequal probability selection by the categories specified in \code{caty_n} or
#'   \code{"proportional"} for unequal probability selection proportional to the auxiliary variable
#'   \code{aux_var}. If single character, then seltype applies to all strata. If vector, then each
#'   stratum may have different selection type. Default is single character value of \code{"equal"}.
#'
#' @param n_base The sample size required. If single stratum, then single numeric value.
#'   If sample is stratified, then numeric vector with same length as \code{"stratum"} and sample sizes
#'   required in same order as strata in \code{"stratum"}. Must be specified.
#'
#' @param caty_n If design is not stratified and seltype is \code{"unequal"}, a named character vector
#'   with the expected sample size for each category specified in variable \code{caty_var}. If the design
#'   is stratified, then either a named character vector with the expected sample size for each
#'   category for all strata or if the expected sample size for each category may differ, then
#'   a list of named character vectors with the expected sample size for each category in the
#'   stratum. The list must be in same order as the \code{"stratum"} variable. For each stratum,
#'   the sum of \code{caty_n} values must equal \code{n_base} for that stratum. Default is \code{NULL}.
#'
#' @param n_over If seltype is \code{"equal"} and is not stratified, a numeric value specifying the
#'   over sample size requested. If seltype is \code{"equal"} and is stratified either a numeric value
#'   specifying the over sample size that will be applied to each stratum or a numeric vector
#'   specifying the over sample size for each stratum listed in same order as \code{"strata"}.
#'   If seltype is \code{"unequal"} and is not stratified, a named character vector with the over sample size
#'   for each category where names are the same as \code{"caty_n"}. If seltype is \code{"unequal"} and is
#'   stratified,  either a numeric vector specifying the over sample size for each category in
#'   \code{"caty_n"} that will be applied to each stratum or a list of named numeric vectors with
#'   the over sample size for each \code{"caty_n"} category for each stratum. List must be in same order
#'   as the \code{"stratum"} variable order. Default is \code{NULL}.
#'
#' @param n_near Numeric value specifying the number of nearby points to select as
#'   possible replacement sites if a site cannot be sampled. Default is \code{NULL}. If specified,
#'   must be integer from \code{1} to \code{10}.
#'
#' @param stratum_var Character string containing the name of the column from
#'   \code{sframe} that identifies stratum membership for each element in the frame.
#'   If stratum equals \code{NULL}, the design is unstratified and all elements in sample frame
#'   are eligible to be selected in the sample. The default is \code{NULL}.
#'
#' @param caty_var Character string containing the name of the column from
#'   \code{sframe} that identifies the unequal probability category for each element
#'   in the frame.  Default is \code{NULL}.
#'
#' @param aux_var Character string that is the name of the column from \code{sframe} that
#'   identifies the auxiliary variable value for each element in the sample frame
#'   that will be used to calculate inclusion probabilities when the survey design
#'   specifies that the selection type (seltype) is \code{"proportional"}. Default is \code{NULL}.
#'
#' @param legacy_var Character value for name of column for legacy site variable.
#'   Default is \code{NULL}.
#'
#' @param mindis Numeric value for the minimum distance required between elements
#'   in the sample. If design is stratified, then mindis applies only within each stratum.
#'   Units must be the same units as in \code{sf} geometry. Default is \code{NULL}.
#'
#' @param DesignID Name for the design, which is used to create a site ID for
#'   each site.  Default is \code{"Site"}.
#'
#' @param SiteBegin Number to use for first site in the design.  Default is \code{1}.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance (mindis) between sites.
#'   Default is \code{10}.
#'
#'  @param projcrs_check A check for whether the coordinates are projected. If \code{TRUE},
#'    an error is returned if coordinates are not projected. If \code{FALSE}, the
#'    check is not performed and the raw coordinates in the geometry column of
#'    \code{sframe} (and \code{legacy_sites} if provided) are used.
#'
#'
#' @return Nothing is returned. If errors are found they are collected and written out.
#'   One or more errors will cause the call to \code{grts()} to stop.
#'
#'
#' @author Tony Olsen email{olsen.tony@@epa.gov}
#'
#' @noRd
###############################################################################
dsgn_check <- function(sframe, sf_type, legacy_sites, legacy_option, stratum, seltype, n_base, caty_n,
                       n_over, n_near, stratum_var, caty_var, aux_var,
                       legacy_stratum_var, legacy_caty_var, legacy_aux_var,
                       legacy_var, mindis,
                       DesignID, SiteBegin, maxtry, projcrs_check) {

  # Create a data frame for stop messages
  stop_ind <- FALSE
  stop_df <- NULL

  # check that coordinates are NA or geographic # | st_is_longlat(sframe))
  if (projcrs_check & is.na(st_crs(sframe))) {
    stop_ind <- TRUE
    stop_mess <- "The coordinate reference system (crs) for sframe is NA. The coordinate reference system for sframe should instead use projected coordinates. For more information on geographic and projected coordinates, see spsurvey's \"Start Here\" vignette by running vignette(\"start-here\", \"spsurvey\"). To override the check for projected coordinates, set projcrs_check = FALSE."
    stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
  }

  if (projcrs_check & st_is_longlat(sframe)) {
    stop_ind <- TRUE
    stop_mess <- "The coordinate reference system (crs) for sframe is geographic. The coordinate reference system for sframe should instead use projected coordinates. For more information on geographic and projected coordinates, see spsurvey's \"Start Here\" vignette by running vignette(\"start-here\", \"spsurvey\"). To override the check for projected coordinates, set projcrs_check = FALSE."
    stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
  }

  # check that legacy and sframe coordinates match
  if (!is.null(legacy_sites)) {
    if (sum(is.na(st_crs(sframe)), is.na(st_crs(legacy_sites))) == 1) {
      stop_ind <- TRUE
      stop_mess <- "sframe and legacy_sites must have the same crs. If crs should be ignored completely, run st_crs(sframe) <- NA and st_crs(legacy_sites) <- NA"
      stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
    } else if (st_crs(sframe) != st_crs(legacy_sites)) {
      stop_ind <- TRUE
      stop_mess <- "sframe and legacy_sites must have the same crs. If crs should be ignored completely, run st_crs(sframe) <- NA and st_crs(legacy_sites) <- NA"
      stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
    }
  }

  # check that sframe has required variables for stratum, caty, aux and legacy
  # If stratum_var is provided, does the attribute exist in sframe
  if (!is.null(stratum_var)) {
    if (match(stratum_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for stratum variable does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
    }
  }

  # If caty_var is provided, does the attribute exist in sframe
  if (!is.null(caty_var)) {
    if (match(caty_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for unequal probability category variable does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("caty_var"), I(stop_mess)))
    }
  }

  # If aux_var is provided, does the attribute exist in sframe
  if (!is.null(aux_var)) {
    if (match(aux_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for the auxillary variable for proportional sampling does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
    }
    # ensure class for aux variable is numeric
    if (!is.numeric(sframe[[aux_var]])) {
      stop_ind <- TRUE
      stop_mess <- "The auxillary variable in sample frame for proportional sampling must be numeric."
      stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
    } else {
      # check that values are > 0.
      if (any(sframe[[aux_var]] <= 0)) {
        stop_ind <- TRUE
        stop_mess <- "The auxillary variable for proportional sampling must have all values greater than zero"
        stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
      }
    }
  }

  # If legacy_var is provided, does the attribute exist in sframe
  if (sf_type == "sf_point" & !is.null(legacy_var)) {
    if (match(legacy_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for the variable identifying legacy sites does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("legacy_var"), I(stop_mess)))
    }
  }

  ### Check legacy_sites sf object if present
  if (sf_type %in% c("sf_point", "sf_linear", "sf_area") & !is.null(legacy_sites)) {
    # check that legacy_sites has required variables for stratum, caty, aux and legacy
    # If stratum_var is provided, does the attribute exist
    if (!is.null(stratum_var) & is.null(legacy_stratum_var)) {
      if (match(stratum_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for stratum variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If caty_var is provided, does the attribute exist
    if (!is.null(caty_var) & is.null(legacy_caty_var)) {
      if (match(caty_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for caty variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If aux_var is provided, does the attribute exist
    if (!is.null(aux_var) & is.null(legacy_aux_var)) {
      if (match(aux_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for aux variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If legacy_var is provided, does the attribute exist
    if (!is.null(legacy_var)) {
      if (match(legacy_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for legacy variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
  }


  ##### Check design components to ensure they provide what is required.

  # check if stratum is provided and values are in sframe
  if (!is.null(stratum)) {
    if (is.null(stratum_var)) {
      stop_ind <- TRUE
      stop_mess <- "Design is stratified and no 'stratum_var' is provided."
      stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
    } else {
      if (any(stratum %in% unique(sframe[[stratum_var]]) == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Not all stratum values are in sample frame.")
        stop_df <- rbind(stop_df, data.frame(func = I("stratum"), I(stop_mess)))
      }
    }
  }

  # check seltype
  if (any(seltype %in% c("equal", "unequal", "proportional") == FALSE)) {
    stop_ind <- TRUE
    stop_mess <- paste0("seltype must be 'equal', 'unequal' or 'proportional'.")
    stop_df <- rbind(stop_df, data.frame(func = I("seltype"), I(stop_mess)))
  }

  # check seltype when caty_var and aux_var provided
  if (all(seltype %in% "unequal") | all(seltype %in% "proportional")) {
    if (!is.null(caty_var) & !is.null(aux_var)) {
      stop_ind <- TRUE
      stop_mess <- paste0("aux_var and caty_var cannot both be specified when all elements of seltype are the same.")
      stop_df <- rbind(stop_df, data.frame(func = I("seltype mismatch"), I(stop_mess)))
    }
  }

  # check caty_var and caty_n are provided together
  if ((is.null(caty_var) & !is.null(caty_n)) | (!is.null(caty_var) & is.null(caty_n))) {
    stop_ind <- TRUE
    stop_mess <- paste0("caty_n and caty_var must be provided together.")
    stop_df <- rbind(stop_df, data.frame(func = I("caty_var and caty_n"), I(stop_mess)))
  }

  # check n_base length and stratum_var are provided together
  if (length(n_base) > 1 & is.null(stratum_var)) {
    stop_ind <- TRUE
    stop_mess <- paste0("if the length of n_base is larger than 1 then stratification is assumed and stratum_var must be provided.")
    stop_df <- rbind(stop_df, data.frame(func = I("n_base and stratum_var"), I(stop_mess)))
  }

  # check names of caty_n when it is a list
  if (is.list(caty_n) & is.null(names(caty_n))) {
    stop_ind <- TRUE
    stop_mess <- paste0("caty_n must be a named list (and these names must match the strata)")
    stop_df <- rbind(stop_df, data.frame(func = I("caty_n names"), I(stop_mess)))
  }


  # check n_base
  if (any(n_base <= 0)) {
    stop_ind <- TRUE
    stop_mess <- paste0("Sample size must be integers greater than 0.")
    stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
  }

  # check total sample size
  if (sf_type == "sf_point") {
    if (length(stratum) > 1) {
      if (any(sapply(stratum, function(x) n_base[x] > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
        stop_ind <- TRUE
        stop_mess <- paste0("Each stratum must have a sample size no larger than the number of rows in 'sframe' representing that stratum")
        stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
      }
    } else {
      if (n_base > NROW(sframe)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Sample size must be no larger than the number of rows in 'sframe'")
        stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
      }
    }
  }

  # check caty_n
  if (!is.null(caty_n)) {
    if (!is.list(caty_n)) {
      if (any(names(caty_n) %in% unique(sframe[[caty_var]]) == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Not all caty_n values are in sample frame.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
      tst <- function(x, caty_n) {
        x != sum(caty_n)
      }
      if (any(sapply(n_base, tst, caty_n))) {
        stop_ind <- TRUE
        stop_mess <- paste0("Sum of caty_n values do not equal n_base.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
    }
    if (is.list(caty_n)) {
      if (any(names(caty_n) %in% stratum == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Names for caty_n list are not values in 'stratum' variable.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
        stop_mess <- paste0("For each stratum make sure caty_n values in 'caty_var' variable.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
      if (any(sapply(stratum, function(x) sum(caty_n[[x]]) != n_base[[x]]))) {
        stop_ind <- TRUE
        stop_mess <- paste0("The sum of the 'caty_n' values in each strata must match the value in n_base that corresponds to the respective strata")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
    }
  }

  # check n_over
  if (!is.null(n_over)) {
    if (is.null(stratum) | length(stratum) == 1) {
      if (any(seltype %in% c("equal", "proportional"))) {
        if (n_over < 0) {
          stop_ind <- TRUE
          stop_mess <- paste0("n_over value must be zero or positive.")
          stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
        }
      }
      if (any(seltype == "unequal")) {
        if (!is.null(caty_n)) {
          if (!is.list(n_over)) {
            if (any(n_over < 0)) {
              stop_ind <- TRUE
              stop_mess <- paste0("n_over values must be zero or positive.")
              stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
            }
          }
        }
      }
    }
    if (length(stratum) > 1) {
      if (any(seltype %in% c("equal", "proportional", "unequal"))) {
        if (!is.list(n_over)) {
          if (any(n_over < 0)) {
            stop_ind <- TRUE
            stop_mess <- paste0("n_over values must be zero or positive.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
          }
          
          # fixes a bug downstream in checking sizes of n_base + n_over
          if (length(n_over) == 1) {
            n_over <- rep(n_over, length(stratum))
          }
          if (is.null(names(n_over)) || all(names(n_over) %in% stratum)) {
            names(n_over) <- stratum
          }
          n_over <- as.list(n_over)
        }
        if (is.list(n_over)) {
          if (any(names(n_over) %in% stratum == FALSE)) {
            stop_ind <- TRUE
            stop_mess <- paste0("Names for n_over list are not values in 'stratum' variable.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
            stop_mess <- paste0("For each stratum make sure n_over values are non-negative.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
          }
        }
      }
    }
  }

  # check total sample size for n_over
  if (sf_type == "sf_point") {
    if (!is.null(n_over)) {
      if (length(stratum) > 1) {
        if (is.list(n_over)) {
          if (any(sapply(stratum, function(x) (n_base[[x]] + ifelse(is.null(n_over[[x]]), 0, sum(n_over[[x]]))) > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
            stop_ind <- TRUE
            stop_mess <- paste0("For each stratum, the sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe' representing that stratum.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
          }
        } else {
           if (any(sapply(stratum, function(x) (n_base[[x]] + sum(n_over[[x]])) > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
            stop_ind <- TRUE
            stop_mess <- paste0("For each stratum, the sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe' representing that stratum.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
          }
        }
      } else {
        if ((n_base + sum(n_over)) > NROW(sframe)) {
          stop_ind <- TRUE
          stop_mess <- paste0("The sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe'.")
          stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
        }
      }
    }
  }

  # check n_near
  if (!is.null(n_near)) {
    if (!(all(unlist(n_near) %in% 1:10))) {
      stop_ind <- TRUE
      stop_mess <- paste0("values of n_near must be from 1 to 10.\n")
      stop_df <- rbind(stop_df, data.frame(func = I("n_near"), I(stop_mess)))
    }
  }

  # find system info
  on_solaris <- Sys.info()[["sysname"]] == "SunOS"
  if (on_solaris) {
    stop_ind <- TRUE
    stop_mess <- paste0("grts() and irs() are not supported on Solaris.")
    stop_df <- rbind(stop_df, data.frame(func = I("Solaris"), I(stop_mess)))
  }

  ### If any issues, write out stop_df and then stop
  if (stop_ind) {
    names(stop_df) <- c("Design Input", "Error Message")
    stop_df <<- stop_df
    message("During the check of the input to grtspts, one or more errors were identified.\n")
    message("Enter the following command to view all input error messages: stopprnt()\n")
    message("To view a subset of the errors (e.g., errors 1 and 5) enter stopprnt(m=c(1,5))\n\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
}
