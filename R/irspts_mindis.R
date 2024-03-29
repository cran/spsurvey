###############################################################################
# Function: grtspts_mindis (not exported)
# Programmer:  Tony Olsen
# Date: January 22, 2021
#
#' Select an irs sample with minimum distance between sites.
#'
#' @inheritParams grtspts_mindis
#'
#' @return sites A list of \code{sf} object of sample points, an \code{sf} object of over sample
#'   points if any, warning indicator and warning messages \code{data.frame}.
#'
#' @author Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

irspts_mindis <- function(mindis, sframe, samplesize, stratum, maxtry = 10,
                          legacy_option = NULL, legacy_var = NULL,
                          warn_ind = NULL, warn_df = NULL) {

  # select initial set of sites
  if (nrow(sframe) <= samplesize) {
    samp_id <- sframe$idpts
  } else {
    # randomly shuffle site order
    sframe <- sframe[sample(1:nrow(sframe)), ]
    s <- UPpivotal(sframe$ip)
    samp_id <- sframe$idpts[round(s) == 1]
  }
  # extract sites from sample frame
  sites_base <- sframe[sframe$idpts %in% samp_id, ]

  # calculate distance between sites
  site_dist <- st_distance(sites_base)
  class(site_dist) <- "numeric"
  nr <- nrow(sites_base)

  # find sites less than mindis and set to FALSE otherwise set to TRUE
  keep <- apply(site_dist, 1, function(x) {
    ifelse(any(x[x > 0] < mindis), FALSE, TRUE)
  })

  # if any legacy sites keep those sites
  if (legacy_option == TRUE) {
    keep[sites_base$legacy] <- TRUE
  }

  # see if any sites are less than mindis and check until none or max tries
  ntry <- 1
  while (any(!keep)) {
    # identify sites that will be treated as legacy probability sites in sample frame
    sframe$probdis <- FALSE
    sframe$probdis[sframe$idpts %in% sites_base$idpts[keep]] <- TRUE

    # if any true legacy sites add them to sites to be kept
    if (legacy_option == TRUE) {
      sframe$probdis[sframe$legacy] <- TRUE
    }

    # Adjust initial inclusion probabilities to account for current mindis sites
    # and any legacy sites
    sframe$ip <- grtspts_ipleg(sframe$ip_init, sframe$probdis)

    # select new sites that include legacy sites
    samp_id <- sample(sframe$idpts, samplesize, prob = sframe$ip)
    # extract sites from sample frame
    sites <- sframe[sframe$idpts %in% samp_id, ]

    # calculate distance between sites
    site_dist <- st_distance(sites_base)
    class(site_dist) <- "numeric"
    nr <- nrow(sites_base)

    # identify sites less than mindis
    keep <- apply(site_dist, 1, function(x) {
      ifelse(any(x[x > 0] < mindis), FALSE, TRUE)
    })

    # Change to TRUE if any legacy sites
    if (legacy_option == TRUE) {
      keep[sites_base$legacy] <- TRUE
    }

    # check if maxtry reached. If so write out warning message
    if (ntry >= maxtry) {
      keep <- rep(TRUE, nr)
      warn <- paste0("Minimum distance between sites not attained after ", maxtry, " attempts.")
      if (warn_ind) { # this code has a bug but is never called (the rbind fails because warn_df has a column warn and the bind has a column warning)
        warn_df <- rbind(warn_df, data.frame(
          stratum = stratum, func = I("grtspts_mindis"),
          warning = warn
        ))
      } else {
        warn_ind <- TRUE
        warn_df <- data.frame(stratum = stratum, func = I("grtspts_mindis"), warning = warn)
      }
    } else {
      ntry <- ntry + 1
    }
  } # end of ntry loop

  # drop internal variables
  tmp <- names(sites_base)
  sites_base <- subset(sites_base, select = tmp[!(tmp %in% c("probdis", "geometry"))])

  # Put sites in reverse hierarchical order
  sites_base <- rho(sites_base)
  sites_base$siteuse <- NA
  sites_base$replsite <- NA

  sites <- list(
    sites = sites_base,
    warn_ind = warn_ind, warn_df = warn_df
  )

  invisible(sites)
}
