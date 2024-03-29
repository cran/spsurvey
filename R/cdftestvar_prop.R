################################################################################
# Function: cdftestvar_prop (not exported)
# Programmer: Tom Kincaid
# Date: October 23, 2020
# Revised: November 2, 2020 to correctly process the column variable when it
#          includes missing (NA) values
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Local Mean Variance/Covariance Estimates of Estimated Population Proportions
#'
#' This function calculates the local mean estimate of the variance/covariance
#' matrix of the population proportions for a contingency table.  The function
#' can accomodate single-stage and two-stage samples.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'   the x- coordinate for a single-stage sample or the stage two x-coordinate
#'   for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'   the y- coordinate for a single-stage sample or the stage two y-coordinate
#'   for a two-stage sample.
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level Vector indicating the stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two- stage sample, where \code{TRUE} = a two-stage sample and
#'   \code{FALSE} = not a two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @param x1 Vector of the stage one x-coordinate for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinate for location for each site.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df A data frame for storing warning messages.
#'
#' @param warn_vec Character vector that contains a subpopulation name, the
#'   first subpopulation level, the second subpopulation level, and an
#'   indicator name.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{varest}}{matrix containing the variance/covariance estimates
#'       for the contingency table total estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

cdftestvar_prop <- function(design, wgt, x, y, stratum_ind, stratum_level,
                            cluster_ind, clusterID, wgt1, x1, y1, warn_ind,
                            warn_df, warn_vec) {

  # Assign the function name

  fname <- "cdftestvar_prop"

  # Assign the variance type

  vartype <- "Local"

  # Create the model matrix for the contingency table using all of the data

  frm_cells <- eval(bquote(~ interaction(
    factor(.(as.name("rowvar"))),
    factor(.(as.name("colvar")))
  ) - 1))
  mm_cells <- model.matrix(frm_cells, model.frame(frm_cells, design$variables,
    na.action = na.pass
  ))

  # Calculate contingency table means

  means <- svymean(mm_cells, design, na.rm = TRUE)
  names_means <- names(means)
  m <- length(names_means)

  #
  # Branch to handle two-stage and single-stage samples
  #

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_lst <- split(design$variables$colvar, cluster)
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    tw2 <- (sum(wgt1 * wgt))^2
    x2_lst <- split(x, cluster)
    y2_lst <- split(y, cluster)
    x1_u <- as.vector(tapply(x1, cluster, unique))
    y1_u <- as.vector(tapply(y1, cluster, unique))
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # For each stage one sampling unit and each contingency table cell,
    # calculate an estimate of the total of the stage two sampling unit
    # residuals; then calculate the variance/covariance matrix of the totals

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m^2)
    for (i in 1:ncluster) {

      # Create the model matrix for the contingency table using using a single
      # cluster

      design_var <- subset(design$variables, cluster == cluster_levels[i] &
        !(is.na(rowvar) | is.na(colvar)))
      mm_cluster <- model.matrix(frm_cells, model.frame(frm_cells, design_var,
        na.action = na.pass
      ))

      # Calculate the weighted residuals matrix

      n <- length(z_lst[[i]])
      m_cl <- ncol(mm_cluster)
      tst <- names_means %in% colnames(mm_cluster)
      rm <- (mm_cluster - matrix(rep(means[tst], n), nrow = n, byrow = TRUE)) *
        matrix(rep(wgt2_lst[[i]], m_cl), nrow = n)

      # Calculate total estimates for the stage one sampling unit

      total2est[i, tst] <- apply(rm, 2, sum)

      # Adjust the variance/covariance estimator for small sample size

      if (var_ind[i] && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\", \nthe simple random sampling covariance estimator for an infinite population was used \nto calculate covariance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
      }

      # Calculate variance/covariance estimates for the stage one sampling unit

      if (var_ind[i]) {
        tst <- rep(tst, m)
        if (vartype == "Local") {
          weight_lst <- localmean_weight(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt2_lst[[i]]
          )
          if (is.null(weight_lst)) {
            warn_ind <- TRUE
            act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
            if (stratum_ind) {
              warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \ncovariance estimator for an infinite population was used to calculate covariance of the \ntotal of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = stratum_level,
                warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            }
            var2est[i, tst] <- as.vector(n * var(rm))
          } else {
            temp <- localmean_cov(rm, weight_lst)
            var2est[i, ] <- as.vector(temp)
            if (any(diag(temp) < 0)) {
              warn_ind <- TRUE
              act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
              if (stratum_ind) {
                warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstage one sampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the total of the residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3],
                  stratum = stratum_level, warning = I(warn), action = I(act)
                ))
              } else {
                warn <- paste0("The local mean covariance estimator produced one or more  negative variance estimates \nfor stage one sampling unit \"", cluster_levels[i], "\", the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the total of \nthe residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                  warning = I(warn), action = I(act)
                ))
              }
              var2est[i, tst] <- as.vector(n * var(rm))
            }
          }
        } else {
          var2est[i, tst] <- as.vector(n * var(rm))
          vartype <- "Local"
        }
      }
    }

    # Adjust the variance/covariance estimator for small sample size

    if (ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", the simple \nrandom sampling covariance estimator for an infinite population was used to calculate \ncovariance of the contingency table proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling covariance\nestimator for an infinite population was used to calculate covariance of the contingency \ntable proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = NA, warning = I(warn), action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance/covariance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the contingency table proportion \nestimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the contingency table proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, m_cl),
          nrow = ncluster
        )) + matrix(apply(var2est * matrix(rep(wgt1_u, m_cl^2),
          nrow = ncluster
        ), 2, sum), nrow = m_cl)) / tw2
      } else {
        varest <- (localmean_cov(total2est * matrix(rep(wgt1_u, m_cl),
          nrow = ncluster
        ), weight_lst) + matrix(apply(var2est *
          matrix(rep(wgt1_u, m_cl^2), nrow = ncluster), 2, sum), nrow = m_cl)) /
          tw2
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the contingency table proportion \nestimates.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the contingency table proportion estimates.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, m_cl),
            nrow = ncluster
          )) + matrix(apply(var2est *
            matrix(rep(wgt1_u, m_cl^2), nrow = ncluster), 2, sum),
          nrow = m_cl
          )) / tw2
        }
      }
    } else {
      varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, m_cl),
        nrow = ncluster
      )) + matrix(apply(var2est * matrix(rep(wgt1_u, m_cl^2),
        nrow = ncluster
      ), 2, sum), nrow = m_cl)) / tw2
    }
    colnames(varest) <- names_means

    # End of section for a two-stage sample
  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    tw2 <- (sum(wgt))^2

    # Calculate the weighted residuals matrix

    mm_cells <- subset(mm_cells, !(is.na(design$variables$rowvar) |
      is.na(design$variables$colvar)))
    n <- nrow(mm_cells)
    rm <- (mm_cells - matrix(rep(means, n), nrow = n, byrow = TRUE)) *
      matrix(rep(wgt, m), nrow = n)

    # Adjust the variance/covariance estimator for small sample size

    if (n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple \nrandom sampling covariance estimator for an infinite population was used to calculate \ncovariance of the contingency table proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the contingency \ntable proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance/covariance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x, y, 1 / wgt)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the contingency table proportion \nestimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the contingency table proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        varest <- n * var(rm) / tw2
      } else {
        varest <- localmean_cov(rm, weight_lst) / tw2
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the contingency table proportion \nestimates.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = I(stratum_level),
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the contingency table proportion estimates.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          varest <- n * var(rm) / tw2
        }
      }
    } else {
      varest <- n * var(rm) / tw2
    }
    colnames(varest) <- names_means

    # End of section for a single-stage sample
  }

  # Return the variance estimates, warning message indicator, and the warn_df
  # data frame

  list(varest = varest, warn_ind = warn_ind, warn_df = warn_df)
}
