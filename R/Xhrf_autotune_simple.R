#' @include Xhrf.R


#' @title Autotuning for X-Learner with honest RF for both stages
#' @name X_RF_autotune_simple
#' @rdname X_RF_autotune_simple
#' @description This function tunes
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param ntree ..
#' @param Niter ..
#' @param nthread ..
#' @export X_RF_autotune_simple
X_RF_autotune_simple <-
  function(feat,
           tr,
           yobs,
           ntree = 20000,
           nthread = 0,
           verbose = TRUE) {
    starting_settings <- list(
      "start_setting_1" = get_setting_strong(feat,  tr, ntree, nthread),
      "start_setting_2" = get_setting_weak(feat, tr, ntree, nthread)
    )

    setup_eval <-
      check_setups(starting_settings, feat, tr, yobs, ntree,
                   nthread, verbose)
    if (verbose)
      print(paste(names(starting_settings)[which.min(setup_eval$comb)],
                  "is the best."))
    return(
      X_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = starting_settings[[which.min(setup_eval$comb)]],
        verbose = verbose
      )
    )
  }

# This function checks the starting_settings using the OOB errors from those
# It returns a table containing the expected performance of each of the setups
check_setups <-
  function(starting_settings,
           feat,
           tr,
           yobs,
           ntree = 20000,
           nthread = 0,
           verbose = TRUE) {
    # define starting points 1 and 2

    OOB_errors <- data.frame()
    for (i in 1:length(starting_settings)) {
      if (verbose)
        print(paste0("Checking setup ", i, " out of ", length(starting_settings)))
      starting_setting <- starting_settings[[i]]
      starting_setting_name <- names(starting_settings)[i]
      OOB_errors <- rbind(OOB_errors,
                          evaluate_setting(starting_setting, feat, tr, yobs))
    }
    OOB_errors$tau_0 <- OOB_errors$l_first_1 + OOB_errors$l_second_0
    OOB_errors$tau_1 <- OOB_errors$l_first_0 + OOB_errors$l_second_1
    OOB_errors$comb <- (OOB_errors$tau_0 + OOB_errors$tau_1) / 2
    OOB_errors <-
      cbind(data.frame("name" = names(starting_settings)),
            OOB_errors)

    return(OOB_errors)
  }

# This function evaluates a setting by and returns the OOB error of the four
# base learners.
evaluate_setting <- function(setting, feat, tr, yobs) {
  # setting <- starting_setting
  x_eval <- X_RF_fully_specified(feat,
                                 tr,
                                 yobs,
                                 hyperparameter_list = setting,
                                 verbose = FALSE)

  return(
    data.frame(
      "l_first_0" = getOOB(x_eval@base_learners[["l_first_0"]], noWarning = FALSE),
      "l_first_1" = getOOB(x_eval@base_learners[["l_first_1"]], noWarning = FALSE),
      "l_second_0" = getOOB(x_eval@base_learners[["l_second_0"]], noWarning = FALSE),
      "l_second_1" = getOOB(x_eval@base_learners[["l_second_1"]], noWarning = FALSE)
    )
  )
}

################################################################################
################################################################################
# Define serveral standard settings:

get_setting_strong <- function(feat,
                               tr,
                               ntree,
                               nthread,
                               relevant_Variable_first = 1:ncol(feat),
                               relevant_Variable_second = 1:ncol(feat),
                               relevant_Variable_prop = 1:ncol(feat)) {
  hyperparameter_list <- list(
    "general" = list("predmode" = "propmean",
                     "nthread" = nthread),
    "l_first_0" = list(
      "relevant_Variable" = relevant_Variable_first,
      "ntree" = ntree,
      "replace" = TRUE,
      "sampsize" = round(.8 * sum(1 - tr)),
      "mtry" = round(ncol(feat) / 2),
      "nodesizeSpl" = 1,
      "nodesizeAvg" = 5,
      "splitratio" = .5,
      "middleSplit" = FALSE
    ),
    "l_first_1" = list(
      "relevant_Variable" = relevant_Variable_first,
      "ntree" = ntree,
      "replace" = TRUE,
      "sampsize" = round(.8 * sum(tr)),
      "mtry" = round(ncol(feat) / 2),
      "nodesizeSpl" = 1,
      "nodesizeAvg" = 5,
      "splitratio" = .5,
      "middleSplit" = FALSE
    ),
    "l_second_0" = list(
      "relevant_Variable" = relevant_Variable_second,
      "ntree" = ntree,
      "replace" = TRUE,
      "sampsize" = round(.9 * sum(1 - tr)),
      "mtry" = ncol(feat),
      "nodesizeSpl" = 5,
      "nodesizeAvg" = 3,
      "splitratio" = .5,
      "middleSplit" = FALSE
    ),
    "l_second_1" = list(
      "relevant_Variable" = relevant_Variable_second,
      "ntree" = ntree,
      "replace" = TRUE,
      "sampsize" = round(.9 * sum(tr)),
      "mtry" = ncol(feat),
      "nodesizeSpl" = 5,
      "nodesizeAvg" = 3,
      "splitratio" = .5,
      "middleSplit" = FALSE
    ),
    "l_prop" = list(
      "relevant_Variable" = relevant_Variable_prop,
      "ntree" = ntree,
      "replace" = TRUE,
      "sampsize" = round(0.9 * length(tr)),
      "mtry" = ncol(feat),
      "nodesizeSpl" = 5,
      "nodesizeAvg" = 3,
      "splitratio" = .5,
      "middleSplit" = FALSE
    )
  )
  return(hyperparameter_list)
}


get_setting_weak <- function(feat,
                             tr,
                             ntree,
                             nthread,
                             relevant_Variable_first = 1:ncol(feat),
                             relevant_Variable_second = 1:ncol(feat),
                             relevant_Variable_prop = 1:ncol(feat)) {
  hyperparameter_list <- list(
    "general" = list("predmode" = "propmean",
                     "nthread" = nthread),
    "l_first_0" = list(
      "relevant_Variable" = relevant_Variable_first,
      "ntree" = ntree,
      "replace" = FALSE,
      "sampsize" = round(.7 * sum(1 - tr)),
      "mtry" = round(ncol(feat) / 2),
      "nodesizeSpl" = 1,
      "nodesizeAvg" = 1,
      "splitratio" = .4,
      "middleSplit" = FALSE
    ),
    "l_first_1" = list(
      "relevant_Variable" = relevant_Variable_first,
      "ntree" = ntree,
      "replace" = FALSE,
      "sampsize" = round(.7 * sum(tr)),
      "mtry" = round(ncol(feat) / 2),
      "nodesizeSpl" = 1,
      "nodesizeAvg" = 1,
      "splitratio" = .4,
      "middleSplit" = FALSE
    ),
    "l_second_0" = list(
      "relevant_Variable" = relevant_Variable_second,
      "ntree" = ntree,
      "replace" = FALSE,
      "sampsize" = round(0.8 * sum(1 - tr)),
      "mtry" = max(1, round(ncol(feat) / 5)),
      "nodesizeSpl" = 10,
      "nodesizeAvg" = 3,
      "splitratio" = .75,
      "middleSplit" = FALSE
    ),
    "l_second_1" = list(
      "relevant_Variable" = relevant_Variable_second,
      "ntree" = ntree,
      "replace" = FALSE,
      "sampsize" = round(0.8 * sum(tr)),
      "mtry" = max(1, round(ncol(feat) / 5)),
      "nodesizeSpl" = 10,
      "nodesizeAvg" = 3,
      "splitratio" = .75,
      "middleSplit" = FALSE
    ),
    "l_prop" = list(
      "relevant_Variable" = relevant_Variable_prop,
      "ntree" = ntree,
      "replace" = FALSE,
      "sampsize" = round(0.8 * length(tr)),
      "mtry" = max(1, round(ncol(feat) / 5)),
      "nodesizeSpl" = 10,
      "nodesizeAvg" = 3,
      "splitratio" = .75,
      "middleSplit" = FALSE
    )
  )
  return(hyperparameter_list)
}
