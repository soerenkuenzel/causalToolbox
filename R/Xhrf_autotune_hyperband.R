#' @include Xhrf.R


#' @title Autotuning for X-Learner with honest RF for both stages
#' @name X_RF_autotune_hyperband
#' @rdname X_RF_autotune_hyperband
#' @description This function tunes
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param sampsize ..
#' @param num_iter number of iterations.
#' @param eta ..
#' @param firststageVar ..
#' @param secondstageVar ..
#' @param verbose ..
#' @param seed ..
#' @param nthread ..
#' @export X_RF_autotune_hyperband
X_RF_autotune_hyperband <-
  function(feat,
           tr,
           yobs,
           sample.fraction = 0.75,
           num_iter = 3 ^ 8,
           eta = 3,
           verbose = TRUE,
           seed = 24750371,
           nthread = 0) {
    feat <- as.data.frame(feat)

    hyperparameter_list <- list()
    base_learners <- list()
    # this_learner <- "l_first_0"
    for (this_learner in c("l_first_0",
                           "l_first_1",
                           "l_second_0",
                           "l_second_1",
                           "l_prop")) {
      if (this_learner == "l_first_0") {
        yobs_0 <- yobs[tr == 0]
        X_0 <- feat[tr == 0,]
        x = X_0
        y = yobs_0
      } else if (this_learner == "l_first_1") {
        yobs_1 <- yobs[tr == 1]
        X_1 <- feat[tr == 1,]
        x = X_1
        y = yobs_1
      } else if (this_learner == "l_second_0") {
        if (verbose) {
          print("Done with the first stage.")
        }
        r_0 <- predict(base_learners[["l_first_1"]], X_0) - yobs_0
        x = X_0
        y = r_0
      } else if (this_learner == "l_second_1") {
        r_1 <- yobs_1 - predict(base_learners[["l_first_0"]], X_1)
        x = X_1
        y = r_1
      } else{
        if (verbose) {
          print("Done with the second stage.")
        }
        # must be propensity learner
        x = feat
        y = tr
      }

      base_learners[[this_learner]] <-
        autohonestRF(
          x = x,
          y = y,
          sampsize = floor(nrow(x) * sample.fraction),
          num_iter = num_iter,
          eta = eta,
          verbose = verbose,
          seed = seed,
          nthread = nthread
        )

      hyperparameter_list[[this_learner]] <- list(
        "relevant_Variable" = 1:ncol(feat) ,
        "ntree" = base_learners[[this_learner]]@ntree,
        "replace" = base_learners[[this_learner]]@replace,
        "sampsize" = base_learners[[this_learner]]@sampsize,
        "mtry" = base_learners[[this_learner]]@mtry,
        "nodesizeSpl" = base_learners[[this_learner]]@nodesizeSpl,
        "nodesizeAvg" = base_learners[[this_learner]]@nodesizeAvg,
        "splitratio" = base_learners[[this_learner]]@splitratio,
        "middleSplit" = base_learners[[this_learner]]@middleSplit
      )
    }
    if (verbose) {
      print("Done with the propensity score estimation.")
    }
    hyperparameter_list[["general"]] <- list("predmode" = "propmean",
                                             "nthread" = nthread)

    return(
      new(
        "X_RF",
        feature_train = feat,
        tr_train = tr,
        yobs_train = yobs,
        base_learners = base_learners,
        predmode = hyperparameter_list[["general"]]$predmode,
        creator = function(feat, tr, yobs) {
          X_RF_fully_specified(
            feat = feat,
            tr = tr,
            yobs = yobs,
            hyperparameter_list = hyperparameter_list,
            verbose = verbose
          )
        }
      )
    )
  }
