#' @include CATE_estimators.R
## the standard Xlearner object with random forest
setClass(
  "X_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    base_learners = "list",
    predmode = "character",
    creator = "function"
  )
)

#' @title X_RF_most_basic Constructor
#' @rdname X_RF_fully_specified
#' @aliases X_RF_fully_specified
#' @return A `X_RF_fully_specified` object.
#' @export X_RF_fully_specified
X_RF_fully_specified <-
  function(feat,
           tr,
           yobs,
           hyperparameter_list,
           verbose) {
    # train the base_learners of the first stage:
    base_learners <- list()
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
        r_0 <-
          predict(base_learners[["l_first_1"]],
                  X_0[, hyperparameter_list[["l_first_0"]]$relevant_Variable]) -
          yobs_0

        x = X_0
        y = r_0
      } else if (this_learner == "l_second_1") {
        r_1 <-
          yobs_1 -
          predict(base_learners[["l_first_0"]],
                  X_1[,  hyperparameter_list[["l_first_1"]]$relevant_Variable])

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
        honestRF(
          x = x[, hyperparameter_list[[this_learner]]$relevant_Variable],
          y = y,
          ntree = hyperparameter_list[[this_learner]]$ntree,
          replace = hyperparameter_list[[this_learner]]$replace,
          sample.fraction = hyperparameter_list[[this_learner]]$sample.fraction,
          mtry = hyperparameter_list[[this_learner]]$mtry,
          nodesizeSpl = hyperparameter_list[[this_learner]]$nodesizeSpl,
          nodesizeAvg = hyperparameter_list[[this_learner]]$nodesizeAvg,
          nthread = hyperparameter_list[["general"]]$nthread,
          splitrule = "variance",
          splitratio = hyperparameter_list[[this_learner]]$splitratio
        )
    }
    if (verbose) {
      print("Done with the propensity score estimation.")
    }

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

#' @title X_RF Constructor
#' @rdname X_RF-X_RF
#' @aliases X_RF, X_RF-X_RF
#' @return A `X_RF` object.
#' @export X_RF
X_RF <-
  function(feat,
           tr,
           yobs,
           predmode = "propmean",
           relevant_Variable_first = 1:ncol(feat),
           relevant_Variable_second = 1:ncol(feat),
           relevant_Variable_prop = 1:ncol(feat),
           ntree_first = 500,
           ntree_second = 500,
           ntree_prop = 500,
           mtry_first = round(ncol(feat) / 2),
           mtry_second = ncol(feat),
           mtry_prop = max(floor(ncol(feat) / 3), 1),
           min_node_size_spl_first = 1,
           min_node_size_ave_first = 5,
           min_node_size_spl_second = 5,
           min_node_size_ave_second = 3,
           min_node_size_spl_prop = 3,
           min_node_size_ave_prop = 10,
           splitratio_first = .5,
           splitratio_second = .5,
           splitratio_prop = .5,
           replace_first = TRUE,
           replace_second = TRUE,
           replace_prop = TRUE,
           sample_fraction_first = 0.8,
           sample_fraction_second = 0.9,
           sample_fraction_prop = 1,
           nthread = 0,
           verbose = FALSE,
           middleSplit_first = FALSE,
           middleSplit_second = FALSE,
           middleSplit_prop = FALSE) {
    # if relevant_Variable_first is not set, then set it to select all:
    feat <- as.data.frame(feat)

    if(is.character(relevant_Variable_first))
      relevant_Variable_first <-
        which(colnames(feat) %in% relevant_Variable_first)
    if(is.character(relevant_Variable_second))
    relevant_Variable_second <-
        which(colnames(feat) %in% relevant_Variable_second)
    if(is.character(relevant_Variable_prop))
      relevant_Variable_prop <-
        which(colnames(feat) %in% relevant_Variable_prop)

    if ((!is.null(mtry_first)) &&
        (mtry_first > ncol(feat))) {
      warning(
        "mtry_first is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry_first <- ncol(feat)
    }
    if ((!is.null(mtry_second)) &&
        (mtry_second > ncol(feat))) {
      warning(
        "mtry_second is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry_second <- ncol(feat)
    }

    general_hyperpara <- list("predmode" = predmode,
                              "nthread" = nthread)
    first_stage_hyperpara <- list(
      "relevant_Variable" = relevant_Variable_first,
      "ntree" = ntree_first,
      "replace" = replace_first,
      "sample.fraction" = sample_fraction_first,
      "mtry" = mtry_first,
      "nodesizeSpl" = min_node_size_spl_first,
      "nodesizeAvg" = min_node_size_ave_first,
      "splitratio" = splitratio_first,
      "middleSplit" = middleSplit_first
    )
    second_stage_hyperpara <- list(
      "relevant_Variable" = relevant_Variable_second,
      "ntree" = ntree_second,
      "replace" = replace_second,
      "sample.fraction" = sample_fraction_second,
      "mtry" = mtry_second,
      "nodesizeSpl" = min_node_size_spl_second,
      "nodesizeAvg" = min_node_size_ave_second,
      "splitratio" = splitratio_second,
      "middleSplit" = middleSplit_second
    )
    prop_hyperpara <- list(
      "relevant_Variable" = relevant_Variable_prop,
      "ntree" = ntree_prop,
      "replace" = replace_prop,
      "sample.fraction" = sample_fraction_prop,
      "mtry" = mtry_prop,
      "nodesizeSpl" = min_node_size_spl_prop,
      "nodesizeAvg" = min_node_size_ave_prop,
      "splitratio" = splitratio_prop,
      "middleSplit" = middleSplit_prop
    )

    hyperparameter_list <- list(
      "general" = general_hyperpara,
      "l_first_0" = first_stage_hyperpara,
      "l_first_1" = first_stage_hyperpara,
      "l_second_0" = second_stage_hyperpara,
      "l_second_1" = second_stage_hyperpara,
      "l_prop" = prop_hyperpara
    )

    return(
      X_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = hyperparameter_list ,
        verbose = verbose
      )
    )
  }


#' EstimateCate-X_hRF
#' @name EstimateCate-X_hRF
#' @rdname EstimateCate-X_hRF
#' @description Return the estimated CATE
#' @param object A `X_hRF` object.
#' @param feature_new A data frame.
#' @return A vector of predicted CATE
#' @aliases EstimateCate, X_hRF-method
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "X_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)

    prop_scores <-
      predict(theObject@base_learners[["l_prop"]], feature_new)
    if (theObject@predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@base_learners[["l_second_0"]], feature_new) +
          (1 - prop_scores)  * predict(theObject@base_learners[["l_second_1"]], feature_new)
      )
    }
    if (theObject@predmode == "extreme") {
      return(ifelse(
        prop_scores > .5,
        predict(theObject@base_learners[["l_second_0"]], feature_new),
        predict(theObject@base_learners[["l_second_1"]], feature_new)
      ))
    }
    if (theObject@predmode == "control") {
      return(predict(theObject@base_learners[["l_second_0"]], feature_new))
    }
    if (theObject@predmode == "treated") {
      return(predict(theObject@base_learners[["l_second_1"]], feature_new))
    }
  }
)
