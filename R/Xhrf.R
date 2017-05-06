#' @include CATE_estimators.R

############################
### Xlearner - hRF - hRF ###
############################
#' @title XhRF constructor
#' @name X_RF-class
#' @rdname X_RF-class
#' @description The `X_RF` object is X-learner combined with honest random
#' forest used for the propensity score estimate, the first stage and the second
#' stage.
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector contain 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_0 contains an honest random forest predictor for the control group of
#' the first stage.
#' @slot m_1 contains an honest random forest predictor for the treated group of
#' the first stage.
#' @slot m_tau_0 contains an honest random forest predictor for the control
#' group of the second stage.
#' @slot m_tau_1 contains an honest random forest predictor for the treated
#' group of the second stage.
#' @slot m_prop contains an honest random forest predictor the propensity score.
#' @slot relevant_Variable_first contains the indices of variables, which are only used in
#' the first stage.
#' @slot relevant_Variable_second contains the numbers of variables, which are only used
#' in the second stage.
#' @exportClass X_RF
setClass(
  "X_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_0 = "honestRF",
    m_1 = "honestRF",
    m_tau_0 = "honestRF",
    m_tau_1 = "honestRF",
    m_prop = "honestRF",
    hyperparameter_list = "list",
    creator = "function"
  )
)


#' @title X-Learner with honest RF for both stages
#' @name X_RF-X_RF
#' @rdname X_RF-X_RF
#' @description This is an implementation of the X-learner with honest random
#' forest in the first and second stage. The function returns an X-RF object.
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param predmode One of propmean, control, treated, extreme. It specifies how
#' the two estimators of the second stage should be aggregated. The default is
#' propmean which refers to propensity score weighting.
#' @param relevant_Variable_first Variables which are only used in the first stage.
#' @param relevant_Variable_second Variables which are only used in the second stage.
#' @param ntree_first Numbers of trees in the first stage.
#' @param ntree_second Numbers of trees in the second stage.
#' @param mtry_first Numbers of trees in the second stage.
#' @param mtry_second Numbers of trees in the second stage.
#' @param min_node_size_spl_first minimum nodesize in the first stage for the
#' observations in the splitting set.
#' @param min_node_size_ave_first minimum nodesize in the first stage for the
#' observations in the average set.
#' @param min_node_size_spl_second minimum nodesize in the second stage for the
#' observations in the splitting set.
#' @param min_node_size_ave_second minimum nodesize in the second stage for the
#' observations in the averaging set.
#' @param splitratio_first Proportion of the training data used as the splitting
#' dataset in the first stage.
#' @param splitratio_second Proportion of the training data used as the
#' splitting dataset in the second stage.
#' @param replace_first Sample with or without replacement in the first stage.
#' @param replace_second Sample with or without replacement in the first stage.
#' @param sample_fraction_first The size of total samples to draw for the
#' training data in the first stage.
#' @param sample_fraction_second The size of total samples to draw for the
#' training data in the second stage.
#' @param nthread number of threats which should be used to work in parallel.
#' @param verbose whether or not to print messages of the training procedure.
#' @export X_RF
setGeneric(
  name = "X_RF",
  def = function(
    feat,
    tr,
    yobs,
    predmode,
    relevant_Variable_first,
    relevant_Variable_second,
    relevant_Variable_prop,
    ntree_first,
    ntree_second,
    ntree_prop,
    mtry_first,
    mtry_second,
    mtry_prop,
    min_node_size_spl_first,
    min_node_size_ave_first,
    min_node_size_spl_second,
    min_node_size_ave_second,
    min_node_size_spl_prop,
    min_node_size_ave_prop,
    splitratio_first,
    splitratio_second,
    splitratio_prop,
    replace_first,
    replace_second,
    replace_prop,
    sample_fraction_first,
    sample_fraction_second,
    sample_fraction_prop,
    nthread,
    middleSplit_first,
    middleSplit_second,
    middleSplit_prop,
    verbose
  ) {
    standardGeneric("X_RF")
  }
)

#' @title X_RF Constructor
#' @rdname X_RF-X_RF
#' @aliases X_RF, X_RF-X_RF
#' @return A `X_RF` object.
X_RF <-
  function(feat,
           tr,
           yobs,
           predmode = "propmean",
           relevant_Variable_first = 1:ncol(feat),
           relevant_Variable_second = 1:ncol(feat),
           relevant_Variable_prop = 1:ncol(feat),
           ntree_first = 1000,
           ntree_second = 1000,
           ntree_prop = 500,
           mtry_first = round(ncol(feat) * 13 / 20),
           mtry_second = round(ncol(feat) * 17 / 20),
           mtry_prop = ncol(feat),
           min_node_size_spl_first = 2,
           min_node_size_ave_first = 1,
           min_node_size_spl_second = 5,
           min_node_size_ave_second = 6,
           min_node_size_spl_prop = 11,
           min_node_size_ave_prop = 33,
           splitratio_first = 1,
           splitratio_second = 0.8,
           splitratio_prop = .5,
           replace_first = TRUE,
           replace_second = TRUE,
           replace_prop = TRUE,
           sample_fraction_first = 0.8,
           sample_fraction_second = 0.7,
           sample_fraction_prop =  0.5,
           nthread = 0,
           verbose = TRUE,
           middleSplit_first = TRUE,
           middleSplit_second = TRUE,
           middleSplit_prop = FALSE) {
    # if relevant_Variable_first is not set, then set it to select all:
    feat <- as.data.frame(feat)
    if (is.null(relevant_Variable_first)) {
      relevant_Variable_first <- 1:ncol(feat)
    } else{
      if (is.character(relevant_Variable_first))
        relevant_Variable_first <-
          which(colnames(feat) %in% relevant_Variable_first)
    }
    if (is.null(relevant_Variable_second)) {
      relevant_Variable_second <- 1:ncol(feat)
    } else{
      if (is.character(relevant_Variable_second))
        relevant_Variable_second <-
          which(colnames(feat) %in% relevant_Variable_second)
    }
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

    ############################################################################
    # Translate the settings to a feature list
    ############################################################################
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

    return(X_RF_fully_specified(feat = feat,
                                tr = tr,
                                yobs = yobs,
                                hyperparameter_list = hyperparameter_list,
                                verbose = verbose))
  }

#' @title X_RF_most_basic Constructor
#' @rdname X_RF_fully_specified
#' @description This is the most basic X-learner with honest random forest
#' constructor. It should not be called by the user, since the list of
#' parameters is too big. Instead call the simpler version XhRF or one of the
#' self tuning versions
#' @param feat feature data.frame.
#' @param tr treatment assignment 0 for control and 1 for treatment.
#' @param yobs the observed outcome.
#' @param hyperparameter_list A list of lists of hyper parameters
#' @param verbose TRUE for detailed output FALSE for no output
#' @return A `X_RF` object.
#' @export X_RF_fully_specified
X_RF_fully_specified <-
  function(feat,
           tr,
           yobs,
           hyperparameter_list,
           verbose) {
    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]

    X_0 <- feat[tr == 0,]
    X_1 <- feat[tr == 1,]

    m_0 <-
      honestRF(
        x = X_0[ , hyperparameter_list[["l_first_0"]]$relevant_Variable],
        y = yobs_0,
        ntree = hyperparameter_list[["l_first_0"]]$ntree,
        replace = hyperparameter_list[["l_first_0"]]$replace,
        sample.fraction = hyperparameter_list[["l_first_0"]]$sample.fraction,
        mtry = hyperparameter_list[["l_first_0"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_first_0"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_first_0"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_first_0"]]$splitratio
      )

    m_1 <-
      honestRF(
        x = X_1[ , hyperparameter_list[["l_first_1"]]$relevant_Variable],
        y = yobs_1,
        ntree = hyperparameter_list[["l_first_1"]]$ntree,
        replace = hyperparameter_list[["l_first_1"]]$replace,
        sample.fraction = hyperparameter_list[["l_first_1"]]$sample.fraction,
        mtry = hyperparameter_list[["l_first_1"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_first_1"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_first_1"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_first_1"]]$splitratio
      )

    if (verbose) {
      print("Done with the first stage.")
    }
    r_0 <- predict(m_1, X_0[, hyperparameter_list[["l_first_0"]]$relevant_Variable]) - yobs_0
    r_1 <- yobs_1 - predict(m_0, X_1[, hyperparameter_list[["l_first_1"]]$relevant_Variable])

    m_tau_0 <-
      honestRF(
        x = X_0[, hyperparameter_list[["l_second_0"]]$relevant_Variable],
        y = r_0,
        ntree = hyperparameter_list[["l_second_0"]]$ntree,
        replace = hyperparameter_list[["l_second_0"]]$replace,
        sample.fraction = hyperparameter_list[["l_second_0"]]$sample.fraction,
        mtry = hyperparameter_list[["l_second_0"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_second_0"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_second_0"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_second_0"]]$splitratio
      )

    m_tau_1 <-
      honestRF(
        x = X_1[, hyperparameter_list[["l_second_1"]]$relevant_Variable],
        y = r_1,
        ntree = hyperparameter_list[["l_second_1"]]$ntree,
        replace = hyperparameter_list[["l_second_1"]]$replace,
        sample.fraction = hyperparameter_list[["l_second_1"]]$sample.fraction,
        mtry = hyperparameter_list[["l_second_1"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_second_1"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_second_1"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_second_1"]]$splitratio
      )
    if (verbose) {
      print("Done with the second stage.")
    }

    m_prop <-
      honestRF(
        x = feat[, hyperparameter_list[["l_prop"]]$relevant_Variable],
        y = tr,
        ntree = hyperparameter_list[["l_prop"]]$ntree,
        replace = hyperparameter_list[["l_prop"]]$replace,
        sample.fraction = hyperparameter_list[["l_prop"]]$sample.fraction,
        mtry = hyperparameter_list[["l_prop"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_prop"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_prop"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_prop"]]$splitratio
      )
    if (verbose) {
      print("Done with the propensity score estimation.")
    }
    return(
      new(
        "X_RF",
        feature_train = feat,
        tr_train = tr,
        yobs_train = yobs,
        m_0 = m_0,
        m_1 = m_1,
        m_tau_0 = m_tau_0,
        m_tau_1 = m_tau_1,
        m_prop = m_prop,
        hyperparameter_list = hyperparameter_list,
        creator = function(feat, tr, yobs) {
          X_RF_fully_specified(feat,
               tr,
               yobs,
               hyperparameter_list,
               verbose)
        }
      )
    )
  }


############################
### Estimate CATE Method ###
############################
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

    predmode <- theObject@hyperparameter_list[["general"]]$predmode
    prop_scores <- predict(theObject@m_prop, feature_new)
    if (predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@m_tau_0, feature_new) +
          (1 - prop_scores)  * predict(theObject@m_tau_1, feature_new)
      )
    }
    if (predmode == "extreme") {
      return(ifelse(
        prop_scores > .5,
        predict(theObject@m_tau_0, feature_new),
        predict(theObject@m_tau_1, feature_new)
      ))
    }
    if (predmode == "control") {
      return(predict(theObject@m_tau_0, feature_new))
    }
    if (predmode == "treated") {
      return(predict(theObject@m_tau_1, feature_new))
    }
  }
)
