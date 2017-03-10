





setOldClass("honestRF")

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
#' @slot firststageVar contains the indices of variables, which are only used in
#' the first stage.
#' @slot secondstageVar contains the numbers of variables, which are only used
#' in the second stage.
#' @exportClass X_RF
setClass(
  "X_RF",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_0 = "honestRF",
    m_1 = "honestRF",
    m_tau_0 = "honestRF",
    m_tau_1 = "honestRF",
    m_prop = "honestRF",
    predmode = "character",
    firststageVar = "numeric",
    secondstageVar = "numeric"
  )
)


#' @title honstRF Constructor
#' @name X_RF-X_RF
#' @rdname X_RF-X_RF
#' @description Initialize a `honestRF` object.
#' @param feat A data frame of all the features.
#' @param tr A vector contain 0 for control and 1 for treated variables.
#' @param yobs A vector containing the observed outcomes.
#' @param predmode One of propmean, control, treated, extreme. It specifies how
#' the two estimators of the second stage should be aggregated.
#' @param firststageVar Variables which are only used in the first stage.
#' @param secondstageVar Variables which are only used in the second stage.
#' @param num_trees_first Numbers of trees in the first stage.
#' @param num_trees_second Numbers of trees in the second stage.
#' @param mtry_first number of features sampled as possible split features in
#' each node in the first stage.
#' @param mtry_second number of features sampled as possible split features in
#' each node in the second stage.
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
#' @param honest_first Whether or not it should be sampled honest or not in the
#' first stage.
#' @param honest_second Whether or not it should be sampled honest or not in the
#' second stage.
#' @param honest_prop Whether or not it should be sampled honest or not in the
#' for the propensity RF.
#' @param nthread number of threats which should be used to work in parallel.
#' @export X_RF
setGeneric(
  name = "X_RF",
  def = function(feat,
                 tr,
                 yobs,
                 predmode,
                 firststageVar,
                 secondstageVar,
                 num_trees_first,
                 num_trees_second,
                 mtry_first,
                 mtry_second,
                 min_node_size_spl_first,
                 min_node_size_ave_first,
                 splitratio_first,
                 min_node_size_second,
                 replace_first,
                 replace_second,
                 sample_fraction_first,
                 sample_fraction_second,
                 honest_first,
                 honest_second,
                 honest_prop,
                 nthread,
                 min_node_size_spl_second,
                 splitratio_second,
                 min_node_size_ave_second) {
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
           firststageVar = NULL,
           secondstageVar = NULL,
           num_trees_first = 50,
           num_trees_second = 50,
           mtry_first = ncol(feat) / 2,
           mtry_second = ncol(feat),
           min_node_size_spl_first = 3,
           min_node_size_ave_first = 3,
           splitratio_first = .5,
           replace_first = FALSE,
           replace_second = TRUE,
           sample_fraction_first = 0.7,
           sample_fraction_second = 0.5,
           honest_first = TRUE,
           honest_second = TRUE,
           honest_prop = TRUE,
           nthread = 4,
           min_node_size_spl_second = 5,
           splitratio_second = .5,
           min_node_size_ave_second = 5) {
    # if firststageVar is not set, then set it to select all:
    if (is.null(firststageVar)) {
      firststageVar <- 1:ncol(feat)
    } else{
      if (is.character(firststageVar))
        firststageVar <-
          which(colnames(feat) %in% firststageVar)
    }
    if (is.null(secondstageVar)) {
      secondstageVar <- 1:ncol(feat)
    } else{
      if (is.character(secondstageVar))
        secondstageVar <-
          which(colnames(feat) %in% secondstageVar)
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

    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]

    X_0 <- feat[tr == 0,]
    X_1 <- feat[tr == 1,]

    m_0 <-
      honestRF(
        x = X_0[, firststageVar],
        y = yobs_0,
        ntree = num_trees_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_0)),
        mtry = mtry_first,
        nodesize = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    m_1 <-
      honestRF(
        x = X_1[, firststageVar],
        y = yobs_1,
        ntree = num_trees_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_1)),
        mtry = mtry_first,
        nodesize = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    r_0 <- predict(m_1, X_0) - yobs_0
    r_1 <- yobs_1 - predict(m_0, X_1)

    m_tau_0 <-
      honestRF(
        x = X_0[, secondstageVar],
        y = r_0,
        ntree = num_trees_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_0)),
        mtry = mtry_second,
        nodesize = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )

    m_tau_1 <-
      honestRF(
        x = X_1[, secondstageVar],
        y = r_1,
        ntree = num_trees_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_1)),
        mtry = mtry_second,
        nodesize = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )
    m_prop <-
      honestRF(x = feat,
               y = tr,
               ntree = num_trees_second)
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
        predmode = predmode,
        firststageVar = firststageVar,
        secondstageVar = secondstageVar
      )
    )
  }


############################
### Estimate CATE Method ###
############################
setGeneric(name="EstimateCate",
           def=function(theObject, feature_new)
           {
             standardGeneric("EstimateCate")
           }
)

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
    prop_scores <- predict(theObject@m_prop, feature_new)
    if (theObject@predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@m_tau_0, feature_new) +
          (1 - prop_scores)  * predict(theObject@m_tau_1, feature_new)
      )
    }
    if (theObject@predmode == "extreme") {
      return(
        ifelse(prop_scores > .5,
               predict(theObject@m_tau_0, feature_new),
               predict(theObject@m_tau_1, feature_new))
      )
    }
    if (theObject@predmode == "control") {
      return(
        predict(theObject@m_tau_0, feature_new)
      )
    }
    if (theObject@predmode == "treated") {
      return(
        predict(theObject@m_tau_1, feature_new)
      )
    }
  }
)
