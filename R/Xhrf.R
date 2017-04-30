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
#' @slot firststageVar contains the indices of variables, which are only used in
#' the first stage.
#' @slot secondstageVar contains the numbers of variables, which are only used
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
    predmode = "character",
    firststageVar = "numeric",
    secondstageVar = "numeric",
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
#' @param firststageVar Variables which are only used in the first stage.
#' @param secondstageVar Variables which are only used in the second stage.
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
  def = function(feat,
                 tr,
                 yobs,
                 predmode,
                 firststageVar,
                 secondstageVar,
                 ntree_first,
                 ntree_second,
                 mtry_first,
                 mtry_second,
                 min_node_size_spl_first,
                 min_node_size_ave_first,
                 min_node_size_spl_second,
                 min_node_size_ave_second,
                 splitratio_first,
                 splitratio_second,
                 replace_first,
                 replace_second,
                 sample_fraction_first,
                 sample_fraction_second,
                 nthread,
                 verbose) {
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
           ntree_first = 500,
           ntree_second = 500,
           mtry_first = round(ncol(feat) / 2),
           mtry_second = ncol(feat),
           min_node_size_spl_first = 1,
           min_node_size_ave_first = 5,
           min_node_size_spl_second = 5,
           min_node_size_ave_second = 3,
           splitratio_first = .5,
           splitratio_second = .5,
           replace_first = TRUE,
           replace_second = TRUE,
           sample_fraction_first = 0.8,
           sample_fraction_second = 0.9,
           nthread = 4,
           verbose = TRUE) {
    # if firststageVar is not set, then set it to select all:
    feat <- as.data.frame(feat)
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
        ntree = ntree_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_0)),
        mtry = mtry_first,
        nodesizeSpl = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    m_1 <-
      honestRF(
        x = X_1[, firststageVar],
        y = yobs_1,
        ntree = ntree_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_1)),
        mtry = mtry_first,
        nodesizeSpl = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    if (verbose) {
      print("Done with the first stage.")
    }
    r_0 <- predict(m_1, X_0[, firststageVar]) - yobs_0
    r_1 <- yobs_1 - predict(m_0, X_1[, firststageVar])

    m_tau_0 <-
      honestRF(
        x = X_0[, secondstageVar],
        y = r_0,
        ntree = ntree_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_0)),
        mtry = mtry_second,
        nodesizeSpl = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )

    m_tau_1 <-
      honestRF(
        x = X_1[, secondstageVar],
        y = r_1,
        ntree = ntree_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_1)),
        mtry = mtry_second,
        nodesizeSpl = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )
    if (verbose) {
      print("Done with the second stage.")
    }

    m_prop <-
      honestRF(
        x = feat,
        y = tr,
        ntree = 500,
        nthread = nthread,
        splitratio = .5,
        nodesizeAvg = 10
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
        predmode = predmode,
        firststageVar = firststageVar,
        secondstageVar = secondstageVar,
        creator = function(feat, tr, yobs) {
          X_RF(feat,
               tr,
               yobs,
               predmode = predmode,
               firststageVar = firststageVar,
               secondstageVar = secondstageVar,
               ntree_first = ntree_first,
               ntree_second = ntree_second,
               mtry_first = mtry_first,
               mtry_second = mtry_second,
               min_node_size_spl_first = min_node_size_spl_first,
               min_node_size_ave_first = min_node_size_ave_first,
               min_node_size_spl_second = min_node_size_spl_second,
               min_node_size_ave_second = min_node_size_ave_second,
               splitratio_first = splitratio_first,
               splitratio_second = splitratio_second,
               replace_first = replace_first,
               replace_second = replace_second,
               sample_fraction_first = sample_fraction_first,
               sample_fraction_second = sample_fraction_second,
               nthread = nthread,
               verbose = FALSE)
        }
      )
    )
  }


############################
### Estimate CATE Method ###
############################
# This is documented out, because we use the include statement in the beginning
# of the document to be able to load the generic in many different places

# setGeneric(name="EstimateCate",
#            def=function(theObject, feature_new)
#            {
#              standardGeneric("EstimateCate")
#            }
# )

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

    prop_scores <- predict(theObject@m_prop, feature_new)
    if (theObject@predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@m_tau_0, feature_new) +
          (1 - prop_scores)  * predict(theObject@m_tau_1, feature_new)
      )
    }
    if (theObject@predmode == "extreme") {
      return(ifelse(
        prop_scores > .5,
        predict(theObject@m_tau_0, feature_new),
        predict(theObject@m_tau_1, feature_new)
      ))
    }
    if (theObject@predmode == "control") {
      return(predict(theObject@m_tau_0, feature_new))
    }
    if (theObject@predmode == "treated") {
      return(predict(theObject@m_tau_1, feature_new))
    }
  }
)
