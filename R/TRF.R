#' @include CATE_estimators.R



############################
### Tlearner - hRF ###
############################
#' @title ThRF constructor
#' @name T_RF-class
#' @rdname T_RF-class
#' @description The `T_RF` object is T-learner combined with honest random
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector containing 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_y_t contains an honest random forest predictor for the treated group
#' @slot m_y_c contains an honest random forest predictor for the control group
#' @slot creator A function which creates a T_RF
#' @exportClass T_RF
setClass(
  "T_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_y_t = "forestry",
    m_y_c = "forestry",
    creator = "function"
  ),
  validity = function(object)
  {
    if (!all(object@tr_train %in% c(0, 1))) {
      return("TR is the treatment and must be either 0 or 1")
    }
    return(TRUE)
  }
)


#' @title honstRF Constructor
#' @name T_RF-T_RF
#' @rdname T_RF-T_RF
#' @description This is an implementation of the T-learner combined with honest
#' random forest for both response functions
#' @param feat A data frame of all the features.
#' @param tr A numeric vector containing 0 for control and 1 for treated 
#' variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param mtry Number of variables to try at each node.
#' @param nodesizeSpl Minimum observations contained in terminal nodes. The
#' default value is 1.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset. The
#' default value is 3.
#' @param replace  An indicator of whether sampling of training data is with 
#' replacement. The default value is TRUE.
#' @param ntree Number of trees to grow. The default value is 1000.
#' @param sample_fraction TODO: Add Description
#' @param nthread Number of threads to train and predict the forest. The
#' default number is 4.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. The default value is 0.5.
#' @export T_RF
setGeneric(
  name = "T_RF",
  def = function(feat,
                 tr,
                 yobs,
                 mtry,
                 nodesizeSpl,
                 nodesizeAvg,
                 replace,
                 ntree,
                 sample_fraction,
                 nthread,
                 splitratio) {
    standardGeneric("T_RF")
  }
)

#' @title T_RF Constructor
#' @rdname T_RF-T_RF
#' @description This is an implementation of the T-learner combined with honest
#' random forest for both response functions
#' @aliases T_RF,T_RF-T_RF
#' @return A `T_RF` object.
#' @import methods
T_RF <-
  function(feat,
           tr,
           yobs,
           mtry = ncol(feat),
           nodesizeSpl = 1,
           nodesizeAvg = 3,
           replace = TRUE,
           ntree = 1000,
           sample_fraction = 0.9,
           nthread = 4,
           splitratio = .5) {
    feat <- as.data.frame(feat)

    if ((!is.null(mtry)) && (mtry > ncol(feat))) {
      warning(
        "mtry is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry <- ncol(feat)
    }


    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]

    X_0 <- feat[tr == 0, ]
    X_1 <- feat[tr == 1, ]


    m_y_c <- forestry::forestry(
      x = X_0,
      y = yobs_0,
      ntree = ntree,
      replace = replace,
      sampsize = round(sample_fraction * length(yobs_0)),
      mtry = mtry,
      nodesizeSpl = nodesizeSpl,
      nodesizeAvg = nodesizeAvg,
      nthread = nthread,
      splitrule =  'variance',
      splitratio = splitratio
    )

    m_y_t <- forestry::forestry(
      x = X_1,
      y = yobs_1,
      ntree = ntree,
      replace = replace,
      sampsize = round(sample_fraction * length(yobs_1)),
      mtry = mtry,
      nodesizeSpl = nodesizeSpl,
      nodesizeAvg = nodesizeAvg,
      nthread = nthread,
      splitrule =  'variance',
      splitratio = splitratio
    )


    new(
      "T_RF",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      m_y_t = m_y_t,
      m_y_c = m_y_c,
      creator = function(feat, tr, yobs) {
        T_RF(
          feat,
          tr,
          yobs,
          mtry = mtry,
          nodesizeSpl = nodesizeSpl,
          nodesizeAvg = nodesizeAvg,
          replace = replace,
          ntree = ntree,
          sample_fraction = sample_fraction,
          nthread = nthread,
          splitratio = splitratio
        )
      }
    )
  }

############################
### Estimate CATE Method ###
############################
#' EstimateCate-T_hRF
#' @name EstimateCate-T_RF
#' @rdname EstimateCate-T_RF
#' @description Return the estimated CATE
#' @param theObject A `T_hRF` object.
#' @param feature_new A feature data frame.
#' @return A vector of predicted CATE
#' @aliases EstimateCate,T_RF-method
#' @exportMethod EstimateCate

setMethod(
  f = "EstimateCate",
  signature = "T_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)

    return(
      predict(theObject@m_y_t, feature_new) -
        predict(theObject@m_y_c, feature_new)
    )
  }
)

