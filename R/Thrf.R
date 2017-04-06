# setOldClass("honestRF")

############################
### Tlearner - hRF ###
############################
#' @title ThRF constructor
#' @name T_RF-class
#' @rdname T_RF-class
#' @description The `T_RF` object is T-learner combined with honest random
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector contain 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_y_t contains an honest random forest predictor for the treated group
#' @slot m_y_c contains an honest random forest predictor for the control group
#' @slot creator function which creates a T_RF
#' @exportClass T_RF
setClass(
  "T_RF",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_y_t = "ranger",
    m_y_c = "ranger",
    creator = "function"
  ),
  validity = function(object)
  {
    if (!all(object@tr.train %in% c(0, 1))) {
      return("TR is the treatment and must be either 0 or 1")
    }
    return(TRUE)
  }
)


#' @title honstRF Constructor
#' @name T_RF
#' @rdname T_RF
#' @description This is an implementation of the T-learner combined with honest
#' random forest for both response functions
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param mtry Number of variables to try at each node.
#' @param nodesize ...asdasd
#' @param replace ...asdasda
#' @param ntree ...asdasd
#' @export T_RF
setGeneric(
  name = "X_RF",
  def = function(feat,
                 tr,
                 yobs,
                 mtry,
                 nodesize,
                 replace,
                 ntree,
                 sample_fraction,
                 nthread,
                 splitratio,
                 nodesizeAvg) {
    standardGeneric("X_RF")
  }
)

#' @title X_RF Constructor
#' @rdname X_RF-X_RF
#' @aliases X_RF, X_RF-X_RF
#' @return A `X_RF` object.
T_RF <-
  function(feat,
           tr,
           yobs,
           mtry = ncol(feat),
           nodesizeSpl = 1,
           nodesizeAvg = 1,
           replace = TRUE,
           ntree = 5000,
           sample_fraction = 0.9,
           nthread = 4,
           splitratio = .5) {
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


    m_y_c <- honestRF(
      x = X_0,
      y = yobs_0,
      ntree = ntree,
      replace = replace,
      sampsize = round(sample_fraction * length(yobs_0)),
      mtry = mtry,
      nodesize = nodesizeSpl,
      nodesizeAvg = nodesizeAvg,
      nthread = nthread,
      splitrule =  'variance',
      splitratio = splitratio
    )

    m_y_t <- honestRF(
      x = X_1,
      y = yobs_1,
      ntree = ntree,
      replace = replace,
      sampsize = round(sample_fraction * length(yobs_0)),
      mtry = mtry,
      nodesize = nodesizeSpl,
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
setGeneric(
  name = "EstimateCate",
  def = function(theObject, feature_new)
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
  signature = "T_RF",
  definition = function(theObject, feature.new)
  {
    return(
      predict(theObject@m_y_t, data = feature.new)$predictions -
        predict(theObject@m_y_c, data = feature.new)$predictions
    )
  }
)
