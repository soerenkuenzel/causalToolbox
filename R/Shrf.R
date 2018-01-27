#' @include CATE_estimators.R



############################
### Tlearner - hRF ###
############################
#' @title ThRF constructor
#' @name S_RF-class
#' @rdname S_RF-class
#' @description The `S_RF` object is T-learner combined with honest random
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector contain 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_y_t contains an honest random forest predictor for the treated group
#' @slot m_y_c contains an honest random forest predictor for the control group
#' @slot forest TODO: Add Description
#' @slot creator function which creates a S_RF
#' @exportClass S_RF
#' @importFrom forestry predict
setClass(
  "S_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    forest = "honestRF",
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
#' @name S_RF-S_RF
#' @rdname S_RF-S_RF
#' @description This is an implementation of the T-learner combined with honest
#'   random forest for both response functions
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param mtry Number of variables to try at each node.
#' @param nodesize TODO: Add Description
#' @param replace TODO: Add Description
#' @param ntree TODO: Add Description
#' @param sample_fraction TODO: Add Description
#' @param nthread TODO: Add Description
#' @param splitratio TODO: Add Description
#' @param nodesizeAvg TODO: Add Description
#' @param alwaysTr weather or not we always test weather we should split on the
#'   treatment assignment. Currently only alwaysTr=FALSE is implemented
#' @export S_RF
setGeneric(
  name = "S_RF",
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
                 nodesizeAvg,
                 alwaysTr) {
    standardGeneric("S_RF")
  }
)

#' @title S_RF Constructor
#' @rdname S_RF-S_RF
#' @description This is an implementation of the T-learner combined with honest
#'   random forest for both response functions
#' @aliases S_RF,S_RF-S_RF
#' @return A `S_RF` object.
#' @import methods
S_RF <-
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
           splitratio = .5,
           alwaysTr = FALSE) {
    feat <- as.data.frame(feat)

    if ((!is.null(mtry)) && (mtry > ncol(feat) + 1)) {
      warning(
        "mtry is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry <- ncol(feat) + 1
    }

    if (alwaysTr) {
      stop("always trying to split on the treatment variable is currently not
           implemented")
    } else{
      m <- forestry::honestRF(
        x = cbind(feat, tr),
        y = yobs,
        ntree = ntree,
        replace = replace,
        sampsize = round(sample_fraction * length(yobs)),
        mtry = mtry,
        nodesize = nodesizeSpl,
        nodesizeAvg = nodesizeAvg,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio
      )
    }
    new(
      "S_RF",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      forest = m,
      creator = function(feat, tr, yobs) {
        S_RF(
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
          splitratio = splitratio,
          alwaysTr = alwaysTr
        )
      }
    )
  }

############################
### Estimate CATE Method ###
############################
#' EstimateCate-S_hRF
#' @name EstimateCate-S_RF
#' @rdname EstimateCate-S_RF
#' @description Return the estimated CATE
#' @param object A `S_hRF` object.
#' @param feature_new A data frame.
#' @return A vector of predicted CATE
#' @aliases EstimateCate,S_RF-method
#' @exportMethod EstimateCate
#' @importFrom forestry predict
setMethod(
  f = "EstimateCate",
  signature = "S_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)

    return(
      forestry::predict(theObject@forest, cbind(feature_new, tr = 1)) -
        forestry::predict(theObject@forest, cbind(feature_new, tr = 0))
    )
  }
)
