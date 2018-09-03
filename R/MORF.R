#' @include CATE_estimators.R
######################################
### Modified outcome adjusted - hRF ###
######################################
#' @title Modified outcome constructor
#' @name MO_RF-class
#' @rdname MO_RF-class
#' @description The `MO_RF` object is S-learner combined with honest random
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector containing 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot forest A forest object
#' @slot creator A function which creates a S_RF
#' @exportClass S_RF
#' @importFrom forestry predict
setClass(
  "MO_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    forest= "forestry",
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
#' @name MO_RF-MO_RF
#' @rdname MO_RF-MO_RF
#' @description This is an implementation of the modiifed outcome with regression
#' adjustment  combined with honest random forest for both response functions
#' @param feat A data frame of all the features.
#' @param tr A numeric vector containing 0 for control and 1 for treated
#' variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param mtry Number of variables to try at each node.
#' @param replace An indicator of whether sampling of training data is with
#' replacement. The default value is TRUE.
#' @param ntree Number of trees to grow. The default value is 1000.
#' @param sample_fraction TODO: Add Description
#' @param nthread Number of threads to train and predict the forest. The
#' default number is 4.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. The default value is 0.5.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset. The
#' default value is 3.
#' @param nodesizeSpl Minimum observations contained in terminal nodes. The
#' default value is 1.
#' @param alwaysTr weather or not we always test weather we should split on the
#'   treatment assignment. Currently only alwaysTr=FALSE is implemented.
#' @export S_RF
setGeneric(
  name = "MO_RF",
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
                 splitratio,
                 alwaysTr) {
    standardGeneric("MO_RF")
  }
)

#' @title S_RF Constructor
#' @rdname S_RF-S_RF
#' @description This is an implementation of the S-learner combined with honest
#'   random forest for both response functions
#' @aliases S_RF,S_RF-S_RF
#' @return A `S_RF` object.
#' @import methods
MO_RF <-
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
      m0 <- forestry::forestry(
        x = feat[!tr,],
        y = yobs[!tr],
        ntree = ntree,
        replace = replace,
        sampsize = round(sample_fraction * length(yobs)),
        mtry = mtry,
        nodesizeSpl = nodesizeSpl,
        nodesizeAvg = nodesizeAvg,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio
      )
      m1 <- forestry::forestry(
        x = feat[as.logical(tr),],
        y = yobs[as.logical(tr)],
        ntree = ntree,
        replace = replace,
        sampsize = round(sample_fraction * length(yobs)),
        mtry = mtry,
        nodesizeSpl = nodesizeSpl,
        nodesizeAvg = nodesizeAvg,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio
      )
      
      m0_hat <- forestry::predict(m0, feat)
      m1_hat <- forestry::predict(m1, feat)
      
      propensity_score <- forestry::forestry(
        x = feat,
        y = tr,
        ntree = ntree,
        replace = replace,
        sampsize = round(sample_fraction * length(yobs)),
        mtry = mtry,
        nodesizeSpl = nodesizeSpl,
        nodesizeAvg = nodesizeAvg,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio
      )
      #browser()
      propensity_score_hat <- forestry::predict(propensity_score, feat)
      # Fixing very low and high propensity scores. 
      propensity_score_hat <- pmin(pmax(propensity_score_hat, 0.01), 0.99)
      
      modified_outcome_ra <- (tr - propensity_score_hat)/(propensity_score_hat * (1 - propensity_score_hat)) * 
        (yobs - m1_hat*(1 - propensity_score_hat) - m0_hat * propensity_score_hat)
      
      cate_forest <- forestry::forestry(
        x = feat,
        y = modified_outcome_ra,
        ntree = ntree,
        replace = replace,
        sampsize = round(sample_fraction * length(yobs)),
        mtry = mtry,
        nodesizeSpl = nodesizeSpl,
        nodesizeAvg = nodesizeAvg,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio
      )
    }
    
    new(
      "MO_RF",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      forest = cate_forest,
      creator = function(feat, tr, yobs) {
        MO_RF(
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
#' @name EstimateCate-MO_RF
#' @rdname EstimateCate-MO_RF
#' @description Return the estimated CATE
#' @param theObject A `MO_hRF` object.
#' @param feature_new A data frame.
#' @return A vector of predicted CATE
#' @aliases EstimateCate,S_RF-method
#' @exportMethod EstimateCate
#' @importFrom forestry predict
setMethod(
  f = "EstimateCate",
  signature = "MO_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)
    return(
      forestry::predict(theObject@forest, feature_new) 
    )
  }
)
