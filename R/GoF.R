#' @include TBART.R
#' @include Thrf.R
#' @include XBART.R
#' @include Xhrf.R
#' @include SBART.R
#' @include Shrf.R
#' @import ranger

setClass(
  "GoF_estimator",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    feature_test = "data.frame",
    tr_test = "numeric",
    yobs_test = "numeric",
    e_estimator = "ANY"
  )
)

#' @title GoF_estimator
#' @rdname GoF_estimator
#' @description Out-of-sample Goodness-of-fit measure
#' @param feat features in data.frame.
#' @param tr treatment assignment, 0 for control and 1 for treated.
#' @param yobs the observed outcome.
#' @return A `GoF_estimator` object.
#' @export GoF_estimator
# Nameing?
GoF_estimator <-
  function(feat,
           tr,
           yobs) {
    split <- as.integer(nrow(feat) * 0.8)
    feat_train <- feat[1:split,]
    tr_train <- rbinom(nrow(feat_train), 1, .5)
    yobs_train <- feat[, 1][1:split]
    
    feat_test <- feat[, -1][split:nrow(feat),]
    tr_test <- rbinom(nrow(feat_test), 1, .5)
    yobs_test <- feat[, 1][split:nrow(feat)]
    
    df <- data.frame(feat_train, tr_train)
    df$tr_train <- as.factor(df$tr_train)
    e_est <- ranger(tr_train ~ ., data = df, probability = TRUE)
    
    new(
      "GoF_estimator",
      feature_train = feat_train,
      tr_train = tr_train,
      yobs_train = yobs_train,
      feature_test = feat_test,
      tr_test = tr_test,
      yobs_test = yobs_test,
      e_estimator = e_est
    )
  }

setGeneric(
  name = "GoF_TOT",
  def = function(theObject, learner_func) {
    standardGeneric("GoF_TOT")
  }
)

#' GoF_TOT-GoF_estimator
#' @name GoF_TOT-GoF_estimator
#' @rdname GoF_TOT-GoF_estimator
#' @description TOT method for measuring goodnes-of-fit
#' @param object A `GoF_estimator` object.
#' @param learner_func The CATE estimator to be evaluated.
#' @return The goodness-of-fit measure for the estimator.
setMethod(
  f = "GoF_TOT",
  signature = "GoF_estimator",
  definition = function(theObject, learner_func)
  {
    learner <- learner_func(feat = theObject@feature_test, 
                            tr = theObject@tr_test, 
                            yobs = theObject@yobs_test)
    estimate <- EstimateCate(learner, theObject@feature_test)
    y_te_star <- transform_yobs(theObject)
    GoF_estimate <- -sum((y_te_star - estimate)^2) / nrow(theObject@feature_test)
    return(GoF_estimate)
  }
)

setGeneric(
  name = "transform_yobs",
  def = function(theObject, feat_new, tr_new, yobs_new, ...) {
    standardGeneric("transform_yobs")
  }
)

setMethod(
  f = "transform_yobs",
  signature = "GoF_estimator",
  definition = function(theObject)
  {
    estimated_e <- estimate_propensity(theObject)
    y_te_star <- theObject@yobs_test / ((theObject@tr_test * estimated_e) 
                - (1 - theObject@tr_test) * (1 - estimated_e))
    return(y_te_star)
  }
)

setGeneric(
  name = "estimate_propensity",
  def = function(theObject) {
    standardGeneric("estimate_propensity")
  }
)

setMethod(
  f = "estimate_propensity",
  signature = "GoF_estimator",
  definition = function(theObject)
  {
    feat_test <- data.frame(theObject@feature_test)
    estimated_e <- predict(theObject@e_estimator, feat_test, seed = 123456)
    return(estimated_e$predictions[,2])
  }
)

# Example
# set.seed(123425)

# feat <- iris[, -1]
# tr <- rbinom(nrow(feat), 1, .5)
# yobs <- iris[, 1]

# gof <- GoF_estimator(feat, tr, yobs)
# estimate_propensity(gof)
# transform_yobs(gof)
# GoF_TOT(gof, T_RF)
# GoF_TOT(gof, T_BART)
# GoF_TOT(gof, S_RF)
# GoF_TOT(gof, S_BART)
# GoF_TOT(gof, X_RF)
# GoF_TOT(gof, X_BART)
