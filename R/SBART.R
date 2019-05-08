#'@import dbarts
#'@include CATE_estimators.R

## the standard Xlearner object with random forest
setClass(
  "S_BART",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    ndpost = "numeric",
    sample_stat = "character",
    tree_package = "character",
    ntree = "numeric",
    creator = "function"
  )
)

#' @title S_BART
#' @rdname S_BART
#' @description This is an implementation of S_BART
#' @param um.bart 
#' @param ndpost The number of posterior draws
#' @param sample_stat TODO: Add description
#' @param tree_package Package used to create a tree
#' @param ntree Number of trees to grow
#' @param verbose TRUE for detailed output FALSE for no output
#' @import methods
#' @return Object of class \code{S_RF}. It should be used with one of the 
#'   following functions \code{EstimateCATE}, \code{CateCI}, \code{CateBIAS}, 
#'   and \code{EstimateAllSampleStatistics}. The object has the following slots:
#'   \item{\code{feature_train}}{A copy of feat.}
#'   \item{\code{tr_train}}{A copy of tr.}
#'   \item{\code{yobs_train}}{A copy of yobs.}
#'   \item{\code{m_0}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the control group as the dependent variable.}
#'   \item{\code{m_1}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the treated group as the dependent variable.}
#'   \item{\code{hyperparameter_list}}{List containting the hyperparameters of 
#'      the three random forest algorithms used}
#'   \item{\code{creator}}{Function call of S_RF. This is used for different 
#'      bootstrap procedures.}
#' @inherit X_RF
#' @family metalearners
#' @export
S_BART <-
    function(feat,
             tr,
             yobs, 
             verbose = TRUE, 
             ndpost = 1200,
             sample_stat = "counterfactuals estimated",
             tree_package = "dbarts",
             ntree = 200){
    feat <- as.data.frame(feat)

    new(
      "S_BART",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      ndpost = ndpost,
      sample_stat = sample_stat,
      tree_package = tree_package,
      ntree = ntree,
      creator = function(feat, tr, yobs) {
        S_BART(feat,
               tr,
               yobs,
               ndpost = ndpost,
               sample_stat = sample_stat,
               tree_package = tree_package,
               ntree = ntree)
      }
    )
  }


#' EstimateCate-S_BART
#' @name EstimateCate-S_BART
#' @rdname EstimateCate
#' @inherit EstimateCate
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "S_BART",
  definition = function(theObject, feature_new, verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    n_feature_new <- nrow(feature_new)

    pred_matrix <-
      get_pred_mat(theObject, feature_new, verbose, ndpost)

    pred_0 <- apply(pred_matrix, 2, mean)[1:n_feature_new]
    pred_1 <-
      apply(pred_matrix, 2, mean)[(n_feature_new + 1):(2 * n_feature_new)]

    return(pred_1 - pred_0)
  }
)


#' CateCI-S_BART
#' @rdname CateCI
#' @inheritParams CateCI
#' @aliases CateCI,S_BART-method
#' @exportMethod CateCI
setMethod(
  f = "CateCI",
  signature = "S_BART",
  definition = function(theObject, feature_new, verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    n_feature_new <- nrow(feature_new)
    pred_matrix <-
      get_pred_mat(theObject, feature_new, verbose, ndpost)

    ite_matrix <-
      pred_matrix[, (n_feature_new + 1):(2 * n_feature_new)] -
      pred_matrix[, 1:n_feature_new]


    pred <- apply(ite_matrix, 2, mean)

    CI <-
      t(apply(ite_matrix, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))

    to_return <- as.data.frame(cbind(pred,  CI))
    row.names(to_return) <- 1:nrow(to_return)
    colnames(to_return) <- c('pred','X5.','X95.')
    return(to_return)
  }
)


#' EstimateAllSampleStatistics-S_BART
#' @name EstimateAllSampleStatistics-S_BART
#' @rdname EstimateAllSampleStatistics
#' @inherit EstimateAllSampleStatistics
#' @exportMethod EstimateAllSampleStatistics
#' @import stats
setMethod(
  f = "EstimateAllSampleStatistics",
  signature = "S_BART",
  definition = function(theObject, verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    yobs <- theObject@yobs_train
    sample_stat <- theObject@sample_stat

    pred_matrix <- get_pred_mat(
      theObject = theObject,
      feature_new = feat,
      verbose = verbose,
      ndpost = ndpost
    )

    n_feat <- nrow(feat)


    mu_hat_0_MCMC_samples <- pred_matrix[, 1:n_feat]
    mu_hat_1_MCMC_samples <- pred_matrix[, (n_feat + 1):(2 * n_feat)]

    return(
      compute_sample_statistics(
        mu_hat_0_MCMC_samples = mu_hat_0_MCMC_samples,
        mu_hat_1_MCMC_samples = mu_hat_1_MCMC_samples,
        ndpost = ndpost,
        feat = feat,
        tr = tr,
        yobs = yobs,
        sample_stat = sample_stat
      )
    )
  }
)

# Helper Functions -------------------------------------------------------------

get_pred_mat <- function(theObject, feature_new, verbose, ndpost) {
  feature_new <- as.data.frame(feature_new)
  n_feature_new <- nrow(feature_new)




  if (theObject@tree_package == "BayesTree") {
    pred_matrix <-
      BayesTree::bart(
        x.train = cbind(theObject@feature_train, tr = theObject@tr_train),
        y.train = theObject@yobs_train,
        x.test = cbind(rbind(feature_new, feature_new),
                       tr = c(
                         rep(0, n_feature_new), rep(1, n_feature_new)
                       )),
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test
  } else if (theObject@tree_package == "dbarts") {
    pred_matrix <-
      dbarts::bart(
        x.train = cbind(theObject@feature_train, tr = theObject@tr_train),
        y.train = theObject@yobs_train,
        x.test = cbind(rbind(feature_new, feature_new),
                       tr = c(
                         rep(0, n_feature_new), rep(1, n_feature_new)
                       )),
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test
  } else if (theObject@tree_package == "BART"){
    pred_matrix <-
      BART::mc.wbart(
        x.train = cbind(theObject@feature_train, tr = theObject@tr_train),
        y.train = theObject@yobs_train,
        x.test = cbind(rbind(feature_new, feature_new),
                       tr = c(
                         rep(0, n_feature_new), rep(1, n_feature_new)
                       )),
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test
  } else{
    stop("tree_package must be either BayesTree or dbarts")
  }
  return(pred_matrix)
}






