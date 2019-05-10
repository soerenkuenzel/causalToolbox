#' @include CATE_estimators.R
#' @import dbarts

## the standard MO_BART object
setClass(
  "MO_BART",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    ensemble = "character",
    ndpost = "numeric",
    bart_function = "function",
    ntree = "numeric",
    creator = "function"
  )
)

#' @title MO_BART
#' @rdname MO_BART
#' @description This is an implementation of MO_BART
#' @param feat A feature data frame.
#' @param tr A vector of treatment assignment 0 for control and 1 for treatment.
#' @param yobs A vector of all the observed outcomes.
#' @param ensemble TODO: Add Description
#' @param ndpost TODO: Add Description
#' @param tree_package Package used to create tree. 
#' @param ntree Number of trees to grow. 
#' @return A `MO_BART` object.
#' @export MO_BART
#' @family metalearners
#' @import methods
MO_BART <-
  function(feat,
           tr,
           yobs,
           ensemble = "pscore",
           ndpost = 1200,
           tree_package = "dbarts",
           ntree = 200) {
    feat <- as.data.frame(feat)
    
    if (tree_package == "dbarts") {
      bart_function <- function(...) {
        dbarts::bart(...)
      }
    } else if (tree_package == "BayesTree") {
      bart_function <- function(...) {
        BayesTree::bart(...)
      }
    } else if (tree_package == "BART") {
      bart_function <- function(...) {
        BART::mc.wbart(...)
      }
    } else{
      stop("tree_package must be either BayesTree, dbarts or BART")
    }
    
    
    new(
      "MO_BART",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      ensemble = ensemble,
      ndpost = ndpost,
      bart_function = bart_function,
      ntree = ntree,
      creator = function(feat, tr, yobs) {
        MO_BART(feat,
               tr,
               yobs,
               ensemble = ensemble,
               ndpost = ndpost,
               tree_package = tree_package,
               ntree = ntree)
      }
    )
  }


#' EstimateCate-MO_BART
#' EstimateCate-M_BART
#' @name EstimateCate-M_BART
#' @rdname EstimateCate
#' @inherit EstimateCate
#' @exportMethod EstimateCate
#' @import stats
setMethod(
  f = "EstimateCate",
  signature = "MO_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE,
                        return_CI = FALSE)
  {
    # theObject = xb;  verbose = TRUE; ndpost = 100; return_CI = TRUE; 
    # feature_new = feat[1:5,]; ensemble = "pscore"
    yobs <- theObject@yobs_train
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    ndpost <- theObject@ndpost
    ensemble <- theObject@ensemble
    ############################################################################
    # Constructing regression adjustment #######################################
    ############################################################################

    pred_matrix_f_0 <- theObject@bart_function(
      x.train = feat[as.logical(tr),],
      y.train = yobs[as.logical(tr)],
      x.test =  feat,
      verbose = verbose,
      ndpost = ndpost,
      ntree = theObject@ntree
    )$yhat.test
    
    mu_hat_1 <- colMeans(pred_matrix_f_0)
    
    pred_matrix_f_1 <- theObject@bart_function(
      x.train = feat[!tr,],
      y.train = yobs[!tr],
      x.test =  feat,
      verbose = verbose,
      ndpost = ndpost,
      ntree = theObject@ntree
    )$yhat.test
    
    mu_hat_0 <- colMeans(pred_matrix_f_1)
    
    prop_matrix <- theObject@bart_function(
      x.train = feat,
      y.train = factor(tr),
      x.test =  feat,
      verbose = verbose,
      ndpost = ndpost,
      ntree = theObject@ntree
    )$yhat.test
    propensity_score_hat <- pnorm(apply(prop_matrix, 2, mean))
    
    
    modified_outcome_ra <- (tr - propensity_score_hat)/(propensity_score_hat * (1 - propensity_score_hat)) *
      (yobs - mu_hat_1*(1 - propensity_score_hat) - mu_hat_0 * propensity_score_hat)
    
    if (verbose)
      print("Done estimating regression adjustment.")
    
    ############################################################################
    ### Computing tau  #########################################################
    ############################################################################
    pred_matrix_tau <- theObject@bart_function(
      x.train = feat,
      y.train = modified_outcome_ra,
      x.test =  feature_new,
      verbose = verbose,
      ndpost = ndpost,
      ntree = theObject@ntree
    )$yhat.test
    
    tau_hat <- apply(pred_matrix_tau, 2, mean)
    
    if (verbose)
      print("Done with the second stage.")
    
    return(tau_hat)
  }
)

