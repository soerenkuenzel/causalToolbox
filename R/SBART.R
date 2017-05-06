#' @include CATE_estimators.R
#' @import dbarts

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
#' @param feat feature data.frame.
#' @param tr treatment assignment 0 for control and 1 for treatment.
#' @param yobs the observed outcome.
#' @param verbose TRUE for detailed output FALSE for no output
#' @return A `X_RF` object.
#' @export S_BART
S_BART <-
  function(feat,
           tr,
           yobs,
           ndpost = 1200,
           sample_stat = "counterfactuals estimated",
           tree_package = "dbarts",
           ntree = 200,
           verbose) {
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
#' @rdname EstimateCate-S_BART
#' @description Return the estimated CATE
#' @param object A `S_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @return A vector of predicted CATE
#' @aliases EstimateCate, S_BART-method
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
#' @name CateCI-S_BART
#' @rdname CateCI-S_BART
#' @description Return the estimated CATE
#' @param object A `S_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @return A vector of predicted CATE
#' @aliases CateCI, S_BART-method
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

    return(cbind(pred, CI))
  }
)


#' EstimateAllSampleStatistics-S_BART
#' @name EstimateAllSampleStatistics-S_BART
#' @rdname EstimateAllSampleStatistics-S_BART
#' @description Return the estimated CATE
#' @exportMethod EstimateAllSampleStatistics
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
  } else{
    stop("tree_package must be either BayesTree or dbarts")
  }
  return(pred_matrix)
}

compute_sample_statistics <- function(mu_hat_0_MCMC_samples,
                                      mu_hat_1_MCMC_samples,
                                      ndpost,
                                      feat,
                                      tr,
                                      yobs,
                                      sample_stat) {

  tau_hat_MCMC_samples <-
    mu_hat_1_MCMC_samples - mu_hat_0_MCMC_samples

  SATE_MCMC_samples_alle <- apply(tau_hat_MCMC_samples, 1, mean)
  SATE_estimate_alle <- mean(SATE_MCMC_samples_alle)
  SATE_CI_alle <-
    quantile(SATE_MCMC_samples_alle, probs = c(.05, .95))

  SATT_MCMC_samples_alle <-
    apply((tau_hat_MCMC_samples)[, tr == 1], 1, mean)
  SATT_estimate_alle <- mean(SATT_MCMC_samples_alle)
  SATT_CI_alle <- quantile(SATT_MCMC_samples_alle, probs = c(.05, .95))

  SATC_MCMC_samples_alle <-
    apply((tau_hat_MCMC_samples)[, tr == 0], 1, mean)
  SATC_estimate_alle <- mean(SATC_MCMC_samples_alle)
  SATC_CI_alle <- quantile(SATC_MCMC_samples_alle, probs = c(.05, .95))


  # SATE, ATT, ATC using the observed data.-----------------------------------

  # SATE_MCMC_samples <- apply(tau_hat_MCMC_samples, 1, mean)
  # SATE_estimate <- mean(SATE_MCMC_samples)
  # SATE_CI <- quantile(SATE_MCMC_samples, probs = c(.05, .95))
  #

  yobs_treated_matrix <- matrix(rep(yobs[tr == 1], each = ndpost),
                                nrow = ndpost)

  tau_hat_MCMC_samples_for_treated_cfe <-
    yobs_treated_matrix - mu_hat_0_MCMC_samples[, tr == 1]
  SATT_MCMC_cfe <-
    apply(tau_hat_MCMC_samples_for_treated_cfe,
          1, mean)
  SATT_estimate_cfe <- mean(SATT_MCMC_cfe)
  SATT_CI_cfe <-
    quantile(SATT_MCMC_cfe, probs = c(.05, .95))

  yobs_treated_matrix <- matrix(rep(yobs[tr == 0], each = ndpost),
                                nrow = ndpost)

  tau_hat_MCMC_samples_for_control_cfe <-
    mu_hat_0_MCMC_samples[, tr == 0] - yobs_treated_matrix
  SATC_MCMC_cfe <-
    apply(tau_hat_MCMC_samples_for_control_cfe,
          1, mean)
  SATC_estimate_cfe <- mean(SATC_MCMC_cfe)
  SATC_CI_cfe <-
    quantile(SATC_MCMC_cfe, probs = c(.05, .95))


  # Compute the CATE ---------------------------------------------------------
  ite_matrix <- mu_hat_1_MCMC_samples - mu_hat_0_MCMC_samples

  CATE_pred <- apply(ite_matrix, 2, mean)

  CATE_CI <-
    t(apply(ite_matrix, 2, function(x)
      quantile(x, probs = c(.05, 0.95))))



  # Transform Output for easy access------------------------------------------
  ATE <-
    rbind(
      data.frame(
        method = "all estimated",
        estimate = SATE_estimate_alle,
        "lower" = SATE_CI_alle[1],
        "upper" = SATE_CI_alle[2]
      )
    )

  if(sample_stat == "counterfactuals estimated"){
    ATT <-
      data.frame(
        method = "counterfactuals estimated",
        estimate = SATT_estimate_cfe,
        "lower" = SATT_CI_cfe[1],
        "upper" = SATT_CI_cfe[2]
      )
  }
  if(sample_stat == "all estimated"){
  ATT <-
      data.frame(
        method = "all estimated",
        estimate = SATT_estimate_alle,
        "lower" = SATT_CI_alle[1],
        "upper" = SATT_CI_alle[2]
      )
  }

  ATC <-
    rbind(
      data.frame(
        method = "all estimated",
        estimate = SATC_estimate_alle,
        "lower" = SATC_CI_alle[1],
        "upper" = SATC_CI_alle[2]
      )
    )

  CATE <-
    rbind(
      data.frame(
        method = "all estimated",
        estimate = CATE_pred,
        "lower" = CATE_CI[ ,1],
        "upper" = CATE_CI[ ,2]
      )
    )

  row.names(ATE) <- row.names(ATT) <- row.names(ATC) <- NULL



  return(list("SATE" = ATE,
              "SATT" = ATT,
              "SATC" = ATC,
              "CATE" = CATE))

}





