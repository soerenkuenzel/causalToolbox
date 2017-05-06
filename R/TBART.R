#' @include CATE_estimators.R
#' @include SBART.R

#' @import dbarts



## the standard Xlearner object with random forest
setClass(
  "T_BART",
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

#' @title T_BART
#' @rdname T_BART
#' @description This is an implementation of T_BART
#' @param feat feature data.frame.
#' @param tr treatment assignment 0 for control and 1 for treatment.
#' @param yobs the observed outcome.
#' @param verbose TRUE for detailed output FALSE for no output
#' @return A `X_RF` object.
#' @export T_BART
T_BART <-
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
      "T_BART",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      ndpost = ndpost,
      sample_stat = sample_stat,
      tree_package = tree_package,
      ntree = ntree,
      creator = function(feat, tr, yobs) {
        T_BART(feat,
               tr,
               yobs,
               ndpost = ndpost,
               sample_stat = sample_stat,
               tree_package = tree_package,
               ntree = ntree)
      }
    )
  }


#' EstimateCate-T_BART
#' @name EstimateCate-T_BART
#' @rdname EstimateCate-T_BART
#' @description Return the estimated CATE
#' @param object A `T_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @return A vector of predicted CATE
#' @aliases EstimateCate, T_BART-method
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "T_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE,
                        return_CI = FALSE)
  {
    # theObject = xb;  verbose = TRUE; ndpost = 100; return_CI = TRUE; feature_new = feat[1:5,]
    ndpost <- theObject@ndpost
    yobs <- theObject@yobs_train
    feat <- theObject@feature_train
    tr <- theObject@tr_train

    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0, ]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1, ]


    if (theObject@tree_package == "BayesTree") {
      pred_matrix_f_0 <- BayesTree::bart(
        x.train = X_0,
        y.train = yobs_0,
        x.test =  feature_new,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_0 <- apply(pred_matrix_f_0, 2, mean)

      pred_matrix_f_1 <- BayesTree::bart(
        x.train = X_1,
        y.train = yobs_1,
        x.test =  feature_new,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_1 <- apply(pred_matrix_f_1, 2, mean)
    } else if (theObject@tree_package == "dbarts") {
      pred_matrix_f_0 <- dbarts::bart(
        x.train = X_0,
        y.train = yobs_0,
        x.test =  feature_new,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_0 <- apply(pred_matrix_f_0, 2, mean)

      pred_matrix_f_1 <- dbarts::bart(
        x.train = X_1,
        y.train = yobs_1,
        x.test =  feature_new,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_1 <- apply(pred_matrix_f_1, 2, mean)
    } else{
      stop("tree_package must be either BayesTree or dbarts")
    }

    ############################################################################
    predictions <- mu_hat_1 - mu_hat_0

    if (return_CI) {
      get_CI_0 <- t(apply(pred_matrix_f_0, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))
      get_CI_1 <- t(apply(pred_matrix_f_1, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))

      CI_comb <- get_CI_1 - get_CI_0[, 2:1]

      return(cbind(predictions, CI_comb))
    } else{
      return(predictions)
    }
  }
)


#' EstimateCate-T_BART
#' @name EstimateCate-T_BART
#' @rdname EstimateCate-T_BART
#' @description Return the estimated CATE
#' @param object A `T_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @return A vector of predicted CATE
#' @aliases EstimateCate, T_BART-method
#' @exportMethod EstimateCate
setMethod(
  f = "CateCI",
  signature = "T_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    yobs <- theObject@yobs_train
    feat <- theObject@feature_train
    tr <- theObject@tr_train

    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0,]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1,]

    # will contain for each estimator the prediciton for x_to_predict:
    output <- list()
    # will contain the appropraite CI:
    CI <- list()
    for (this_learner in c("l_first_0",
                           "l_first_1")) {
      if (this_learner == "l_first_0") {
        x = X_0
        y = yobs_0
        x_to_predict = feature_new
      } else if (this_learner == "l_first_1") {
        x = X_1
        y = yobs_1
        x_to_predict = feature_new
      }


      if (theObject@tree_package == "BayesTree") {

        pred_matrix <- BayesTree::bart(
          x.train = x,
          y.train = y,
          x.test =  x_to_predict,
          verbose = verbose,
          ndpost = ndpost,
          ntree = theObject@ntree
        )$yhat.test

      } else if (theObject@tree_package == "dbarts") {

        pred_matrix <- dbarts::bart(
          x.train = x,
          y.train = y,
          x.test =  x_to_predict,
          verbose = verbose,
          ndpost = ndpost,
          ntree = theObject@ntree
        )$yhat.test

      } else{
        stop("tree_package must be either BayesTree or dbarts")
      }

      output[[this_learner]] <- apply(pred_matrix, 2, mean)
      CI[[this_learner]] <-
        t(apply(pred_matrix, 2, function(x)
          quantile(x, probs = c(.05, 0.95))))

    }
    if (verbose) {
      print("Done with the propensity score estimation.")
    }
    pred <- output[["l_first_1"]] - output[["l_first_0"]]

    CI_comb <- CI[["l_first_1"]] - CI[["l_first_0"]][, 2:1]

    return(cbind(pred, CI_comb))
  }
)


#' EstimateAllSampleStatistics-T_BART
#' @name EstimateAllSampleStatistics-T_BART
#' @rdname EstimateAllSampleStatistics-T_BART
#' @description Return the estimated CATE
#' @exportMethod EstimateAllSampleStatistics
setMethod(
  f = "EstimateAllSampleStatistics",
  signature = "T_BART",
  definition = function(theObject, verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    yobs <- theObject@yobs_train
    sample_stat <- theObject@sample_stat


    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0, ]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1, ]


    if (theObject@tree_package == "BayesTree") {

      mu_hat_0_MCMC_samples <- BayesTree::bart(
        x.train = X_0,
        y.train = yobs_0,
        x.test =  feat,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_1_MCMC_samples <- BayesTree::bart(
        x.train = X_1,
        y.train = yobs_1,
        x.test =  feat,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

    } else if (theObject@tree_package == "dbarts") {

      mu_hat_0_MCMC_samples <- dbarts::bart(
        x.train = X_0,
        y.train = yobs_0,
        x.test =  feat,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

      mu_hat_1_MCMC_samples <- dbarts::bart(
        x.train = X_1,
        y.train = yobs_1,
        x.test =  feat,
        verbose = verbose,
        ndpost = ndpost,
        ntree = theObject@ntree
      )$yhat.test

    } else{
      stop("tree_package must be either BayesTree or dbarts")
    }

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


