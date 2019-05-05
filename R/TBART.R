#' @include CATE_estimators.R
#' @include SBART.R
#' @import BART



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
    nthread = "numeric",
    creator = "function"
  )
)

#' @title T_BART
#' @rdname T_BART
#' @description This is an implementation of T_BART
#' @param feat A feature data.frame.
#' @param tr A vector of treatment assignment 0 for control and 1 for treatment.
#' @param yobs A vector of the observed outcome.
#' @param verbose TRUE for detailed output FALSE for no output
#' @param ndpost Number of posterior draws
#' @param sample_stat TODO: Add Description
#' @param tree_package Package used to create a tree
#' @param ntree Number of trees to grow
#' @return A `T_BART` object.
#' @export T_BART
#' @import methods
T_BART <-
  function(feat,
           tr,
           yobs,
           ndpost = 1200,
           sample_stat = "counterfactuals estimated",
           tree_package = "BART",
           ntree = 200,
           nthread = 1,
           verbose = FALSE) {
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
      nthread = nthread,
      creator = function(feat, tr, yobs) {
        T_BART(feat,
               tr,
               yobs,
               ndpost = ndpost,
               sample_stat = sample_stat,
               tree_package = tree_package,
               ntree = ntree,
               nthread = nthread)
      }
    )
  }


#' EstimateCate-T_BART
#' @name EstimateCate-T_BART
#' @rdname EstimateCate-T_BART
#' @description Return the estimated CATE
#' @param theObject A `T_BART` object.
#' @param feature_new A feature data frame.
#' @param verbose TRUE for detailed output FALSE for no output
#' @param return_CI If TRUE, return predictions and their confidence intervals;
#' if FALSE, return only predictions.
#' @return A vector of predicted CATE
#' @aliases EstimateCate,T_BART-method
#' @exportMethod EstimateCate
#' @import parallel
setMethod(
  f = "EstimateCate",
  signature = "T_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE,
                        return_CI = FALSE)
  {
    # theObject = xb;  verbose = TRUE; ndpost = 100; return_CI = TRUE;
    # feature_new = feat[1:5,]
    ndpost <- theObject@ndpost
    yobs <- theObject@yobs_train
    if ((is.null(yobs)) | (is.data.frame(yobs) && nrow(yobs) == 0)) {
      stop('Observations cannot be empty or null.')
    }
    feat <- theObject@feature_train
    if ((is.null(feat)) | (is.data.frame(feat) && nrow(feat) == 0)) {
      stop('Features cannot be empty or null.')
    }
    tr <- theObject@tr_train
    if ((is.null(tr)) | (is.data.frame(tr) && nrow(tr) == 0)) {
      stop('Treatment assignments cannot be empty or null.')
    }
    nthread <- theObject@nthread
    if (nthread > parallel::detectCores()) {
      warning('nthread is chosen bigger than the number of cores. It is changed
              to be equal to the number of cores.')
      nthread <- parallel::detectCores()
    }

    yobs_0 <- yobs[tr == 0]
    if (is.data.frame(yobs_0) && nrow(yobs_0) == 0) {
      stop('There is no observation in the control group (labelled 0).')
    }
    X_0 <- feat[tr == 0, ]
    if (is.data.frame(X_0) && nrow(X_0) == 0) {
      stop('There is no feature in the control group (labelled 0).')
    }
    yobs_1 <- yobs[tr == 1]
    if (is.data.frame(yobs_1) && nrow(yobs_1) == 0) {
      stop('There is no observation in the treatment group (labelled 1).')
    }
    X_1 <- feat[tr == 1, ]
    if (is.data.frame(X_1) && nrow(X_1) == 0) {
      stop('There is no feature in the treatment group (labelled 1).')
    }
    
    if (theObject@tree_package == "BART"){
      pred_matrix_f_0 <- BART::mc.wbart(
        x.train = X_0,
        y.train = yobs_0,
        x.test =  feature_new,
        ndpost = ndpost,
        ntree = theObject@ntree,
        mc.cores = nthread
      )$yhat.test
      
      mu_hat_0 <- apply(pred_matrix_f_0, 2, mean)
      
      pred_matrix_f_1 <- BART::mc.wbart(
        x.train = X_1,
        y.train = yobs_1,
        x.test =  feature_new,
        ndpost = ndpost,
        ntree = theObject@ntree,
        mc.cores = nthread
      )$yhat.test
      
      mu_hat_1 <- apply(pred_matrix_f_1, 2, mean)
    } else{
      stop("tree_package must be BART")
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


#' CateCI-T_BART
#' @name CateCI-T_BART
#' @rdname CateCI-T_BART
#' @description Return the estimated CATE
#' @param theObject A `T_BART` object.
#' @inheritParams CateCI
#' @return A vector of predicted CATE
#' @aliases CateCI,T_BART-method
#' @exportMethod CateCI
setMethod(
  f = "CateCI",
  signature = "T_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE)
  {
    ndpost <- theObject@ndpost
    yobs <- theObject@yobs_train
    if ((is.null(yobs)) | (is.data.frame(yobs) && nrow(yobs) == 0)) {
      stop('Observations cannot be empty or null.')
    }
    feat <- theObject@feature_train
    if ((is.null(feat)) | (is.data.frame(feat) && nrow(feat) == 0)) {
      stop('Features cannot be empty or null.')
    }
    tr <- theObject@tr_train
    if ((is.null(tr)) | (is.data.frame(tr) && nrow(tr) == 0)) {
      stop('Treatment assignments cannot be empty or null.')
    }
    nthread <- theObject@nthread
    if (nthread > parallel::detectCores()) {
      warning('nthread is chosen bigger than the number of cores. It is changed
              to be equal to the number of cores.')
      nthread <- parallel::detectCores()
    }
    
    yobs_0 <- yobs[tr == 0]
    if (is.data.frame(yobs_0) && nrow(yobs_0) == 0) {
      stop('There is no observation in the control group (labelled 0).')
    }
    X_0 <- feat[tr == 0,]
    if (is.data.frame(X_0) && nrow(X_0) == 0) {
      stop('There is no feature in the control group (labelled 0).')
    }
    yobs_1 <- yobs[tr == 1]
    if (is.data.frame(yobs_1) && nrow(yobs_1) == 0) {
      stop('There is no observation in the treatment group (labelled 1).')
    }
    X_1 <- feat[tr == 1,]
    if (is.data.frame(X_1) && nrow(X_1) == 0) {
      stop('There is no feature in the treatment group (labelled 1).')
    }                
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
      
      x_to_predict_subset <- split(x_to_predict, rep(x = 1:nthread,
                                                     times = get_CV_sizes(
                                                       nrow(x_to_predict),
                                                      nthread)))

 
      if (theObject@tree_package == "BART"){
        pred_matrix <- BART::mc.wbart(
            x.train = x,
            y.train = y,
            x.test =  x_to_predict,
            verbose = verbose,
            ndpost = ndpost,
            ntree = theObject@ntree,
            mc.cores = nthread
          )$yhat.test
      } else {
        stop("tree_package must be BART")
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
    
    to_return <- as.data.frame(cbind(pred, CI_comb))
    row.names(to_return) <- 1:nrow(to_return)
    colnames(to_return) <- c('pred','X5.','X95.')
    return(to_return)
  }
)


#' EstimateAllSampleStatistics-T_BART
#' @name EstimateAllSampleStatistics-T_BART
#' @rdname EstimateAllSampleStatistics-T_BART
#' @description Return the estimated CATE
#' @param theObject A "T_BART" object
#' @param verbose TRUE for detailed output FALSE for no output
#' @aliases EstimateAllSampleStatistics,T_BART-method
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
    nthread <- theObject@nthread
    feat_subset <- split(feat, rep(x = 1:nthread,
                                   times = get_CV_sizes(nrow(feat), nthread)))
    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0, ]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1, ]
    

    if (theObject@tree_package == "BART"){
      mu_hat_0_MCMC_samples <- BART::mc.wbart(
          x.train = X_0,
          y.train = yobs_0,
          x.test =  feat,
          verbose = verbose,
          ndpost = ndpost,
          ntree = theObject@ntree,
          mc.cores = nthread
        )$yhat.test
    
      mu_hat_1_MCMC_samples <- BART::mc.wbart(
          x.train = X_1,
          y.train = yobs_1,
          x.test =  feat,
          verbose = verbose,
          ndpost = ndpost,
          ntree = theObject@ntree,
          mc.cores = nthread
        )$yhat.test
      
    } else{
      stop("tree_package must be BART")
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

