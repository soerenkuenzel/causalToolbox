#' @include CATE_estimators.R
#' @import dbarts

## the standard Xlearner object with random forest
setClass(
  "X_BART",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    ensemble = "character",
    ndpost = "numeric",
    creator = "function"
  )
)

#' @title X_BART
#' @rdname X_BART
#' @description This is an implementation of X_BART
#' @param feat feature data.frame.
#' @param tr treatment assignment 0 for control and 1 for treatment.
#' @param yobs the observed outcome.
#' @param verbose TRUE for detailed output FALSE for no output
#' @return A `X_RF` object.
#' @export X_BART
X_BART <-
  function(feat,
           tr,
           yobs,
           ensemble = "pscore",
           ndpost = 1200) {
    feat <- as.data.frame(feat)

    new(
      "X_BART",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      ensemble = ensemble,
      ndpost = ndpost,
      creator = function(feat, tr, yobs) {
        X_BART(feat,
               tr,
               yobs)
      }
    )
  }


#' EstimateCate-X_BART
#' @name EstimateCate-X_BART
#' @rdname EstimateCate-X_BART
#' @description Return the estimated CATE
#' @param object A `X_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @param ensemble Specify how tau_hat_1 and tau_hat_2 should be averaged the
#' dafault is by the weight of the propensity score, other version is
#' "variance" which uses the variance of the weighting
#' @return A vector of predicted CATE
#' @aliases EstimateCate, X_BART-method
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "X_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE,
                        return_CI = FALSE)
  {
    # theObject = xb;  verbose = TRUE; ndpost = 100; return_CI = TRUE; feature_new = feat[1:5,]; ensemble = "pscore"
    yobs <- theObject@yobs_train
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    ndpost <- theObject@ndpost
    ensemble <- theObject@ensemble

    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0, ]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1, ]

    ############################################################################
    # First stage ##############################################################
    ############################################################################

    n_1 <- sum(tr)
    n_0 <- sum(1 - tr)

    if(return_CI){
      # if CI should be returned, we also need to estimate the uncertainty, we
      # had in the first stage.
      f_0_test_set <- rbind(X_1, feature_new)
      f_1_test_set <- rbind(X_0, feature_new)
    } else{
      f_0_test_set <- X_1
      f_1_test_set <- X_0
    }


    pred_matrix_f_0 <- dbarts::bart(
      x.train = X_0,
      y.train = yobs_0,
      x.test =  f_0_test_set,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    mu_hat_1 <- apply(pred_matrix_f_0[ ,1:n_1], 2, mean)

    pred_matrix_f_1 <- dbarts::bart(
      x.train = X_1,
      y.train = yobs_1,
      x.test =  f_1_test_set,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    mu_hat_0 <- apply(pred_matrix_f_1[ ,1:n_0], 2, mean)

    if (verbose)
      print("Done with the first stage.")

    ############################################################################
    # second stage #############################################################
    ############################################################################
    D_1 <- yobs_1 - mu_hat_1
    D_0 <- mu_hat_0 - yobs_0

    pred_matrix_s_1 <- dbarts::bart(
      x.train = X_1,
      y.train = D_1,
      x.test =  feature_new,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    tau_hat_1 <- apply(pred_matrix_s_1, 2, mean)

    pred_matrix_s_0 <- dbarts::bart(
      x.train = X_0,
      y.train = D_0,
      x.test =  feature_new,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    tau_hat_0 <- apply(pred_matrix_s_0, 2, mean)

    if (verbose)
      print("Done with the second stage.")
    ############################################################################
    ### Combining the two ######################################################
    ############################################################################

    if(ensemble == "pscore"){
      prop_matrix <- dbarts::bart(
        x.train = feat,
        y.train = factor(tr),
        x.test =  feature_new,
        verbose = verbose,
        ndpost = ndpost
      )$yhat.test

      g_weights <- pnorm(apply(prop_matrix, 2, mean))
      if (verbose)
        print("Done with the propensity score estimation.")
    }else if(ensemble == "1/2"){
      g_weights <- 1/2
    }else if(ensemble == "constant p-score"){
      g_weights <- sum(tr) / length(tr)
    }else if(ensemble == "only control"){
      g_weights <- 0
    }else if(ensemble == "only treated"){
      g_weights <- 1
    }else if(ensemble == "variance"){
      var_s_0 <- apply(pred_matrix_s_0, 2, var) / ndpost
      var_s_1 <- apply(pred_matrix_s_1, 2, var) / ndpost
      g_weights <- var_s_1 / (var_s_1 + var_s_0)
    }
    ############################################################################
    ### Combining the two ######################################################
    ############################################################################

    pred <- g_weights * tau_hat_0 +
      (1 - g_weights) * tau_hat_1


    if (return_CI) {
      # Variance from the first stage:

      #TODO : This is a very concervatice way of getting CI, one could directly
      # use the MCMC samples and combine them or look at the convoultion of the
      # empericals.

      n_new <- nrow(feature_new)

      get_CI_mu0 <- t(apply(pred_matrix_f_0[ ,(n_1 + 1):(n_1 + n_new)], 2,
                            function(x) quantile(x, probs = c(.05, 0.95))))
      get_CI_mu1 <- t(apply(pred_matrix_f_1[ ,(n_0 + 1):(n_0 + n_new)], 2,
                            function(x) quantile(x, probs = c(.05, 0.95))))

      mu0_hat_feature_new <- apply(pred_matrix_f_0[ ,(n_1 + 1):(n_1 + n_new)],
                                   2, mean)
      mu1_hat_feature_new <- apply(pred_matrix_f_1[ ,(n_0 + 1):(n_0 + n_new)],
                                   2, mean)


      # Variance from the second stage:
      get_CI_0 <- t(apply(pred_matrix_s_0, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))
      get_CI_1 <- t(apply(pred_matrix_s_1, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))

      CI_comb <-
        g_weights *       (get_CI_0 - get_CI_mu1[ ,2:1] + mu1_hat_feature_new) +
        (1 - g_weights) * (get_CI_1 - get_CI_mu0[ ,2:1] + mu0_hat_feature_new)

      return(cbind(pred, CI_comb))
    } else{
      return(pred)
    }
  }
)


#' EstimateCate-X_BART
#' @name EstimateCate-X_BART
#' @rdname EstimateCate-X_BART
#' @description Return the estimated CATE
#' @param object A `X_BART` object.
#' @param feature_new A data frame.
#' @param verbose Should the training output be posted?
#' @return A vector of predicted CATE
#' @aliases EstimateCate, X_BART-method
#' @exportMethod EstimateCate
setMethod(
  f = "CateCI",
  signature = "X_BART",
  definition = function(theObject,
                        feature_new,
                        verbose = FALSE)
  {
    return(
      EstimateCate(
        theObject,
        feature_new,
        verbose = verbose,
        return_CI = TRUE
      )
    )
  }
)


#' EstimateAllSampleStatistics-X_BART
#' @name EstimateAllSampleStatistics-X_BART
#' @rdname EstimateAllSampleStatistics-X_BART
#' @description Return the estimated CATE
#' @exportMethod EstimateAllSampleStatistics
setMethod(
  f = "EstimateAllSampleStatistics",
  signature = "X_BART",
  definition = function(theObject, verbose = FALSE)
  {
    # theObject = xb;  verbose = TRUE; library(dbarts)
    yobs <- theObject@yobs_train
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    ndpost <- theObject@ndpost
    ensemble <- theObject@ensemble

    yobs_0 <- yobs[tr == 0]
    X_0 <- feat[tr == 0, ]
    yobs_1 <- yobs[tr == 1]
    X_1 <- feat[tr == 1, ]

    ############################################################################
    # First stage ##############################################################
    ############################################################################

    n_1 <- sum(tr)
    n_0 <- sum(1 - tr)

    # if CI should be returned, we also need to estimate the uncertainty, we
    # had in the first stage.
    f_0_test_set <- rbind(X_1, feat)
    f_1_test_set <- rbind(X_0, feat)


    pred_matrix_f_0 <- dbarts::bart(
      x.train = X_0,
      y.train = yobs_0,
      x.test =  f_0_test_set,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    mu_hat_1 <- apply(pred_matrix_f_0[ ,1:n_1], 2, mean)

    pred_matrix_f_1 <- dbarts::bart(
      x.train = X_1,
      y.train = yobs_1,
      x.test =  f_1_test_set,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    mu_hat_0 <- apply(pred_matrix_f_1[ ,1:n_0], 2, mean)

    if (verbose)
      print("Done with the first stage.")

    ############################################################################
    # second stage #############################################################
    ############################################################################
    D_1 <- yobs_1 - mu_hat_1
    D_0 <- mu_hat_0 - yobs_0

    pred_matrix_s_1 <- dbarts::bart(
      x.train = X_1,
      y.train = D_1,
      x.test =  feat,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    tau_hat_1 <- apply(pred_matrix_s_1, 2, mean)

    pred_matrix_s_0 <- dbarts::bart(
      x.train = X_0,
      y.train = D_0,
      x.test =  feat,
      verbose = verbose,
      ndpost = ndpost
    )$yhat.test

    tau_hat_0 <- apply(pred_matrix_s_0, 2, mean)

    if (verbose)
      print("Done with the second stage.")
    ############################################################################
    ### Combining the two ######################################################
    ############################################################################

    if(ensemble == "pscore"){
      prop_matrix <- dbarts::bart(
        x.train = feat,
        y.train = factor(tr),
        x.test =  feat,
        verbose = verbose,
        ndpost = ndpost
      )$yhat.test

      g_weights <- pnorm(apply(prop_matrix, 2, mean))
      if (verbose)
        print("Done with the propensity score estimation.")
    }else if(ensemble == "1/2"){
      g_weights <- 1/2
    }else if(ensemble == "constant p-score"){
      g_weights <- sum(tr) / length(tr)
    }else if(ensemble == "only control"){
      g_weights <- 0
    }else if(ensemble == "only treated"){
      g_weights <- 1
    }else if(ensemble == "variance"){
      var_s_0 <- apply(pred_matrix_s_0, 2, var) / ndpost
      var_s_1 <- apply(pred_matrix_s_1, 2, var) / ndpost
      g_weights <- var_s_1 / (var_s_1 + var_s_0)
    }



    # Compute CATE =============================================================

    pred <- g_weights * tau_hat_0 +
      (1 - g_weights) * tau_hat_1



      # Variance from the first stage:

      #TODO : This is a very concervatice way of getting CI, one could directly
      # use the MCMC samples and combine them or look at the convoultion of the
      # empericals.

      n_new <- nrow(feat)

      get_CI_mu0 <- t(apply(pred_matrix_f_0[ ,(n_1 + 1):(n_1 + n_new)], 2,
                            function(x) quantile(x, probs = c(.05, 0.95))))
      get_CI_mu1 <- t(apply(pred_matrix_f_1[ ,(n_0 + 1):(n_0 + n_new)], 2,
                            function(x) quantile(x, probs = c(.05, 0.95))))

      mu0_hat_feat <- apply(pred_matrix_f_0[ ,(n_1 + 1):(n_1 + n_new)],
                                   2, mean)
      mu1_hat_feat <- apply(pred_matrix_f_1[ ,(n_0 + 1):(n_0 + n_new)],
                                   2, mean)


      # Variance from the second stage:
      get_CI_0 <- t(apply(pred_matrix_s_0, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))
      get_CI_1 <- t(apply(pred_matrix_s_1, 2, function(x)
        quantile(x, probs = c(.05, 0.95))))

      CI_comb <-
        g_weights *       (get_CI_0 - get_CI_mu1[ ,2:1] + mu1_hat_feat) +
        (1 - g_weights) * (get_CI_1 - get_CI_mu0[ ,2:1] + mu0_hat_feat)

      CATE <-
        rbind(
          data.frame(
            method = "all estimated",
            estimate = pred,
            "lower" = CI_comb[ ,1],
            "upper" = CI_comb[ ,2]
          )
        )

      # COMPUTE ATT etc=========================================================


      pred_MCMC_matrix <- g_weights * pred_matrix_s_0 + (1 - g_weights) * pred_matrix_s_1


      SATE_MCMC_samples_alle <- apply(pred_MCMC_matrix, 1, mean)
      SATE_estimate_alle <- mean(SATE_MCMC_samples_alle)
      SATE_CI_alle <-
        quantile(SATE_MCMC_samples_alle, probs = c(.05, .95))

      SATT_MCMC_samples_alle <-
        apply(pred_MCMC_matrix[, tr == 1], 1, mean)
      SATT_estimate_alle <- mean(SATT_MCMC_samples_alle)
      SATT_CI_alle <- quantile(SATT_MCMC_samples_alle, probs = c(.05, .95))

      SATC_MCMC_samples_alle <-
        apply(pred_MCMC_matrix[, tr == 0], 1, mean)
      SATC_estimate_alle <- mean(SATC_MCMC_samples_alle)
      SATC_CI_alle <- quantile(SATC_MCMC_samples_alle, probs = c(.05, .95))

      ATE <-
        rbind(
          data.frame(
            method = "all estimated",
            estimate = SATE_estimate_alle,
            "lower" = SATE_CI_alle[1],
            "upper" = SATE_CI_alle[2]
          )
        )
      ATT <-
        rbind(
          data.frame(
            method = "all estimated",
            estimate = SATT_estimate_alle,
            "lower" = SATT_CI_alle[1],
            "upper" = SATT_CI_alle[2]
          )
        )
      ATC <-
        rbind(
          data.frame(
            method = "all estimated",
            estimate = SATC_estimate_alle,
            "lower" = SATC_CI_alle[1],
            "upper" = SATC_CI_alle[2]
          )
        )

      row.names(ATE) <- row.names(ATT) <- row.names(ATC) <- NULL

      return(list(
        "SATE" = ATE,
        "SATT" = ATT,
        "SATC" = ATC,
        "CATE" = CATE
      ))
  }
)
