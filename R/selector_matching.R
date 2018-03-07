#' @include selector_transformed.R
#' This is included to have the get get_CV_sizes.R function

#' gof_matching
#' @title gof_matching
#' @name gof_matching
#' @param feat a data frame of features
#' @param yobs a vector of observations
#' @param tr a vector of group assignment (assume entries are integers)
#' @param estimator a learner constructor
#' @param estimand ATE, ATT or ATC
#' @param k we are doing a k fold cross validation
#' @return mean(error) and sd(error)
#' @import Matching
#' @import ranger
gof_matching <- function(feat,
                         yobs,
                         tr,
                         estimator,
                         estimand = 'ATT',
                         k = 2,
                         replace = TRUE,
                         emin = 1e-5) {

  # catch nonsensible specifications
  if (emin <= 0 | emin >= 0.5) {
    stop("0 < emin < 0.5")
  }
  catch_error(feat, yobs, tr, k)
  # -------------------------------------------------------------------------
  # Estimate propensity score
  pscore_estimator <- ranger::ranger(tr ~ .,
                                     data = data.frame(feat, tr = factor(tr)),
                                     probability = TRUE)
  pscore_pred_raw <- pscore_estimator$predictions[, 2]
  pscore_pred <- ifelse(
    pscore_pred_raw < emin,
    emin,
    ifelse(pscore_pred_raw > 1 - emin, 1 - emin,
           pscore_pred_raw)
  )
  # --------------------------------------------------------------------------
  # Create the matched data set. We match on the features and we create a
  # data set which has features|Y(0)|Y(1)|pscore
  m <- Matching::Match(
    Y = yobs,
    Tr = tr,
    X = feat,
    estimand = estimand,
    replace = replace,
    ties = FALSE
  )
  
  Y1 <- m$mdata$Y[m$mdata$Tr == 1]
  Y0 <- m$mdata$Y[m$mdata$Tr == 0]
  pscore_m <-
    (pscore_pred[m$index.treated] + pscore_pred[m$index.control]) / 2
  if (estimand == 'ATT') {
    feat_m <- feat[m$index.treated, ]
  }
  if (estimand == 'ATC') {
    feat_m <- feat[m$index.control, ]
  }
  if (estimand == 'ATE') {
    feat_m <- feat[m$index.control, ]
  }
  
  # ----------------------------------------------------------------------------
  # CV compute CATEs
  # Situation:
  # feat_m : contains the covariates, Y1, Y0 :potential outcomes, pscore_m : ps
  n_matched <- length(Y1)
  
  # Create CV idxes
  cv_idx <- sample(rep(x = 1:k, times = get_CV_sizes(n_matched, k)))
  
  cate_est <- rep(NA, n_matched) # will contain the estimates
  for (b in 1:k) {
    print(paste("Running", b, "out of", k, "CV fold."))
    # get train and test set -- training set is everything but fold i
    train_idx <- which(cv_idx != b)
    test_idx <- which(cv_idx == b)
    
    # construct the treatment column
    feat_b <- feat_m[train_idx, ]
    tr_b <- rbinom(length(train_idx), 1, pscore_m[train_idx])
    yobs_b <- ifelse(tr_b == 1, Y1[train_idx], Y0[train_idx])
    
    if (sum(tr_b) == 0 | sum(tr_b) == length(tr_b)) {
      stop("In the CV all units were in the treated group or all units are in
           the control group")
      # TODO: We might want to do conditional sampling here such that the 
      # proportion of treated units is equal to the poroportion of treated units
      # in the original data set.
    }
    
    # Estimate CATE with the given learner function
    estimator_trained <- estimator(feat = feat_b,
                                   tr = tr_b,
                                   yobs = yobs_b)
    cate_est[test_idx] <- EstimateCate(estimator_trained, feat_m[test_idx, ])
  }
  
  # --------------------------------------------------------------------------
  # Compute the ITE = Y(1) - Y(0)c
  ITE <- Y1 - Y0
  
  # Calcualte the Goodness-of-Fit
  mse <- mean((ITE - cate_est) ^ 2)
  sd_err <- sd((ITE - cate_est) ^ 2) / sqrt(n_matched)
  
  # --------------------------------------------------------------------------
  return(c(mse, sd_err))
}
