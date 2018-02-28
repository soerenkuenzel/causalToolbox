#' @include selector_transformed.R
#' This is included to have the get get_CV_sizes.R function

#' gof_matching
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
                         ties = FALSE, 
                         emin = 1e-5) {
  n <- length(tr)
  tr <- as.factor(tr)
  # catch nonsensible specifications
  if (emin <= 0 | emin >= 0.5) {
    stop("0 < emin < 0.5")
  }
  if (sum(as.numeric(tr)) == 0 | sum(as.numeric(tr)) == length(tr)) {
    stop("All units are in the treated group or all units are in the
         treated group")
  }
  if (n == 0 | length(yobs) != n | nrow(feat) != n) {
    stop("Either no data was provided or the sizes of yobs, feat or tr do not
         match")
  }
  if (k < 2 | k %% 1 != 0) {
    stop("k must be an integer bigger than 1!")
  }
  # -------------------------------------------------------------------------
  # Calculate propensity score
  tr_factor <- factor(tr)
  pscore_estimator <- ranger::ranger(tr_factor ~ .,
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
  # data set which has
  m <- Matching::Match(
    Y = yobs,
    Tr = tr,
    X = feat,
    estimand = estimand,
    replace = replace,
    ties = ties
  )
  
  Y_1 <- m$mdata$Y[m$mdata$Tr == 1]
  Y_0 <- m$mdata$Y[m$mdata$Tr == 0]
  if (estimand == 'ATT')
    feat_m <- feat[m$index.treated, ]
  if (estimand == 'ATC')
    feat_m <- feat[m$index.control, ]
  if (estimand == 'ATE')
    stop('not implemented yet')
  
  # Create CV idxes
  cv_idx <- rep(NA, matching_n)
  cv_idx <-
    sample(rep(x = 1:k, times = get_CV_sizes(matching_n, k)))
  
  
  cate_est <- rep(NA, matching_n) # will contain the estimates
  for (i in 1:k) {
    print(paste("Running", i, "out of", k, "CV fold."))
    # get train and test set -- training set is everything but fold i
    train_idx <- cv_idx != i
    test_idx <- !train_idx
    
    # Estimate CATE with the given learner function
    estimator_trained <-
      estimator(feat = matching_feat[train_idx,],
                tr = matching_Tr[train_idx],
                yobs = matching_Y[train_idx])
    cate_est[test_idx] <- EstimateCate(estimator_trained,
                                       matching_feat[test_idx,])
  }
  
  # --------------------------------------------------------------------------
  # Compute the ITE = Y(1) - Y(0)c
  ITE <-
    rep(yobs[m$index.treated] - yobs[m$index.control], times = 2)
  
  # Calcualte the Goodness-of-Fit
  mse <- mean((ITE - cate_est) ^ 2)
  sd_err <- sd((ITE - cate_est) ^ 2) / sqrt(n)
  
  # --------------------------------------------------------------------------
  return(c(mse, sd_err))
  }
