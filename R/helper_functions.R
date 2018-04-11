#' @include TBART.R
#' @include TRF.R
#' @include XBART.R
#' @include XRF.R
#' @include SBART.R
#' @include SRF.R
#-------------------------------------------------------------------------------
catch_error <- function(feat, yobs, tr, k) {
  n <- length(tr)
  if (sum(tr) == 0 | sum(tr) == n) {
    stop("All units are in the treated group or all units are in the
         control group")
  }
  if (n == 0 | length(yobs) != n | nrow(feat) != n) {
    stop("Either no data was provided or the sizes of yobs, feat or tr do not
         match")
  }
  if (k < 2 | k%%1!=0){
    stop("k must be an integer bigger than 1!")
  }
}
#-------------------------------------------------------------------------------
get_CV_sizes <- function(n, k) {
  # n: number of units
  # k: number of folds
  # Return: the CV fold sizes eg. (5,5,4,4,4) for n = 22, k = 5
  small_size <- floor(n / k)
  remainder <- n %% k 
  sizes <- rep(small_size, k) + c(rep(1, remainder), rep(0, k - remainder)) 
  return(sizes)
}
#-------------------------------------------------------------------------------
getCV_indexes <- function(tr, k) {
  # Description: this function splits n = length(tr) units into k folds 
  # for a k fold CV and it returns a list with k index sequences
  # tr is the treatment assignment. It makes sure that each CV has the same 
  # proportion of treatment indicators. 
  n <- length(tr)
  n1 <- sum(tr)
  n0 <- n - n1
  
  size1 = get_CV_sizes(n = n1, k = k)
  size0 = get_CV_sizes(n = n0, k = k)
  
  cv_fold_idx <- rep(NA, n)
  cv_fold_idx[tr == 1] <- sample(rep(x = 1:k, times = size1))
  cv_fold_idx[tr == 0] <- sample(rep(x = 1:k, times = size0))
  
  return(cv_fold_idx)
}
#-------------------------------------------------------------------------------
# Compute the CATE estimates using a k fold CV
compute_CATE_estimates <- function(feat, yobs, tr, estimator, k, verbose) {
  n <- length(tr)
  # Create CV idxes
  cv_idx <- getCV_indexes(tr = tr, k = k)
  
  cate_est <- rep(NA, n) # will contain the estimates
  for (i in 1:k) {
    if (verbose) {
      print(paste("Running", i, "out of", k, "CV fold."))
    }
    # get train and test set -- training set is everything but fold i
    train_idx <- cv_idx != i
    test_idx <- !train_idx
    
    # Estimate CATE with the given learner function
    estimator_trained <- estimator(feat = feat[train_idx, ],
                                   tr = tr[train_idx],
                                   yobs = yobs[train_idx])
    cate_est[test_idx] <- EstimateCate(estimator_trained, feat[test_idx, ])
  }
  return(cate_est)
}
#-------------------------------------------------------------------------------
#Estimate the propensity score, E[W|X]
library(ranger)
get_pscore <- function(feat, tr, emin) {
  pscore_estimator <- ranger::ranger(tr ~ ., 
                                     data = data.frame(feat, 
                                                       tr = factor(tr)), 
                                     probability = TRUE)
  pscore_pred_raw <- pscore_estimator$predictions[ ,2]
  pscore_pred <- ifelse(
    pscore_pred_raw < emin,
    emin,
    ifelse(pscore_pred_raw > 1 - emin, 1 - emin,
           pscore_pred_raw)
  )
  return(pscore_pred)
}
#-------------------------------------------------------------------------------
#Estimate the expected outcome, E[Y|X]
get_pred_y <- function(feat, yobs) {
  outcome_estimator <- ranger::ranger(yobs ~ ., 
                                      data = data.frame(feat, 
                                                        yobs))
  outcome_pred <- outcome_estimator$predictions
  return(outcome_pred)
}
