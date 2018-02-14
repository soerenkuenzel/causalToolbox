#' @include TBART.R
#' @include Thrf.R
#' @include XBART.R
#' @include Xhrf.R
#' @include SBART.R
#' @include Shrf.R

# ------------------------------------------------------------------------------
# helper functions
tr <- rbinom(n = 500, size = 1, prob = .1)
k <- 4

get_CV_sizes <- function(n, k) {
  # n: number of units
  # k: number of folds
  # Return: the CV fold sizes eg. (5,5,4,4,4) for n = 22, k = 5
  small_size <- floor(n / k)
  remainder <- n %% k 
  sizes <- rep(small_size, k) + c(rep(1, remainder), rep(0, k - remainder)) 
  return(sizes)
}

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
# ------------------------------------------------------------------------------

setGeneric(
  name = "gof_transformed",
  def = function(feat, yobs, tr, estimator, k = 2, emin = 1e-5) {
    standardGeneric("gof_transformed")
  }
)
#' gof_transformed
#' @name gof_transformed
#' @param feat a data frame of features
#' @param obs a vector of observations
#' @param tr a vector of group assignment (assume entries are integers)
#' @param estimator a learner constructor
#' @param k we are doing a k fold cross validation
#' @param emin the pscore prediciton will be bounded between emin and 1 - emin
#' to aovid decide by 0 error
#' @return mean(error) and sd(error)
#' @import ranger
#' @exportMethod gof_transformed
setMethod(
  "gof_transformed",
  definition = function(feat, yobs, tr, estimator, k = 2, emin = 1e-5) {
    n <- length(tr)
    # catch nonsensible specifications
    if (emin <= 0 | emin >= 0.5) {
      stop("0 < emin < 0.5")
    }
    if (sum(tr) == 0 | sum(tr) == length(tr)) {
      stop("All units are in the treated group or all units are in the
           treated group")
    }
    if (n == 0 | length(yobs) != n | nrow(feat) != n) {
      stop("Either no data was provided or the sizes of yobs, feat or tr do not
           match")
    }
    if (k < 2 | k%%1 != 0){
      stop("k must be an integer bigger than 1!")
    }
    # --------------------------------------------------------------------------
    # Compute the CATE estimates using a k fold CV
    
    # Create CV idxes
    cv_idx <- getCV_indexes(tr = tr, k = k)
    
    cate_est <- rep(NA, n) # will contain the estimates
    for (i in 1:k) {
      print(paste("Running", i, "out of", k, "CV fold."))
      # get train and test set -- training set is everything but fold i
      train_idx <- cv_idx != i
      test_idx <- !train_idx
      
      # Estimate CATE with the given learner function
      estimator_trained <- estimator(feat = feat[train_idx, ],
                                     tr = tr[train_idx],
                                     yobs = yobs[train_idx])
      cate_est[test_idx] <- EstimateCate(estimator_trained, feat[test_idx, ])
    }

    # --------------------------------------------------------------------------
    # Compute the Y star and evaluate the model
    
    # Calculate propensity score
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
    
    # Calculate y_star_te
    y_star <- yobs / (tr * pscore_pred - (1 - tr) * (1 - pscore_pred))

    # Calcualte the Goodness-of-Fit
    mse <- mean((y_star - cate_est) ^ 2)
    sd_err <- sd((y_star - cate_est) ^ 2) / sqrt(n)

    # --------------------------------------------------------------------------
    return(c(mse, sd_err))
  }
)
