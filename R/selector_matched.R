#' @include TBART.R
#' @include Thrf.R
#' @include XBART.R
#' @include Xhrf.R
#' @include SBART.R
#' @include Shrf.R
 

get_CV_sizes <- function(n, k) {
  # n: number of units
  # k: number of folds
  # Return: the CV fold sizes eg. (5,5,4,4,4) for n = 22, k = 5
  small_size <- floor(n / k)
  remainder <- n %% k 
  sizes <- rep(small_size, k) + c(rep(1, remainder), rep(0, k - remainder)) 
  return(sizes)
}

#' gof_matched
#' @name gof_matched
#' @param feat a data frame of features
#' @param yobs a vector of observations
#' @param tr a vector of group assignment (assume entries are integers)
#' @param estimator a learner constructor
#' @param estimand ATE, ATT or ATC
#' @param k we are doing a k fold cross validation
#' @param emin the pscore prediciton will be bounded between emin and 1 - emin
#' to avoid decide by 0 error
#' @return mean(error) and sd(error)
#' @import Matching
#' @import ranger
gof_matched <- function(feat, yobs, tr, estimator, estimand, k = 2, 
                            emin = 1e-5) {
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
  if (k < 2 | k%%1!=0){
    stop("k must be an integer bigger than 1!")
  }
  # --------------------------------------------------------------------------
  
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
  
  # Match with replacement
  # Covariates? M? Ties?
  
  # If ATT, match to each treated unit a control unit.
  if (estimand == "ATT") {
    m <- Match(Y = yobs, Tr = tr, X = pscore_pred, 
               estimand = "ATT", replace = TRUE, M = 1, ties = FALSE)
    
  } 
  # If ATC, match to each control unit to a treated unit.
  if (estimand == "ATC") {
    m <- Match(Y = yobs, Tr = tr, X = pscore_pred, 
               estimand = "ATC", replace = TRUE, M = 1, ties = FALSE)

  } 
  # If ATE, match to each treated unit a control unit and to each contrl unit 
  # a treated unit
  if (estimand == "ATE") {
    m <- Match(Y = yobs, Tr = tr, X = pscore_pred, 
               estimand = "ATE", replace = TRUE, M = 1, ties = FALSE)
  }
  matched_Y <- m$mdata$Y
  matched_Tr <- m$mdata$Tr
  matched_feat <- feat[c(m$index.treated, m$index.control), ]
  matched_n = length(matched_Y)

  # Create CV idxes
  cv_idx <- rep(NA, matched_n)
  cv_idx <- sample(rep(x = 1:k, times = get_CV_sizes(matched_n, k)))
    
  
  cate_est <- rep(NA, matched_n) # will contain the estimates
  for (i in 1:k) {
    print(paste("Running", i, "out of", k, "CV fold."))
    # get train and test set -- training set is everything but fold i
    train_idx <- cv_idx != i
    test_idx <- !train_idx

    # Estimate CATE with the given learner function
    estimator_trained <- estimator(feat = matched_feat[train_idx, ],
                                   tr = matched_Tr[train_idx],
                                   yobs = matched_Y[train_idx])
    cate_est[test_idx] <- EstimateCate(estimator_trained, 
                                       matched_feat[test_idx, ])
  }
  
  # --------------------------------------------------------------------------
  # Compute the ITE = Y(1) - Y(0)c
  ITE <- rep(yobs[m$index.treated] - yobs[m$index.control], times = 2)
  
  # Calcualte the Goodness-of-Fit
  mse <- mean((ITE - cate_est) ^ 2)
  sd_err <- sd((ITE - cate_est) ^ 2) / sqrt(n)
  
  # --------------------------------------------------------------------------
  return(c(mse, sd_err))
}
