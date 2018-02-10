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

get_CV_sizes <- function(n, k){
  # n number of units
  # k number of folds
  # returns the CV fold sizes eg. (5,5,4,4,4) for n = 22, k = 5
  small_size <- floor(n / k)
  remainder <- n %% k 
  sizes <- rep(small_size, k) + c(rep(1, remainder), rep(0, k - remainder)) 
  return(sizes)
}

getCV_indexes <- function(tr, k = 2) {
  # this function splits n = length(tr) units into k folds for a k fold CV and it returns
  # a list with k index sequences
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
  name = "GoF_yStar",
  def = function(feat, yobs, tr, estimator, seed) {
    standardGeneric("GoF_yStar")
  }
)
#' GoF_yStar
#' @name GoF_yStar
#' @param feat a data frame of features
#' @param obs a vector of observations
#' @param tr a vector of group assignment (assume entries are integers)
#' @param estimator a learner constructor
#' @param k we are doing a k fold cross validation
#' @import ranger
#' @exportMethod GoF_yStar
setMethod(
  "GoF_yStar",
  definition = function(feat, yobs, tr, estimatorm, k) {
    # Split the given data frame into train/test data set.
    sample_idx <- sample.int(n = nrow(feat),
                             size = floor(0.8 * nrow(feat)),
                             replace = FALSE)
    feat_train <- feat[sample_idx, ]
    tr_train <- tr[sample_idx]
    yobs_train <- yobs[sample_idx]
    
    feat_test <- feat[-sample_idx, ]
    tr_test <- tr[-sample_idx]
    yobs_test <- yobs[-sample_idx]
    
    # Calculate propensity score by using ranger
    pscoreDS <- data.frame(feat_test, tr_test)
    # Convert integer labels to factors (required in ranger function?)
    pscoreDS$feat_test <- as.factor(pscoreDS$feat_test)
    rf <- ranger::ranger(feat_test ~ ., data = pscoreDS, probability = TRUE)
    propensity_score <- rf$predictions
    
    # Calculate y_star_te
    y_star_te <- yobs_test / 
      (tr_test * propensity_score - (1 - tr_test) * (1 - propensity_score))
    
    
    # Estimate CATE with the given learner function
    estimator_trained <- estimator(feat = feat_train, 
                                   tr = tr_train, 
                                   yobs = yobs_train)
    cate <- EstimateCate(estimator_trained, feat_test)
    
    # Calcualte the Goodness-of-Fit
    mse <- mean((y_star_te - cate) ^ 2)
    sd_err <- sd((y_star_te - cate) ^ 2) / sqrt(nrow(feat_test))
    
    return(c(mse, sd_err))
  }
)
