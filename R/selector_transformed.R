#' @include TBART.R
#' @include Thrf.R
#' @include XBART.R
#' @include Xhrf.R
#' @include SBART.R
#' @include Shrf.R

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
#' @import ranger
#' @exportMethod GoF_yStar
setMethod(
  "GoF_yStar",
  definition = function(feat, yobs, tr, estimator, seed) {
    set.seed(seed)
    # Split the given data frame into train/test data set. 
    sample_idx <- sample.int(n = nrow(feat), size = floor(0.8 * nrow(feat)), 
                             replace = FALSE)
    feat_train <- feat[sample_idx, ]
    tr_train <- tr[sample_idx]
    yobs_train <- yobs[sample_idx]
    
    feat_test <- feat[-sample_idx, ]
    tr_test <- tr[-sample_idx]
    yobs_test <- yobs[-sample_idx]
    
    # Calculate propensity score by using ranger
    temp_df <- data.frame(feat_train, tr_train)
    # Convert integer labels to factors (required in ranger function?)
    temp_df$tr_train <- as.factor(temp_df$tr_train)
    rf <- ranger(tr_train ~ ., data = temp_df, probability = TRUE)
    propensity_score <- predict(rf, feat_test)$predictions[ ,2]
    
    # Calculate y_star_te 
    y_star_te <- yobs_test / (tr_test * propensity_score - 
                                (1 - tr_test) * (1 - propensity_score))

 
    # Estimate CATE with the given learner function
    est <- estimator(feat = feat_test, tr = tr_test, yobs = yobs_test)
    cate <- EstimateCate(est, feat_test)
    
    # Calcualte the Goodness-of-Fit 
    gof <- - sum((y_star_te - cate)^2) / nrow(feat_test)
    mse <- mean((y_star_te - cate)^2)
    sd_err <- sd((y_star_te - cate)^2) / sqrt(nrow(feat_test))
    
    return(c(mse, sd_err))
  }
)
# Example
# set.seed(123425)

# feat <- iris[, -1]
# tr <- rbinom(nrow(feat), 1, .5)
# yobs <- iris[, 1]
# GoF_yStar(feat, yobs, tr, S_BART, 123425)
# GoF_yStar(feat, yobs, tr, T_BART, 123425)
# GoF_yStar(feat, yobs, tr, X_BART, 123425)
# GoF_yStar(feat, yobs, tr, T_RF, 123425)
# GoF_yStar(feat, yobs, tr, S_RF, 123425)

# Error
# GoF_yStar(feat, yobs, tr, X_RF, 123425)