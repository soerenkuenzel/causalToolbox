#' @include selector_transformed.R

# ------------------------------------------------------------------------------

#' gof_double_robust
#' @name gof_double_robust
#' @param feat A data frame of features
#' @param yobs A vector of observations
#' @param tr A vector of group assignment (assume entries are integers)
#' @param estimator A learner constructor
#' @param k Number of folds used for cross-validation
#' @param emin the pscore prediciton will be bounded between emin and 1 - emin
#' to avoid decide by 0 error
#' @param verbose determines whether detailed updates will be printed
#' @return mean(error) and sd(error)
#' @import ranger
#' @export gof_double_robust
gof_double_robust <- function(feat,
                              yobs,
                              tr,
                              estimator,
                              k = 2,
                              emin = 1e-5,
                              verbose = FALSE) {
  
  # catch nonsensible specifications
  if (emin <= 0 | emin >= 0.5) {
    stop("0 < emin < 0.5")
  }
  catch_error(feat, yobs, tr, k)

  # ----------------------------------------------------------------------------
  # Estimate propensity score
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

  # ----------------------------------------------------------------------------
  # Estimate the expected outcome
  outcome_estimator <- ranger::ranger(yobs ~ ., 
                                      data = data.frame(feat, 
                                                        tr = factor(tr)))
  outcome_pred <- outcome_estimator$predictions

  # ----------------------------------------------------------------------------
  # Compute the CATE estimates using k-fold CV (function in selector_transformed)
  cate_est <- compute_CATE_estimates(feat, yobs, tr, estimator, k, verbose)
  
  # Calculate the Goodness-of-Fit
  mse <- mean((y_star - cate_est) ^ 2)
  sd_err <- sd((y_star - cate_est) ^ 2) / sqrt(n)

  # ----------------------------------------------------------------------------
  return(c(mse, sd_err))
}
