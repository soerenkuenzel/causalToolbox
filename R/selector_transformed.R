#' @include helper_functions.R
#-------------------------------------------------------------------------------
#' gof_transformed
#' @title gof_transformed
#' @name gof_transformed
#' @param feat A data frame of features
#' @param yobs A vector of observations
#' @param tr A vector of group assignment (assume entries are integers)
#' @param estimator A learner constructor
#' @param k Number of folds used for cross validation
#' @param emin the pscore prediciton will be bounded between emin and 1 - emin
#' to avoid decide by 0 error
#' @param verbose determines whether detailed updates will be printed
#' @return mean(error) and sd(error)
#' @import ranger
#' @export gof_transformed
gof_transformed <- function(feat,
                            yobs,
                            tr,
                            estimator,
                            k = 2,
                            emin = 1e-5,
                            verbose = FALSE) {
  n = length(tr)
  # catch nonsensible specifications
  if (emin <= 0 | emin >= 0.5) {
    stop("0 < emin < 0.5")
  }
  catch_error(feat, yobs, tr, k)
  # ----------------------------------------------------------------------------
  # Compute the CATE estimates using a k fold CV
  cate_est <- compute_CATE_estimates(feat, yobs, tr, estimator, k, verbose)

  # ----------------------------------------------------------------------------
  # Compute the Y star and evaluate the model
  
  # Estimate propensity score
  pscore_pred <- get_pscore(feat, tr, emin)
  
  # Calculate y_star_te
  y_star <- yobs / (tr * pscore_pred - (1 - tr) * (1 - pscore_pred))

  # Calculate the Goodness-of-Fit
  mse <- mean((y_star - cate_est) ^ 2)
  sd_err <- sd((y_star - cate_est) ^ 2) / sqrt(n)

  # ----------------------------------------------------------------------------
  return(c(mse, sd_err))
}
