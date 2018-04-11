#' @include helper_functions.R
# ------------------------------------------------------------------------------
#' gof_double_robust
#' @title gof_double_robust
#' @name gof_double_robust
#' @param feat a data frame of features
#' @param yobs a vector of observations
#' @param tr a vector of group assignment (assume entries are integers)
#' @param estimator a learner constructor
#' @param emin Minimum value of the propensity score
#' @param k k fold cross validation
#' @param verbose TRUE for detailed output FALSE for no output
#' @return error
#' @import ranger
#' @export gof_double_robust
gof_double_robust <- function(feat, yobs, tr, estimator, emin, 
                              k = 5,
                              verbose = FALSE) {
  n_obs <- length(tr)
  # catch nonsensible specifications
  if (emin <= 0 | emin >= 0.5) {
    stop("0 < emin < 0.5")
  }
  
  catch_error(feat, yobs, tr, k)
  #-------------------------------------------------------------------------------
  #get k bootstrap subsets?
  #boot = bootstrap(feat, k, id = ".subset")

  # ----------------------------------------------------------------------------
  # Estimate propensity score: E(W|X)
  pscore_pred <- get_pscore(feat, tr, emin)

  # ----------------------------------------------------------------------------
  # Estimate the expected outcome: E(Y|X)
  outcome_pred <- get_pred_y(feat, yobs)

  # ----------------------------------------------------------------------------
  # For(i in 1:k):
  # 1. Train Estimator on everything but S_i and call the estimator \tau_i
  # 2. Use \tau_i to predict outcomes in in S_i and call them \hat tau_i
  cate_est <- compute_CATE_estimates(feat, yobs, tr, estimator, k, verbobse)
  # Return 
  # sum_i (\hat tau_i) ^ 2 - 2 \hat tau_i * (Y_i - \hat \mu(X_i)) / (W_i - \hat e(X_i))
  result <- sum(cate_est ^ 2 - 2 * cate_est * (yobs - outcome_pred) / (tr - pscore_pred))
  return(result)
}
