#' @include helper_functions.R
# ------------------------------------------------------------------------------

#' gof_double_robust
#' @title gof_double_robust
#' @name gof_double_robust
#' @inheritParams gof_transformed
#' @return mean(error) and sd(error)
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
  pscore_pred <- estimate_pscore(feat, tr, emin)

  # ----------------------------------------------------------------------------
  # Estimate the expected outcome: E(Y|X)
  outcome_pred <- estimate_pred_y(feat, yobs)

  # ----------------------------------------------------------------------------
  # For(i in 1:k):
  # 1. Train Estimator on everything but S_i and call the estimator \tau_i
  # 2. Use \tau_i to predict outcomes in in S_i and call them \hat tau_i
  cate_est <- compute_CATE_estimates(feat, yobs, tr, estimator, k, verbose)
  
  err <- cate_est ^ 2 - 2 * cate_est * (yobs - outcome_pred) / (tr - pscore_pred)
  mean_err <- mean(err)
  sd_err <- sd(err) / sqrt(n_obs)
  return(c(mean_err, sd_err))
}
