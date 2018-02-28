library(testthat)
test_that("Tests Selector matching", {
  # ----------------------------------------------------------------------------
  set.seed(1423614230)
  
  dt <-
    simulate_causal_experiment(
      ntrain = 500,
      ntest = 10,
      dim = 6,
      alpha = 5,
      setup = "complexTau2"
    )
  
  yobs = dt$Yobs_tr
  tr = dt$W_tr
  feat = dt$feat_tr
  estimator = S_BART
  k = 3
  estimand = 'ATE'
  replace = TRUE
  M = 1
  ties = FALSE
  emin = 1e-5
  
  expect_output(
    gof_values_S_BART <- gof_matching(
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      feat = dt$feat_tr,
      estimator = S_BART,
      k = 3
    )
  )
  
  expect_equal(gof_values_S_BART,
               c(16080.987, 1781.927),
               tolerance = 1e-3)
  
  expect_output(
    gof_values_S_RF <- gof_matching(
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      feat = dt$feat_tr,
      estimator = S_RF,
      k = 3
    )
  )
  
  expect_equal(gof_values_S_RF,
               c(15851.143, 1606.694),
               tolerance = 1e-3)
})
