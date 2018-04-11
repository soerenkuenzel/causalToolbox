library(testthat)
test_that("Tests Selector matching", {
  # ----------------------------------------------------------------------------
  context('Matching Selector')
  set.seed(1423614231)
  
  dt <-
    simulate_causal_experiment(
      ntrain = 500,
      ntest = 10,
      dim = 6,
      alpha = 5,
      setup = "complexTau2"
    )
  
  gof_values_S_BART <- gof_matching(
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    feat = dt$feat_tr,
    estimator = S_BART,
    k = 3
  )
  
  expect_equal(gof_values_S_BART,
               c(70.97569, 11.38908),
               tolerance = 1e-3)
  
  gof_values_S_RF <- gof_matching(
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    feat = dt$feat_tr,
    estimator = S_RF,
    k = 3
  )
  
  expect_equal(gof_values_S_RF,
               c(227.53828, 25.34041),
               tolerance = 1e-3)
})
