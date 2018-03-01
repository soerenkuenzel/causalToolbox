library(testthat)
test_that("Tests Selector matching", {
  # ----------------------------------------------------------------------------
  set.seed(1423614231)
  
  dt <-
    simulate_causal_experiment(
      ntrain = 500,
      ntest = 10,
      dim = 6,
      alpha = 5,
      setup = "complexTau2"
    )
  
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
               c(70.975690, 7.841114),
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
               c(244.40207,  18.45547),
               tolerance = 1e-3)
})
