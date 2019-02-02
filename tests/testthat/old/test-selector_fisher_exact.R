library(testthat)
test_that("Tests Selector Transformed", {
  context('fisher exact null selector')
  set.seed(1423614230)
  
  dt <- simulate_causal_experiment(ntrain = 500,
                                   ntest = 10,
                                   dim = 6,
                                   alpha = 0.1,
                                   setup = "complexTau2")
  
  expect_output(
    gof_values_S_BART <- gof_fisher_exact(
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      feat = dt$feat_tr,
      estimator = S_BART,
      k = 3
  ))
  
  expect_equal(gof_values_S_BART,
               0.3761287, 
               tolerance = 1e-3)
  
  expect_output(
    gof_values_S_RF <- gof_fisher_exact(
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      feat = dt$feat_tr,
      estimator = S_RF,
      k = 3
  ))
  
  expect_equal(gof_values_S_RF,
               0.1426197,
               tolerance = 1e-3)
})
