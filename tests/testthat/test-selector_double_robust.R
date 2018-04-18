library(testthat)
test_that("Tests Selector Double_Robust", {
  context('double_robust selector')
  set.seed(1423614230)
  
  dt <- simulate_causal_experiment(ntrain = 500,
                                   ntest = 10,
                                   dim = 6,
                                   alpha = 0.1,
                                   setup = "complexTau2")
  
  gof_values_S_BART <- gof_double_robust(
    feat = dt$feat_tr,
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    estimator = S_BART,
    k = 3,
    emin = 1e-5
  )
  
  expect_equal(gof_values_S_BART,
               c(-701.73279, 99.38653), 
               tolerance = 1e-3)
  
  gof_values_S_RF <- gof_double_robust(
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    feat = dt$feat_tr,
    estimator = S_RF,
    k = 3,
    emin = 1e-5
  )
  
  expect_equal(gof_values_S_RF,
               c(-40.397062, 6.117541),
               tolerance = 1e-3)
})
