library(testthat)
test_that("Tests Selector Transformed", {
  context('transformed selector')
  # ----------------------------------------------------------------------------
  # test helper functions
  expect_equal(get_CV_sizes(22, 5),
               c(5, 5, 4, 4, 4),
               tolerance = 1e-7)
  set.seed(94060073)
  tr <- rbinom(n = 500, size = 1, prob = .1)
  expect_equal(getCV_indexes(tr = tr, k = 4)[1:4],
               c(4, 2, 2, 1))
  
  # ----------------------------------------------------------------------------
  set.seed(1423614230)
  
  dt <- simulate_causal_experiment(ntrain = 500,
                                   ntest = 10,
                                   dim = 6,
                                   alpha = 0.1,
                                   setup = "complexTau2")
  
  gof_values_S_BART <- gof_transformed(
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    feat = dt$feat_tr,
    estimator = S_BART,
    k = 3,
    emin = 1e-5
  )
  
  expect_equal(gof_values_S_BART,
               c(9642.2042, 736.9354), 
               tolerance = 1e-3)
  
  gof_values_S_RF <- gof_transformed(
    yobs = dt$Yobs_tr,
    tr = dt$W_tr,
    feat = dt$feat_tr,
    estimator = S_RF,
    k = 3,
    emin = 1e-5
  )
  
  expect_equal(gof_values_S_RF,
               c(10345.8423, 756.9765),
               tolerance = 1e-3)
})
