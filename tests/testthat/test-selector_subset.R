library(testthat)
test_that("Tests Selector Subset", {
  set.seed(1423614230)
  
  dt <- simulate_causal_experiment(ntrain = 500, ntest = 10, dim = 6, alpha = 5,
                             setup = "complexTau2")
  
  feat <- dt$feat_tr
  yobs <- dt$Yobs_tr
  tr <- dt$W_tr
  estimator <- S_RF
  important.features = colnames(feat)
  min.treat.size.per.group = 25
  normalize = "studentize"
  k = 5
  
  expect_output(
    gof_values_S_BART <- gof_subset(
      feat = dt$feat_tr,
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      estimator = S_BART,
      important.features = colnames(feat),
      min.treat.size.per.group = 25,
      normalize = "studentize",
      k = 5
    )
  )
  
  expect_equal(gof_values_S_BART,
               c(41.75972, 29.95785), 
               tolerance = 1e-3)
  
  expect_output(
    gof_values_S_RF <- gof_subset(
      feat = dt$feat_tr,
      yobs = dt$Yobs_tr,
      tr = dt$W_tr,
      estimator = T_RF,
      important.features = colnames(feat),
      min.treat.size.per.group = 25,
      normalize = "studentize",
      k = 5
    )
  )
  
  expect_equal(gof_values_S_RF,
               c(5.756947, 3.308356),
               tolerance = 1e-3)
})
