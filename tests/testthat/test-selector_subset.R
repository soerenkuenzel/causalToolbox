library(testthat)
test_that("Tests Selector Subset", {
  context('Subset Selector')
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
  
  expect_equal(gof_values_S_BART,
               c(96.07298, 59.19640), 
               tolerance = 1e-3)
  
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
  
  expect_equal(gof_values_S_RF,
               c(28.44484, 11.22975),
               tolerance = 1e-3)
})
