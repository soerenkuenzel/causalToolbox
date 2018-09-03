
library(testthat)
test_that(
  "Tests that MO-RF is working correctly",
  {
    context('MO-RF')
    set.seed(1423614230)
    feat <- iris[, -1]
    tr <- rbinom(nrow(iris), 1, .5)
    yobs <- iris[, 1]
    verbose = TRUE
    ntree = 20
    
    morf <- MO_RF(
      feat = feat,
      tr = tr,
      yobs = yobs,
      ntree = ntree
    )
  
  expect_equal(EstimateCate(morf, feat)[1], -0.08017449, tolerance = 1e-4)
  
  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 400,
      ntest = 100,
      dim = 20,
      alpha = .1,
      feat_distribution = "normal",
      setup = "RespSparseTau1strong",
      testseed = 543,
      trainseed = 234
    )
  
  morf <- MO_RF(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr,
    ntree = 50,
    nthread = 1
  )
  
  expect_equal(mean((
    EstimateCate(morf, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  603.9621,
  tolerance = 1)
  
  
  expect_output(smp_stats <- EstimateAllSampleStatistics(morf, B = 2))
  expect_equal(smp_stats$SATT[1, 3],
               -9.004184,
               tolerance = 1e-2)
  
})

