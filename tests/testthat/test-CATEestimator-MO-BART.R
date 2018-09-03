
library(testthat)
test_that(
  "Tests that MO-BART is working correctly",
  {
    context('MO-BART')
    set.seed(1423614230)
    feat <- iris[, -1]
    tr <- rbinom(nrow(iris), 1, .5)
    yobs <- iris[, 1]
    verbose = TRUE
    ntree = 20
    
    mo_bart <- MO_BART(
      feat = feat,
      tr = tr,
      yobs = yobs,
      ntree = ntree
    )

  expect_equal(EstimateCate(mo_bart, feat)[1], 0.07434052, tolerance = 1e-4)

  
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
  
  mo_bart <- MO_BART(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr,
    ntree = 50,
  )
  

  
  expect_equal(  mean((
    EstimateCate(mo_bart, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  222.4855,
  tolerance = 1)
  
  
  # expect_output(smp_stats <- EstimateAllSampleStatistics(morf, B = 2))
  #expect_equal(smp_stats$SATT[1, 3],
  #             -9.004184,
  #             tolerance = 1e-2)
  
})

