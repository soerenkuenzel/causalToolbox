library(testthat)
test_that("Tests that XhRF is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  verbose = TRUE
  ntree = 20

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    ntree = ntree
  )

  expect_equal(EstimateCate(sl, feat)[1], -0.02491667, tolerance = 1e-7)

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

  sl <- S_RF(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr,
    ntree = 50,
    nthread = 1
  )

  expect_equal(mean((
    EstimateCate(sl, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  813.7427,
  tolerance = 1e-7)


  expect_output(smp_stats <- EstimateAllSampleStatistics(sl, B = 2))
  expect_equal(smp_stats$SATT[1, 3],
               -7.969987,
               tolerance = 1e-7)

})
