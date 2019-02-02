library(testthat)
test_that("Tests that XhRF is working correctly", {
  context('X-RF')
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  tl <- T_RF(feat = feat, tr = tr, yobs = yobs)
  EstimateCate(tl, feat)[1]
  expect_equal(EstimateCate(tl, feat)[1], 0.004849434, tolerance = 1e-2)

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

  tl <- T_RF(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr
  )
  
  expect_equal(mean((
    EstimateCate(tl, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  120.0557,
  tolerance = 1)

  expect_output(smp_stats <- EstimateAllSampleStatistics(tl, B = 2))
  # theObject = xl; method = "maintain_group_ratios"; B = 200; nthread = 0;
  # verbose = TRUE
  expect_equal(smp_stats$SATE[1, 2], 3.556161, tolerance = 1e-1)
})
