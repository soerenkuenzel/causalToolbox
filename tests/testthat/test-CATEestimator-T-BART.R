library(testthat)
test_that("Tests that T-BART is working correctly", {
  context('T-BART')
  # library(testthat)
  set.seed(1423614230)
  dir.create("knownTestValues/", showWarnings = FALSE)

  ce <- simulate_causal_experiment(
    ntrain = 200,
    ntest = 200,
    dim = 3,
    alpha = .1,
    feat_distribution = "normal",
    given_features = NULL,
    pscore = "rct5",
    mu0 = "sparseLinearStrong",
    tau = "sparseLinearWeak",
    testseed = 4972609,
    trainseed = 1184332
  ) 
  
  feat <- ce$feat_tr
  tr <- ce$W_tr
  yobs <- ce$Yobs_tr


  set.seed(1111)
  tb <- T_BART(
    feat,
    tr,
    yobs,
    ndpost = 10,
    ntree = 200)
  expect_silent(smpleStats <- EstimateAllSampleStatistics(tb))

  expect_known_output(smpleStats$SATE[1, 2], 
                      file = "knownTestValues/testvalues-T_BART5")

  set.seed(1111)
  tb <- T_BART(
    feat,
    tr,
    yobs,
    ndpost = 10,
    ntree = 100)
  expect_silent(smpleStats <- EstimateAllSampleStatistics(tb))
  expect_known_output(smpleStats$SATE[1, 2], file = "knownTestValues/testvalues-T_BART6")

})
