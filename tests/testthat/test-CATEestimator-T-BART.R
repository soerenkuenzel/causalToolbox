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

})
