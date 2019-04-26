library(testthat)
test_that("Test causal experiment creator", {
  context('causal experiment creator')
  
  ce <- simulate_causal_experiment(
    ntrain = 20,
    ntest = 20,
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
  
  expect_equal(ce$Yobs_tr[1:10], 
               c(-71.628878, -4.562855, -82.279931, 59.705728, -22.144589, 
                 7.685433, 65.266380, 118.229187, 1.079148, 25.941377))
  
})
