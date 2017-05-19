library(testthat)
test_that("Test non-continuous split", {
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "rare2")
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "rare3")
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "STMpp2")
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "STMpp3")
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "complexTau2")
  simulate_causal_experiment(ntrain = 40, ntest = 40, dim = 6, alpha = 5,
                             setup = "WA4")
})
