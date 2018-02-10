library(testthat)
test_that("Tests CateCI", {
  
  # ----------------------------------------------------------------------------
  # test helper functions
  expect_equal(get_CV_sizes(22, 5),
               c(5, 5, 4, 4, 4),
               tolerance = 1e-7)
  set.seed(94060073)
  tr <- rbinom(n = 500, size = 1, prob = .1)
  expect_equal(getCV_indexes(tr = tr, k = 4)[1:4],
               c(4, 2, 2, 1))
  
  
  # ----------------------------------------------------------------------------
  set.seed(1423614230)
  
  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  
  xb <- X_BART(feat, tr, yobs, ensemble = "pscore", ndpost = 10)
  
})
