library(testthat)

test_that("Tests that X-BART is working correctly", {
  context('X-BART')
  # library(testthat)
  set.seed(1423614230)
  dir.create("knownTestValues/", showWarnings = FALSE)
  tmp <- "knownTestValues/testvalues-X_BART"
  
  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  
  xb <- X_BART(feat, tr, yobs, ndpost = 100)
  
  expect_known_output(CateCI(xb, feature_new = feat[5 - 9,]), file = tmp)
  
})
