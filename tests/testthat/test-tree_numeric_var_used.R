library(testthat)
test_that("tests that variance optimal trees is working correctly", {
  set.seed(1242)
  x <- iris[,-1]
  x$Species <- as.numeric(x$Species)
  y_truth <- iris[, 1]
  se <- runif(length(y_truth)) * 5
  y <- y_truth + rnorm(length(y_truth), sd = se)
  mtry = 2
  nodesize = 4

  # tests that if no se is given that the standard algorithm is still running
  tree <- RFTree(
    x = x,
    y = y,
    se = NULL,
    mtry = mtry,
    nodesize = nodesize
  )

  expect_equal(
    predict(
      tree,
      x,
      x,
      y,
      avgfunc = avgMean,
      categoricalFeatureCols = list()
    )[1],
    6.461126,
    tolerance = 0.00001
  )

  # tests that if se is given the splitting and averaging is uses it.
  set.seed(1242)
  tree <- RFTree(
    x = x,
    y = y,
    se = se,
    mtry = mtry,
    nodesize = nodesize
  )

  expect_equal(
    predict(
      tree,
      x,
      x,
      y,
      avgfunc = avgMean,
      categoricalFeatureCols = list(),
      se = se)[1],
    6.233242,
    tolerance = 0.00001
  )

  expect_equal(
    predict(
      tree,
      x,
      x,
      y,
      avgfunc = weightedavgMean,
      categoricalFeatureCols = list(),
      se = se)[1],
    5.557541,
    tolerance = 0.00001
  )

})
