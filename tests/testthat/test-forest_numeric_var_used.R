library(testthat)
test_that("tests that variance optimal forests is working correctly", {
  set.seed(1242)
  x <- iris[,-1]
  x$Species <- as.numeric(x$Species)
  y_truth <- iris[, 1]
  se <- runif(length(y_truth)) * 5
  y <- y_truth + rnorm(length(y_truth), sd = se)
  mtry = 2
  nodesize = 4

  # tests that if no se is given that the standard algorithm is still running
  set.seed(1242)
  forest <- RF(
    x = x,
    y = y,
    # se = NULL,
    mtry = mtry,
    nodesize = nodesize,
    ntree = 4
  )

  expect_equal(
    predict(
      forest,
      x
      )[1],
    5.365302,
    tolerance = 0.00001
  )

  # tests that if we provide se the strucutre of the trees chanages
  set.seed(1242)
  forest <- RF(
    x = x,
    y = y,
    se = se,
    mtry = mtry,
    nodesize = nodesize,
    ntree = 4,
    avgfunc = avgMean
  )

  expect_equal(
    predict(
      forest,
      x
    )[1],
    5.337218,
    tolerance = 0.00001
  )

  # next we also change the averaging criterion to see that there is another
  # change
  set.seed(1242)
  forest <- RF(
    x = x,
    y = y,
    se = se,
    mtry = mtry,
    nodesize = nodesize,
    ntree = 4,
    avgfunc = weightedavgMean
  )

  expect_equal(
    predict(
      forest,
      x
    )[1],
    5.444302,
    tolerance = 0.00001
  )

})
