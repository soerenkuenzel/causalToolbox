library(testthat)
test_that("tests that RF is working", {
  set.seed(1242)
  x <- iris[,-1]
  x$Species <- as.numeric(x$Species)
  y <- iris[, 1]

  tree <- RFTree(
    x = x,
    y = y,
    mtry = 2,
    nodesize = 2
  )

  expect_equal(
    predict(tree,
            x,
            x,
            y,
            avgfunc = avgMean,
            categoricalFeatureCols = list())[1],
    5.266667,
    tolerance = 0.00001
  )
})
