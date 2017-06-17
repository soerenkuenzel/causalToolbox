library(testthat)
test_that("tests that RF is working", {
  set.seed(1242)
  x <- iris[,-1]
  y <- iris[, 1]
  fo <- RF(x = x, y = y, ntree = 5)

  expect_equal(predict(fo, feature.new = x)[1], 5.148312, tolerance = 0.00001)
})
