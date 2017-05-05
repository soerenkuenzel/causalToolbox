library(testthat)

test_that("Tests that autotuning is working correctly", {
  x <- iris[, -1]
  y <- iris[, 1]

  # Set seed for reproductivity
  set.seed(24750371)

  expect_warning(
  tuned_forest <- autohonestRF(x = x,
                               y = y,
                               num_iter = 9,
                               eta = 3 ),
  "honestRF is used as adaptive random forest."
  )

  y_pred <- predict(tuned_forest, x)

  expect_equal(sum((y_pred - y) ^ 2), 13.53225, tolerance=1e-4)
})
