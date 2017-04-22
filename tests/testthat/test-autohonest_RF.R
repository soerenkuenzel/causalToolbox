test_that("Tests that autotuning is working correctly", {
  x <- iris[, -1]
  y <- iris[, 1]

  # Set seed for reproductivity
  set.seed(24750371)

  tuned_forest <- autohonestRF(x = x,
                               y = y,
                               num_iter = 100,
                               eta = 3 )

  y_pred <- predict(tuned_forest, x)

  expect_equal(sum((y_pred - y) ^ 2), 15.37539, tolerance=1e-4)
})
