test_that("Tests that XhRF is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  expect_warning(
  xl <- X_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 8,
    verbose = FALSE
  ),
  "honestRF is used as adaptive random forest."
)

  expect_equal(EstimateCate(xl, feat)[1], 0.1298953446, tolerance=1e-7)
})
