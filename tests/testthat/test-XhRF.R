test_that("Tests that XhRF is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  xl <- X_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 0,
    verbose = FALSE
  )

  expect_equal(EstimateCate(xl, feat)[1], 0.1292386, tolerance=1e-3)

  CIs <- CateCI(xl, feat, B = 5, verbose = FALSE)

  expect_equal(as.numeric(CIs[1, ]), c(0.1292386, -0.1270228, 0.3855000),
               tolerance=1e-5)

})
