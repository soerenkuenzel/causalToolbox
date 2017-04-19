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

  expect_warning(
  CIs <- CateCI(xl, feat, B = 5, verbose = FALSE),
  "honestRF is used as adaptive random forest."
  )
  expect_equal(as.numeric(CIs[1, ]), c(0.1298953, -0.1173602, 0.3771509),
               tolerance=1e-5)

})
