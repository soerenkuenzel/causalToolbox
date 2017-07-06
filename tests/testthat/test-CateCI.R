library(testthat)
test_that("Tests CateCI", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  expect_warning(
    xl <- X_RF(
      feat = feat,
      tr = tr,
      yobs = yobs,
      nthread = 1,
      verbose = FALSE
    ),
    "honestRF is used as adaptive random forest."
  )
  expect_warning(
    CIs <- CateCI(xl, feat, B = 5, verbose = FALSE),
    "honestRF is used as adaptive random forest."
  )
  expect_equal(as.numeric(CIs[2,]),
               c(0.04332, 0.00323, 0.08342),
               tolerance = 1e-4)

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.0124, -0.0120,  0.0367),
               tolerance = 1e-4)

  tl <- T_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.07120, -0.00593,  0.14834),
               tolerance = 1e-4)

  tl <- T_BART(
    feat = feat,
    tr = tr,
    yobs = yobs
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.01734046, -0.36126050,  0.37568245),
               tolerance = 1e-7)
})
