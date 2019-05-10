library(testthat)
test_that("Tests CateCI", {
  context('CateCI')
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  xl <- X_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1,
    verbose = FALSE
  )
  CIs <- CateCI(theObject = xl, feature_new = feat,
                bootstrapVersion = "normalApprox", B = 5, verbose = FALSE)
  testthat::expect_equal(as.numeric(CIs[2,]),
                         c(0.06772533, -0.02262265, 0.15807331),
                         tolerance = 1e-4)

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.04135227, -0.02449547, 0.10720001),
               tolerance = 1e-4)

  tl <- T_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.13283920,-0.05397504, 0.31965345),
               tolerance = 1e-4)

  tl <- T_BART(
    feat = feat,
    tr = tr,
    yobs = yobs
  )
  expect_silent(CIs <- CateCI(tl, feat, B = 5, verbose = FALSE))
  expect_equal(as.numeric(CIs[1,]),
               c(0.1770678, -0.2224571, 0.5581970),
               tolerance = 1e-1)

  expect_warning(CIs <- CateCI(
    theObject = xl,
    feature_new = feat[1:100,],
    bootstrapVersion = "smoothed",
    B = 5,
    verbose = FALSE,
    method = "maintain_group_ratios",
    nthread = 0
  ))
  testthat::expect_equal(as.numeric(CIs[2,]),
                         c(0.08154777, -0.90844635, 1.07154188),
                         tolerance = 1e-4)
})
