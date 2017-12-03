library(testthat)
test_that("Tests CateCI", {
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
  CIs <- CateCI(xl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[2,]),
               c(0.04401702, 0.00134515, 0.08668890),
               tolerance = 1e-4)

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.008396746, -0.021596981, 0.038390473),
               tolerance = 1e-4)

  tl <- T_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.03722925, -0.04627296, 0.12073145),
               tolerance = 1e-4)

  tl <- T_BART(
    feat = feat,
    tr = tr,
    yobs = yobs
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.01734046, -0.36126050, 0.37568245),
               tolerance = 1e-7)
})
