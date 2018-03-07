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
               c(0.068551097, 0.008933832, 0.128168362),
               tolerance = 1e-4)

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.0003204346, -0.0438700601,  0.0445109292),
               tolerance = 1e-4)

  tl <- T_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(-0.01093721, -0.14335959,  0.12148516),
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
