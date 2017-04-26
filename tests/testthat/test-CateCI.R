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
  expect_equal(as.numeric(CIs[2, ]),
               c(0.123391197, -0.009570116, 0.256352511),
               tolerance=1e-7)

  sl <- S_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.01633633, -0.00774704, 0.04041970),
               tolerance=1e-7)

  tl <- T_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.04387417, -0.03828391, 0.12603225),
               tolerance=1e-7)


  set.seed(34221)
  xl_at <- X_RF_autotune_hyperband(
    feat = feat,
    tr = tr,
    yobs = yobs,
    num_iter = 2^3,
    eta = 2,
    verbose = FALSE,
    nthread = 1
  )

  CIs <- CateCI(xl_at, feat, B = 5, verbose = FALSE)

  expect_equal(as.numeric(CIs[1, ]),
               c(0.04387417, -0.03828391, 0.12603225),
               tolerance=1e-7)
})
