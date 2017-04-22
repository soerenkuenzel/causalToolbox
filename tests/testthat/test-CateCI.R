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
               c(0.1388921, -0.1206587, 0.3984428),
               tolerance=1e-7)

  sl <- S_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.01610722, -0.015179546, 0.0473939905),
               tolerance=1e-7)

  tl <- T_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.02942619, -0.39320332, 0.45205570),
               tolerance=1e-7)


  set.seed(342342)
  xl_at <- X_RF_autotune_hyperband(
    feat = feat,
    tr = tr,
    yobs = yobs,
    num_iter = 20,
    eta = 3,
    verbose = FALSE,
    nthread = 1
  )

  CIs <- CateCI(xl_at, feat, B = 5, verbose = FALSE)

  expect_equal(as.numeric(CIs[1, ]),
               c(0.1233837, -0.3876998, 0.6344671),
               tolerance=1e-7)
})
