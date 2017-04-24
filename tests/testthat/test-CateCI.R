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
               c(0.14112163, 0.02522049, 0.25702278),
               tolerance=1e-7)

  sl <- S_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.022837778, -0.007604193, 0.053279749),
               tolerance=1e-7)

  tl <- T_RF(feat = feat,
             tr = tr,
             yobs = yobs,
             nthread = 1)
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.03268183, -0.03974874, 0.10511239),
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

  xl_at@creator(feat, tr, yobs)

  CIs <- CateCI(xl_at, feat, B = 5, verbose = FALSE)

  expect_equal(as.numeric(CIs[1, ]),
               c(0.1233837, -0.3876998, 0.6344671),
               tolerance=1e-7)
})
