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
               c(0.04759293, 0.01084847, 0.08433738),
               tolerance = 1e-7)

  sl <- S_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.01729103, -0.01133297,  0.04591503),
               tolerance = 1e-7)

  tl <- T_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    nthread = 1
  )
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1,]),
               c(0.04663806, -0.03260194,  0.12587805),
               tolerance = 1e-7)


  set.seed(21)

  expect_warning(
    xl_at <- X_RF_autotune_hyperband(
      feat = feat,
      tr = tr,
      yobs = yobs,
      num_iter = 2 ^ 3,
      eta = 2,
      verbose = FALSE,
      nthread = 1
    ),
    "honestRF is used as adaptive random forest."
  )

  # CIs <- CateCI(xl_at, feat, B = 5, verbose = FALSE)

  # expect_equal(as.numeric(CIs[1, ]),
  #              c(0.04387417, -0.03828391, 0.12603225),
  #              tolerance=1e-7)
})
