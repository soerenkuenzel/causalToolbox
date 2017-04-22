test_that("Tests CateCI", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  xl <- X_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    verbose = FALSE
  )
  CIs <- CateCI(xl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[2, ]),
               c(0.1112299, -0.2054523, 0.4279120),
               tolerance=1e-3)

  sl <- S_RF(feat = feat,
             tr = tr,
             yobs = yobs)
  CIs <- CateCI(sl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.01610722, -0.01517955, 0.04739399),
               tolerance=1e-4)

  tl <- T_RF(feat = feat,
             tr = tr,
             yobs = yobs)
  CIs <- CateCI(tl, feat, B = 5, verbose = FALSE)
  expect_equal(as.numeric(CIs[1, ]),
               c(0.02942619, -0.39320332, 0.45205570),
               tolerance=1e-4)
})
