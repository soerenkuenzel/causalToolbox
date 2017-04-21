test_that("Tests that XhRF is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  sampsize <- as.integer(nrow(feat) * 0.75)
  num_iter <- 3^2
  eta <- 3
  firststageVar <- NULL
  secondstageVar <- NULL
  verbose <- TRUE
  seed <- 24750371
  nthread <- 0

  xl <- X_RF_autotune_hyperband(
    feat = feat,
    tr = tr,
    yobs = yobs,
    sampsize = sampsize,
    num_iter = num_iter,
    eta = eta,
    firststageVar = firststageVar,
    secondstageVar = secondstageVar,
    verbose = FALSE,
    seed = seed,
    nthread = nthread
  )


  expect_equal(
    EstimateCate(xl, feat)[1],
    3.268899,
    tolerance=1e-3)

})
