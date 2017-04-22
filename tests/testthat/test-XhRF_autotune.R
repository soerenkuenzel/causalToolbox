test_that("Tests that X_RF_autotune_hyperband is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  sampsize <- as.integer(nrow(feat) * 0.75)
  num_iter <- 3 ^ 2
  eta <- 3
  verbose <- TRUE
  seed <- 24750371
  nthread <- 1

  xl <- X_RF_autotune_hyperband(
    feat = feat,
    tr = tr,
    yobs = yobs,
    sampsize = sampsize,
    num_iter = num_iter,
    eta = eta,
    verbose = FALSE,
    seed = seed,
    nthread = nthread
  )


  expect_equal(EstimateCate(xl, feat)[1],
               0.2394491,
               tolerance = 1e-7)


  #### real example ####
  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 400,
      ntest = 10000,
      dim = 20,
      alpha = .1,
      feat_distribution = "normal",
      setup = "RespSparseTau1strong",
      testseed = 543,
      trainseed = 234
    )

  xl_tuned <- X_RF_autotune_hyperband(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr,
    num_iter = 3 ^ 2,
    verbose = FALSE
  )

  expect_equal(mean((
    EstimateCate(xl_tuned, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  922.1623,
  tolerance = 1e-5)

})
