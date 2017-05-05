library(testthat)
test_that("Tests X_RF_autotune_hyperband", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  sample.fraction <- .75
  num_iter <- 3 ^ 2
  eta <- 3
  verbose <- TRUE
  seed <- 24750371
  nthread <- 1

  expect_warning(
    xl <- X_RF_autotune_hyperband(
      feat = feat,
      tr = tr,
      yobs = yobs,
      sample.fraction = sample.fraction,
      num_iter = num_iter,
      eta = eta,
      verbose = FALSE,
      seed = seed,
      nthread = nthread
    ),
    "honestRF is used as adaptive random forest."
  )



  expect_equal(EstimateCate(xl, feat)[1],
               0.3294971,
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

  expect_warning(
    xl_tuned <- X_RF_autotune_hyperband(
      feat = cate_problem$feat_tr,
      yobs = cate_problem$Yobs_tr,
      tr = cate_problem$W_tr,
      num_iter = 3 ^ 2,
      verbose = FALSE
    ),
    "honestRF is used as adaptive random forest."
  )

  expect_equal(mean((
    EstimateCate(xl_tuned, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  196.7121,
  tolerance = 1e-5)
})
