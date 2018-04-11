library(testthat)
test_that("Tests X_RF_autotune_hyperband2", {
  context("X_RF_autotune_hyperband2")
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
  tune_iter <- 1

  expect_output(
    xl <- autoJointforestry(
      feat,
      tr,
      yobs,
      sample.fraction = sample.fraction,
      num_iter = num_iter,
      eta = eta,
      verbose = verbose,
      seed = seed,
      nthread = nthread,
      tune_iter = tune_iter
    )
  )
  
  expect_equal(EstimateCate(xl, feat)[1],
               -0.04569598,
               tolerance = 1e-4)


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

  expect_output(
      xl_tuned <- autoJointforestry(
        feat = cate_problem$feat_tr,
        yobs = cate_problem$Yobs_tr,
        tr = cate_problem$W_tr,
        num_iter = 3 ^ 2,
        verbose = FALSE,
        tune_iter = 2)
  )

  expect_equal(mean((
    EstimateCate(xl_tuned, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  790.7614,
  tolerance = 1)

  # ----------------------------------------------------------------------------
  set.seed(21)
  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  num_iter <- 2 ^ 3
  eta <- 2
  verbose <- TRUE
  seed <- 24750371
  nthread <- 1

  expect_output(
    xl_at <- autoJointforestry(
      feat = feat,
      tr = tr,
      yobs = yobs,
      num_iter = num_iter,
      eta = eta,
      verbose = FALSE,
      nthread = 1,
      tune_iter = 1
    ),
    "Start joint tuning, iteration 1"
  )

  CIs <- CateCI(
    theObject = xl_at,
    feature_new = feat,
    B = 5,
    verbose = FALSE
  )
  
  #theObject = xl_at; feature_new = feat; B = 5; verbose = FALSE

  expect_equal(as.numeric(CIs[1, ]),
               c(-0.1854260, -0.5245950, 0.1537429),
               tolerance = 1e-4)

})

