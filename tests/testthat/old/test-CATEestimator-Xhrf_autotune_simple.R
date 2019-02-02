library(testthat)
test_that("Tests test-Xhrf_autotune_simple", {
  context("X-RF autotune simple")
  set.seed(1423614230)
  
  feat <- iris[,-1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  ntree = 100
  nthread = 0
  # verbose = TRUE
  # relevant_Variable_first = 1:ncol(feat)
  # relevant_Variable_second = 1:ncol(feat)
  # relevant_Variable_prop = 1:ncol(feat)
  # starting_settings <- list(
  #   "start_setting_1" = get_setting_strong(feat, ntree, nthread),
  #   "start_setting_2" = get_setting_weak(feat, ntree, nthread)
  # )
  
  starting_settings <- get_starting_settings(
    feat = feat,
    tr = tr,
    ntree = ntree,
    nthread = nthread
  )
  
  
  setup_check <- check_setups(
    starting_settings = starting_settings,
    feat = feat,
    tr = tr,
    yobs = yobs,
    ntree = ntree,
    nthread = nthread,
    verbose = FALSE
  )
  
  expect_equal(setup_check[1, 2], 10.09047, tolerance = 1e-1)
  ### Test 2:
  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 200,
      ntest = 10000,
      dim = 20,
      alpha = .1,
      feat_distribution = "normal",
      setup = "RespSparseTau1strong",
      testseed = 543,
      trainseed = 234
    )
  
  expect_warning(
    mm <- X_RF_autotune_simple(
      feat = cate_problem$feat_tr,
      tr = cate_problem$W_tr,
      yobs = cate_problem$Yobs_tr,
      ntree = 20,
      ntree_testing = 10,
      nthread = 1,
      verbose = FALSE
    )
  )
  
  expect_equal(mean((
    EstimateCate(mm, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  193.9365, tolerance = 1)
  
  CATE_ci <- CateCI(mm, B = 2, cate_problem$feat_te, verbose = FALSE)
  
  expect_equal(CATE_ci[2, 2], -12.97698, tolerance = 1e-1)
})
