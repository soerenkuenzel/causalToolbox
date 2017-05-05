library(testthat)

test_that("Tests test-Xhrf_gpp", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  #
  #    ntree = 100
  #    nthread = 0
  #    verbose = TRUE
  #    init_points = 5
  #    n_iter = 1
  #
  #    starting_settings <- list(
  #      "start_setting_1" = get_setting_strong(feat, ntree, nthread),
  #      "start_setting_2" = get_setting_weak(feat, ntree, nthread)
  #    )
  #    setup_eval <-
  #      check_setups(starting_settings, feat, tr, yobs, ntree,
  #                   nthread, verbose)
  #
  #    starting_point <-
  #      starting_settings[[which.min(setup_eval$comb)]]
  #
  #    mean(as.numeric(evaluate_setting(starting_point, feat, tr, yobs)[1,]))
  #
  #
  #    Test_Fun_generic(starting_point,
  #                     feat,
  #                     tr,
  #                     yobs,
  #                     mtry_first = 1.2,
  #                     mtry_second = 1.5,
  #                     nodesizeAvg_first = 3.2,
  #                     nodesizeAvg_second = 3.3,
  #                     nodesizeSpl_first = 3.4,
  #                     nodesizeSpl_second = 3.1)
  #
  #
  #
  #   get_upper_bounds_for_nodesize(starting_point)
  #
  #   starting_point_optimized <-
  #     GP_optimize_small(starting_point, feat, tr, yobs)
  #


  expect_output(
    expect_warning(
      xl_gpp <- X_RF_autotune_gpp(
        feat,
        tr,
        yobs,
        ntree = 100,
        nthread = 0,
        verbose = FALSE,
        init_points = 1,
        n_iter = 1
      ),
      "honestRF is used as adaptive random forest."
    ))



  expect_equal(EstimateCate(xl_gpp, feat)[4],
               0.0981727,
               tolerance = 1e-5)

  set.seed(11122)
  expect_warning(
    CI <- CateCI(xl_gpp, feat, B = 5, verbose = FALSE)
  )
  expect_equal(CI[2, 3],
               0.3085303,
               tolerance = 1e-3)

  ## Example for changing internal parameters of GPfit::GP_fit and
  ## rBayesianOptimization::BayesianOptimization
  # # faster speed:
  # library(hte)
  # feat <- iris[, -1]
  # tr <- rbinom(nrow(iris), 1, .5)
  # yobs <- iris[, 1]
  #
  # # slow, using good optimization:
  # xl_gpp <- X_RF_autotune_gpp(
  #   feat,
  #   tr,
  #   yobs,
  #   ntree = 100,
  #   nthread = 0,
  #   verbose = TRUE,
  #   init_points = 5,
  #   n_iter = 1
  # )
  #
  # # fast, but with worse optimzation:
  # xl_gpp <- X_RF_autotune_gpp(
  #   feat,
  #   tr,
  #   yobs,
  #   ntree = 100,
  #   nthread = 0,
  #   verbose = TRUE,
  #   init_points = 5,
  #   n_iter = 1,
  #   maxit = 2
  # )


})
