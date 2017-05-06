library(testthat)
test_that("Tests that XhRF is working correctly", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]

  predmode = "propmean"
  relevant_Variable_first = 1:ncol(feat)
  relevant_Variable_second = 1:ncol(feat)
  relevant_Variable_prop = 1:ncol(feat)
  ntree_first = 500
  ntree_second = 500
  ntree_prop = 500
  mtry_first = round(ncol(feat) / 2)
  mtry_second = ncol(feat)
  mtry_prop = max(floor(ncol(feat) / 3), 1)
  min_node_size_spl_first = 1
  min_node_size_ave_first = 5
  min_node_size_spl_second = 5
  min_node_size_ave_second = 3
  min_node_size_spl_prop = 3
  min_node_size_ave_prop = 10
  splitratio_first = .5
  splitratio_second = .5
  splitratio_prop = .5
  replace_first = TRUE
  replace_second = TRUE
  replace_prop = TRUE
  sample_fraction_first = 0.8
  sample_fraction_second = 0.9
  sample_fraction_prop = 1
  nthread = 0
  verbose = FALSE
  middleSplit_first = FALSE
  middleSplit_second = FALSE
  middleSplit_prop = FALSE



  xl <- X_RF(
    feat = feat,
    tr = tr,
    yobs = yobs,
    predmode = predmode,
    relevant_Variable_first = relevant_Variable_first,
    relevant_Variable_second = relevant_Variable_second,
    relevant_Variable_prop = relevant_Variable_prop,
    ntree_first = ntree_first,
    ntree_second = ntree_second,
    ntree_prop = ntree_prop,
    mtry_first = mtry_first,
    mtry_second = mtry_second,
    mtry_prop = mtry_prop,
    min_node_size_spl_first = min_node_size_spl_first,
    min_node_size_ave_first = min_node_size_ave_first,
    min_node_size_spl_second = min_node_size_spl_second,
    min_node_size_ave_second = min_node_size_ave_second,
    min_node_size_spl_prop = min_node_size_spl_prop,
    min_node_size_ave_prop = min_node_size_ave_prop,
    splitratio_first = splitratio_first,
    splitratio_second = splitratio_second,
    splitratio_prop = splitratio_prop,
    replace_first = replace_first,
    replace_second = replace_second,
    replace_prop = replace_prop,
    sample_fraction_first = sample_fraction_first,
    sample_fraction_second = sample_fraction_second,
    sample_fraction_prop = sample_fraction_prop,
    nthread = nthread,
    middleSplit_first = middleSplit_first,
    middleSplit_second = middleSplit_second,
    middleSplit_prop = middleSplit_prop,
    verbose = verbose
  )
  EstimateCate(xl, feat)[1]
  expect_equal(EstimateCate(xl, feat)[1], 0.1306867, tolerance = 1e-7)

  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 400,
      ntest = 100,
      dim = 20,
      alpha = .1,
      feat_distribution = "normal",
      setup = "RespSparseTau1strong",
      testseed = 543,
      trainseed = 234
    )

  expect_warning(
    xl <- X_RF(
      feat = cate_problem$feat_tr,
      yobs = cate_problem$Yobs_tr,
      tr = cate_problem$W_tr,
      ntree_first = 50,
      ntree_second = 50,
      # ntree_prop = 50,
      verbose = FALSE,
      nthread = 1
    ),
    "honestRF is used as adaptive random forest."
  )

  expect_equal(mean((
    EstimateCate(xl, cate_problem$feat_te) - cate_problem$tau_te
  ) ^ 2),
  742.9669,
  tolerance = 1e-7)

})
