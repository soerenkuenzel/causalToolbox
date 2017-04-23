test_that("Tests test-Xhrf_gpp_gpp", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  ntree = 100
  nthread = 0
  verbose = TRUE

  starting_settings <- list(
    "start_setting_1" = get_setting_strong(feat, tr, ntree, nthread),
    "start_setting_2" = get_setting_weak(feat, tr, ntree, nthread)
  )

  mean(as.numeric(evaluate_setting(starting_point, feat, tr, yobs)[1,]))


  Test_Fun_generic(starting_point,
                   feat, tr, yobs,
                   mtry_first_0 = 1,
                   mtry_first_1 = 1,
                   mtry_second_0 = 1,
                   mtry_second_1 = 1,
                   nodesizeAvg_first_0 = 1,
                   nodesizeAvg_first_1 = 1,
                   nodesizeAvg_second_0 = 1,
                   nodesizeAvg_second_1 = 1,
                   nodesizeSpl_first_0 = 1,
                   nodesizeSpl_first_1 = 1,
                   nodesizeSpl_second_0 = 1,
                   nodesizeSpl_second_1 = 1,
                   sampsize_first_0 = sum(1 - tr)/3,
                   sampsize_first_1 = sum(tr)/3,
                   sampsize_second_0 = sum(1 - tr)/3,
                   sampsize_second_1 = sum(1 - tr) / 3,
                   splitratio_second_0 = .5,
                   splitratio_second_1 = .5)


}
)
