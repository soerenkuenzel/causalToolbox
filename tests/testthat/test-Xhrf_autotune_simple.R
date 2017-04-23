test_that("Tests test-Xhrf_autotune_simple", {
  set.seed(1423614230)

  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  ntree = 500
  nthread = 0

  get_setting_strong(feat,
                     tr,
                     ntree,
                     nthread,
                     relevant_Variable_first = 1:ncol(feat),
                     relevant_Variable_second = 1:ncol(feat),
                     relevant_Variable_prop = 1:ncol(feat))

  get_setting_weak(feat,
                   tr,
                   ntree,
                   nthread,
                   relevant_Variable_first = 1:ncol(feat),
                   relevant_Variable_second = 1:ncol(feat),
                   relevant_Variable_prop = 1:ncol(feat))

}
)
