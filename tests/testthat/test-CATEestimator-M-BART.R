library(testthat)
test_that("Tests that M-BART is working correctly", {
  dir.create("knownTestValues/", showWarnings = FALSE)
  context('M-BART')
  set.seed(1423614230)
  feat <- iris[, -1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  verbose = TRUE
  ntree = 20
  
  mo_bart <- M_BART(
    feat = feat,
    tr = tr,
    yobs = yobs,
    ntree = ntree
  )
  
  expect_known_output(EstimateCate(mo_bart, feat),
                      file = "knownTestValues/testvalues-M_BART1")
  
  
  expect_known_output(CateCI(
    theObject = mo_bart,
    feature_new = feat,
    B = 5
  ),
  file = "knownTestValues/testvalues-M_BART2")
  
  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 400,
      ntest = 100,
      dim = 10,
      alpha = .1,
      feat_distribution = "normal"
    )
  
  mbart <- M_BART(
    feat = cate_problem$feat_tr,
    yobs = cate_problem$Yobs_tr,
    tr = cate_problem$W_tr,
    ntree = 50
  )
  
  expect_known_output(EstimateCate(mbart, cate_problem$feat_te),
                      file = "knownTestValues/testvalues-M_BART3")
  
  
})
