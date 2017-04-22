library(hte)
# Example 1: Use Iris dataset
set.seed(1423614230)
cate_problem <-
  simulate_causal_experiment(
    ntrain = 10000,
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
  verbose = FALSE
)
EstimateCate(xl, feat)


