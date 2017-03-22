# In this document, we test the performance of confidence intervals using
# X_hRF.

library(hte)
seed <- 2
set.seed(seed)

setup_loop <-
  c("RsparseT2weak", "RespSparseTau1strong", "complexTau")
dim_loop <- c(20, 50, 100)

# Experiment 1:
for (setup in setup_loop) {
  for (dim in dim_loop) {
    for (ntrain in c(100, 500, 1000, 2000)) {
      experiment <- simulate_causal_experiment(
        ntrain = ntrain,
        ntest = 1000,
        dim = dim,
        alpha = .1,
        setup = "RsparseT2weak"
      )

      L <- X_RF(
        feat = experiment$feat_tr,
        tr = experiment$W_tr,
        yobs = experiment$Yobs_tr,
        predmode = "propmean",
        ntree_first = 100,
        ntree = 100,
        nthread = 32
      )

      mean((experiment$tau_te - EstimateCate(L, experiment$feat_te)) ^ 2)
      CIs <- CateCI(L, feature_new = experiment$feat_te, B = 100)

      write.csv(
        cbind(CIs, experiment$tau_te),
        file = paste0(
          "tests/ConfidenceIntervals/data/",
          setup,
          "_n",
          ntrain,
          "_dim",
          dim,
          "_alpha.1_B100.csv"
        )
      )
    }
  }
}
