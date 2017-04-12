# In this document, we test the performance of confidence intervals using
# X_hRF.

library(hte)
seed <- 2
set.seed(seed)

setup_loop <- c("Conf1", "STMpp", "Ufail", "Usual1", "Wager1")
# c("RespSparseTau1strong", "RsparseT2weak", "complexTau", "Conf1", "rare1",
# "STMpp", "Ufail", "Usual1", "Wager1", "Wager2", "Wager3")
dim_loop <- c(20, 50, 100)
ntrain_loop <- c(100, 500, 1000, 2000, 10000, 100000)

# Experiment 1:
for (ntrain in ntrain_loop){
  for (setup in setup_loop) {
    for (dim in dim_loop) {
      print(paste("setup =", setup, "dim =", dim, "ntrain =", ntrain))

      experiment <- simulate_causal_experiment(
        ntrain = ntrain,
        ntest = 1000,
        dim = dim,
        alpha = .1,
        setup = setup
      )

      L <- X_RF(
        feat = experiment$feat_tr,
        tr = experiment$W_tr,
        yobs = experiment$Yobs_tr,
        predmode = "propmean",
        ntree_first = 100,
        ntree = 100,
        nthread = 24
      )

      CIs <- tryCatch({
          CateCI(L, feature_new = experiment$feat_te, B = 100)
      },
      error = function(e){
          print(e)
          NA
      }
      )

      write.csv(
        cbind(CIs, experiment$tau_te),
        file = paste0(
          "data/",
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
