# In this document, we test the performance of confidence intervals using
# X_hRF.

library(hte)
library(causalForest)
library(randomForestCI)
library(causalForest)
seed <- 2
set.seed(seed)

setup_loop <- c("Conf1", "STMpp", "Ufail", "Usual1", "Wager1")
# c("RespSparseTau1strong", "RsparseT2weak", "complexTau", "Conf1", "rare1",
# "STMpp", "Ufail", "Usual1", "Wager1", "Wager2", "Wager3")

dim_loop <- c(20, 50, 100)
ntrain_loop <- c(100, 500, 1000, 2000, 10000, 100000)

# Experiment 1:
for (ntrain in ntrain_loop) {
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
        forest <-
            tryCatch({
        causalForest(
          X = experiment$feat_tr,
          Y = experiment$Yobs_tr,
          W = experiment$W_tr,
          num.trees = 100,
          sample.size = round(length(experiment$Yobs_tr) / 2)
        )
            },
        error = function(e){
            print(e)
            NA
        })
                                        #make predictions on the test sample

        predictions <-
            tryCatch({
                predict(forest, experiment$feat_te)
            },
            error = function(e){
                NA}
            )

      CIs <- tryCatch({
        #get confidence intervals on the predictions
        forest.ci = randomForestInfJack(forest,
                                        experiment$feat_te,
                                        calibrate = TRUE)
        cbind(predictions, forest.ci)
      },
      error = function(e) {
        print(e)
        forest.ci <- tryCatch({
            randomForestInfJack(forest,
                                experiment$feat_te,
                                calibrate = FALSE)
        },
        error = function(e){
            print(e)
            NA
        })

        cbind(predictions, forest.ci)
      })

      write.csv(
        cbind(CIs, experiment$tau_te),
        file = paste0(
          "data/causalForest__",
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
