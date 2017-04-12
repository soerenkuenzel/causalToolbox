# File goes through dim_grid, ntrain_grid, alpha_grid, seed_grid, setup_grid it
# trains each estimator and computes the MSE, and their SD

# args <- commandArgs(TRUE)
# setup_i <- as.numeric(args[1])

set.seed(1145)

library(hte)
library(dplyr)
library(reshape)


dim_grid <- c(5, 10, 20, 100)

ntrain_grid <- c(400, 600, 800, 1000, 1500, 2000, 4000, 10000)
if (setup_name == "rare1"){
  ntrain_grid <- c(1500, 2000, 4000, 10000, 40000)
}

ntest <- 5000

seed_grid <- 1:2

alpha_grid <- c(0, .3, 1, 2)

setup_grid <- setup_loop <- list(
  "RsparseT1strong" = setup_RespSparseTau1strong,
  "RsparseT2weak" = setup_RespSparseTau2weak,
  "Conf1" = setup_Conf1,
  "rare1" = setup_rare1,
  "STMpp" = setup_STMpp,
  "TTMpp" = setup_TTMpp,
  "Ufail" = setup_Ufail,
  "Usual1" = setup_Usual1,
  "Wager1" = setup_Wager1,
  "Wager2" = setup_Wager2,
  "Wager3" = setup_Wager3,
  # Some correlation
  "RsparseT1strongA.3" = function(ntrain, ntest, dim){
    setup_RespSparseTau1strong(ntrain, ntest, dim, alpha = .3)},
  "RsparseT2weakA.3" =  function(ntrain, ntest, dim){
    setup_RespSparseTau2weak(ntrain, ntest, dim, alpha = .3)},
  "Conf1A.3" = function(ntrain, ntest, dim){
    setup_Conf1(ntrain, ntest, dim, alpha = .3)}}


simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 4, alpha = .1,
                           setup = "RsparseT2weak")


setup <- setup_grid[[setup_i]]
setup_name <- names(setup_grid)[setup_i]
print(setup_name)

filename <-
  paste0("Summaries/MSErates/MSE_rates_", setup_name, ".csv")
file.remove(filename)



dim_grid <- c(5, 10, 20, 100)
ntrain_grid <- c(400, 600, 800, 1000, 1500, 2000, 4000, 10000)
if (setup_name == "rare1")
  ntrain_grid <- c(1500, 2000, 4000, 10000, 40000)
ntest <- 5000
seed_grid <- 1:2
alpha_grid <- c(0, .3, 1, 2)

for (seed in seed_grid) {
  for (dim in dim_grid) {
    for (alpha in alpha_grid) {
      print(paste(
        "Starting with seed",
        seed,
        "of",
        max(seed_grid),
        "dim",
        dim,
        "of",
        paste(dim_grid, collapse = ", ")
      ))
      for (ntrain in ntrain_grid) {
        set.seed(seed)
        setup(ntrain, ntest, dim, alpha)
        for (estimator_i in 1:length(estimator_grid)) {
          estimator <- estimator_grid[[estimator_i]]
          estimator_name <- names(estimator_grid)[estimator_i]

          tryCatch({
            L <- estimator(feat = feat_tr,
                           W = W_tr,
                           Yobs = Yobs_tr)
            estimates <- EstimateCate(L, feat_te)
            MSE    <<- mean((tau_te - estimates) ^ 2)
            MSE_sd <<-
              sd((tau_te - estimates) ^ 2) / sqrt(length(tau_te))
            MAE    <<- mean(abs(tau_te - estimates))
            MAE_sd <<-
              sd(abs(tau_te - estimates)) / sqrt(length(tau_te))
          },
          error = function(e) {
            print(e)
            warning(paste("Something went wrong with", setup_name))
            MSE    <<- NA
            MSE_sd <<- NA
            MAE    <<- NA
            MAE_sd <<- NA
          })

          Residuals <- data.frame(
            ntrain = ntrain,
            dim = dim,
            setup = setup_name,
            alpha = alpha,
            seed = seed,
            estimator = estimator_name,
            MSE = MSE,
            MSE_sd = MSE_sd,
            MAE = MAE,
            MAE_sd = MAE_sd
          )
          col.names <- !file.exists(filename)
          write.table(
            Residuals,
            file = filename,
            append = TRUE,
            col.names = col.names,
            row.names = FALSE,
            sep = ","
          )
          print(paste("   Done with", seed, dim, alpha, ntrain, estimator_name))
        }
      }
    }
  }
}
