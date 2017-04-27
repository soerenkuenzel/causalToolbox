# File goes through dim_grid, ntrain_grid, alpha_grid, seed_grid, setup_grid it
# trains each estimator and computes the MSE, and their SD

## Setting up what to loop over: This will be given to the file and we will
#execude it for i raning from 1 to 11 to go thorough all settings:

args <- commandArgs(TRUE)
setup_i <- -as.numeric(args[1])
print(setup_i) #
# setup_i <- 1

set.seed(1145)
nthread = 8

library(CATEestimators)
library(hte)
library(dplyr)
library(reshape)
#library(causalForest)

setup_grid <-
  c(
    "RespSparseTau1strong",
    "RsparseT2weak",
    "complexTau",
    "Conf1",
    "rare1",
    "STMpp",
    "Ufail",
    "Usual1",
    "Wager1",
    "Wager2",
    "Wager3"
  )

setup <- setup_grid[[setup_i]]
print(setup)

dim_grid <- c(5, 20, 100)
ntrain_grid <- round(10 ^ seq(from = 2, to = 4, by = .5))
# if (setup == "rare1"){
#   ntrain_grid <- c(1500, 2000, 4000, 10000, 40000)
# }
ntest <- 10000
seed_grid <- 1:100
alpha_grid <- c(0, .1)

estimator_grid <- list(
  "S_RF" = function(feat, W, Yobs)
    S_RF(feat, W, Yobs, nthread = nthread),
  "T_RF" = function(feat, W, Yobs)
    T_RF(feat, W, Yobs, nthread = nthread),
  "X_RF" = function(feat, W, Yobs)
    X_RF(feat, W, Yobs, verbose = FALSE, nthread = nthread),
  "X_RF_autotune_hyperband" = function(feat, W, Yobs)
    X_RF_autotune_hyperband(
      feat = feat,
      tr = W,
      yobs = Yobs,
      num_iter = 3 ^ 8,
      verbose = FALSE
    ),
  "X_RF_autotune_simple" = function(feat, W, Yobs)
    X_RF_autotune_simple(
      feat = feat,
      tr = W,
      yobs = Yobs,
      verbose = FALSE
    ),
  #  "X_RF_autotune_gpp" = function(feat, W, Yobs)
  #    X_RF_autotune_gpp(
  #      feat = feat,
  #      tr = W,
  #      yobs = Yobs,
  #      init_points = 20,
  #      n_iter = 20,
  #      verbose = FALSE
  #    ),
  "S_BART" = function(feat, W, Yobs)
    S_BART(feat, W, Yobs),
  "T_BART" = function(feat, W, Yobs)
    T_BART(feat, W, Yobs),
  "X_BART" = function(feat, W, Yobs)
    X_BART(feat, W, Yobs)
  # "CF_p" = function(feat, W, Yobs) {
  #   feat <- as.matrix(feat)
  #   colnames(feat) <- NULL
  #   propensityForest(
  #     X = feat,
  #     W = W,
  #     Y = Yobs,
  #     num.trees = 500,
  #     sample.size = nrow(feat) / 10,
  #     nodesize = 1
  #   )
  # },
  # "CF" = function(feat, W, Yobs) {
  #   feat <- as.matrix(feat)
  #   colnames(feat) <- NULL
  #   causalForest(
  #     X = feat,
  #     W = W,
  #     Y = Yobs,
  #     num.trees = 500,
  #     sample.size = nrow(feat) / 10,
  #     nodesize = 1
  #   )
  # }
)

CATEpredictor_grid <- list(
  "S_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "T_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "X_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "X_RF_autotune_hyperband" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "X_RF_autotune_simple" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  # "X_RF_autotune_gpp" = function(estimator, feat_te)
  #   EstimateCate(estimator, feat_te),
  "S_BART" = function(estimator, feat_te)
    CATEestimators::EstimateCate(estimator, feat_te),
  "T_BART" = function(estimator, feat_te)
    CATEestimators::EstimateCate(estimator, feat_te),
  "X_BART" = function(estimator, feat_te)
    CATEestimators::EstimateCate(estimator, feat_te)
  # "CF_p" = function(estimator, feat_te) {
  #   feat_te <- as.matrix(feat_te)
  #   colnames(feat_te) <- NULL
  #   return(predict(estimator, feat_te))
  # },
  # "CF" = function(estimator, feat_te)  {
  #   feat_te <- as.matrix(feat_te)
  #   colnames(feat_te) <- NULL
  #   return(predict(estimator, feat_te))
  # }
)


## Setting up where the data should be saved:
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <-
  paste0(data_folder_name, "MSE_rates_", setup, "_", Sys.Date(), ".csv")
if (file.exists(filename))
  file.remove(filename)


## loop through all cases:
for (seed in seed_grid) {
  for (dim in dim_grid) {
    for (alpha in alpha_grid) {
      print(paste(
        "Starting with seed = ",
        seed,
        "of",
        max(seed_grid),
        "dim = ",
        dim,
        "of",
        paste(dim_grid, collapse = ", ")
      ))
      for (ntrain in ntrain_grid) {
        # create training and test data: note that the test seed is constant:
        set.seed(seed)
        dt <- simulate_causal_experiment(
          ntrain = ntrain,
          ntest = ntest,
          dim = dim,
          alpha = alpha,
          feat_distribution = "normal",
          setup = setup,
          testseed = 293901,
          trainseed = seed
        )

        # go through all estimators we want to compare:
        for (estimator_i in 1:length(estimator_grid)) {
          estimator <- estimator_grid[[estimator_i]]
          CATEpredictor <- CATEpredictor_grid[[estimator_i]]
          estimator_name <- names(estimator_grid)[estimator_i]

          start_time <- Sys.time()

          tryCatch({
            L <- estimator(
              feat = dt$feat_tr,
              W = dt$W_tr,
              Yobs = dt$Yobs_tr
            )
            estimates <- CATEpredictor(L, dt$feat_te)
            MSE    <<- mean((dt$tau_te - estimates) ^ 2)
            MSE_sd <<-
              sd((dt$tau_te - estimates) ^ 2) / sqrt(length(dt$tau_te))
            MAE    <<- mean(abs(dt$tau_te - estimates))
            MAE_sd <<-
              sd(abs(dt$tau_te - estimates)) / sqrt(length(dt$tau_te))
          },
          error = function(e) {
            print(e)
            warning(paste("Something went wrong with", setup))
            MSE    <<- NA
            MSE_sd <<- NA
            MAE    <<- NA
            MAE_sd <<- NA
          })

          min_taken <-
            as.numeric(difftime(Sys.time(), start_time, tz, units = "mins"))

          Residuals <- data.frame(
            ntrain = ntrain,
            dim = dim,
            setup = setup,
            alpha = alpha,
            feat_distribution = "normal",
            testseed = 293901,
            trainingseed = seed,
            estimator = paste0(estimator_name, packageVersion("hte")),
            MSE = MSE,
            MSE_sd = MSE_sd,
            MAE = MAE,
            MAE_sd = MAE_sd,
            timetaken = min_taken
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
          print(
            paste(
              "      Done with ntrain = ",
              ntrain,
              ", estimator = ",
              estimator_name,
              "  -- it took ",
              round(min_taken, 2),
              " min"
            )
          )
        }
      }
    }
  }
}
