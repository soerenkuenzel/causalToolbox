## Setting up what to loop over: This will be given to the file and we will
#execude it for i raning from 1 to 11 to go thorough all settings:
args <- commandArgs(TRUE)

if(length(args)==0){
    print("No arguments supplied.")
    setup_i = 1
    nthread = 4
}else{
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}


set.seed(1223)

library(hte)
library(ranger)
library(randomForest)
library(dplyr)
library(reshape)
library(MASS)

#################
# Configuration #
#################
setup_grid <-
  c(
    "randomNoise",
    "HighSignalToNoiseLinearModel",
    "LowSignalToNoiseLinearModel"
  )

setup <- setup_grid[[setup_i]]
print(setup)

dim_grid <- c(5, 10, 50)
ntrain_grid <- c(100, 316, 1000, 3162, 10000)
ntest <- 10000
seed_grid <- 1:20
alpha_grid <- c(0, 1)

estimator_grid <- list(
  "ranger" = function(x, y)
    ranger(y ~. , data=data.frame(x, y), num.threads=nthread),
  "randomForest" = function(x, y)
    randomForest(x, y),
  "honestRF" = function(x, y)
    honestRF(x, y, nthread=nthread)
)

predictor_grid <- list(
  "ranger" = function(estimator, x)
    predict(estimator, dat=x)$predictions,
  "randomForest" = function(estimator, x)
    predict(estimator, newdata=x),
  "honestRF" = function(estimator, x)
    predict(estimator, x)
)

## Setting up where the data should be saved:
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <- paste0(data_folder_name, "simulation_", setup, "_", Sys.Date(), ".csv")
if (dir.exists(filename))
  file.remove(filename)

source("RF_data_generator.R")

##############
# Simulation #
##############
## loop through all cases:
for (seed in seed_grid) {
  for (dim in dim_grid) {
    for (alpha in alpha_grid) {
      for (ntrain in ntrain_grid) {

        print(
          paste(
            "Starting with seed",
            seed,
            "of",
            max(seed_grid),
            "dim",
            dim,
            "of",
            paste(dim_grid, collapse = ", "),
            ntrain,
            "of",
            paste(ntrain_grid, collapse = ", ")
          )
        )

        # create training and test data: note that the test seed is constant:
        set.seed(seed)
        dt <- simulate_RF_experiment(
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
          predictor <- predictor_grid[[estimator_i]]
          estimator_name <- names(estimator_grid)[estimator_i]

          tryCatch({

            start_time <- Sys.time()
            L <- estimator(
              x = dt$trainX,
              y = dt$trainY
            )
            training_time <<- difftime(Sys.time(), start_time, units='secs')

            start_time <- Sys.time()
            estimates <- predictor(L, dt$testX)
            prediction_time <<- difftime(Sys.time(), start_time, units='secs')

            MSE    <<- mean((dt$testY - estimates) ^ 2)
            MSE_sd <<- sd((dt$testY - estimates) ^ 2) / sqrt(length(dt$testY))
            rm(L)
            gc()
          },
          error = function(e) {
            print(e)
            warning(paste("Something went wrong with", setup))
            training_time    <<- NA
            prediction_time  <<- NA
            MSE              <<- NA
            MSE_sd           <<- NA
          })

          Residuals <- data.frame(
            ntrain = ntrain,
            dim = dim,
            setup = setup,
            alpha = alpha,
            feat_distribution = "normal",
            testseed = 293901,
            trainingseed = seed,
            estimator = estimator_name,
            MSE = MSE,
            MSE_sd = MSE_sd,
            training_time = training_time,
            prediction_time = prediction_time
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
