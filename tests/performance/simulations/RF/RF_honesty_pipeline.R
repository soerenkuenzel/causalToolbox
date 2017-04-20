## Setting up what to loop over: This will be given to the file and we will
#execude it for i raning from 1 to 11 to go thorough all settings:
args <- commandArgs(TRUE)

if(length(args)==0){
    print("No arguments supplied.")
    setup_i = 1
    nthread = 8
}else{
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}


set.seed(1223)

library(hte)
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

dim_grid <- c(5, 10, 15, 20)
ntrain_grid <- c(100, 316, 1000, 3162, 10000)
ntest <- 10000
seed_grid <- 1:200
alpha_grid <- c(0, 1, 5)
splitratio_grid <- c(0.2, 0.4, 0.6, 0.8, 1)

## Setting up where the data should be saved:
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <- paste0(data_folder_name, "honesty_", setup, "_", Sys.Date(), ".csv")
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
            ", dim",
            dim,
            "of",
            paste(dim_grid, collapse = ", "),
            ", ntrain",
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
        for (splitratio in splitratio_grid) {

          tryCatch({

            L <- honestRF(
              x = dt$trainX,
              y = dt$trainY,
              nthread=nthread,
              splitratio=splitratio
            )

            estimates <- predict(L, dt$testX)

            MSE    <<- mean((dt$testY - estimates) ^ 2)
            MSE_sd <<- sd((dt$testY - estimates) ^ 2) / sqrt(length(dt$testY))
            rm(L)
            gc()
          },
          error = function(e) {
            print(e)
            warning(paste("Something went wrong with", setup))
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
            splitratio = splitratio,
            MSE = MSE,
            MSE_sd = MSE_sd
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
          print(paste("   Done with", seed, dim, alpha, ntrain, splitratio))
        }

      }
    }
  }
}
