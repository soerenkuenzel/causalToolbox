library(hte)

args <- commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied.")
  setup_i = 1
  nthread = 8
  ntrain = 10000
  dim = 20
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

print(setup_i)

#################
# Configuration #
#################
setup_grid <-
  c(
    "randomNoise",
    "HighSignalToNoiseLinearModel",
    "LowSignalToNoiseLinearModel"
  )

ntest = 10000
alpha = 0.1

setup <- setup_grid[[setup_i]]
print(setup)

set.seed(59906)

## Setting up where the data should be saved:
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <-
  paste0(data_folder_name, "autotune_", setup, "_", ntrain, "_", dim, "_", Sys.Date(), ".csv")
if (file.exists(filename))
  file.remove(filename)

source("RF_data_generator.R")

dt <- simulate_RF_experiment(
  ntrain = ntrain,
  ntest = ntest,
  dim = dim,
  alpha = alpha,
  feat_distribution = "normal",
  setup = setup,
  testseed = 293901,
  trainseed = 93007
)

total_seed = 50

Rand_tune <- data.frame(
  setup = setup,
  dim = dim,
  ntrain = ntrain,
  seed = 1:total_seed
)


for (i in 1:total_seed) {
  print(paste("Starting with", i, "of", total_seed))

  stat.time <- Sys.time()

  L <- tryCatch({

    L <- autohonestRF(
      dt$trainX, dt$trainY, num_iter=10000, eta=2,
      verbose=TRUE, seed=i,
      nthread=nthread
    )

  },
  error = function(e) {
    print(e)
    NULL
  })

  if (!is.null(L)) {
    MSE <- mean((dt$testY - predict(L, dt$testX)) ^ 2)
    Rand_tune[i, "MSE"] <- MSE
    Rand_tune[i, "mtry"] <- L@mtry
    Rand_tune[i, "ntree"] <- L@ntree
    Rand_tune[i, "replace"] <- L@replace
    Rand_tune[i, "sampsize"] <- L@sampsize
    Rand_tune[i, "nodesizeSpl"] <- L@nodesizeSpl
    Rand_tune[i, "nodesizeAvg"] <- L@nodesizeAvg
    Rand_tune[i, "splitratio"] <- L@splitratio
    Rand_tune[i, "middleSplit"] <- L@middleSplit
    print(paste(
      "   Done with",
      setup,
      "Error was:",
      MSE,
      "   and it took",
      as.numeric(Sys.time() - stat.time, units = "mins"),
      " minutes."
    ))

    col.names <- !file.exists(filename)
    write.table(
      Rand_tune[i, ],
      file = filename,
      append = TRUE,
      col.names = col.names,
      row.names = FALSE,
      sep = ","
    )
  }


}
