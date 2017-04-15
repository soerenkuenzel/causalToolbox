library(hte)

args <- commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied.")
  setup_i = 1
  nthread = 4
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

nsamples = 100000
ntest = 10000
alpha = 0.1

setup <- setup_grid[[setup_i]]
print(setup)

set.seed(59906)

# Sample each parameter
Rand_tune <- data.frame(
  setup = setup,
  dim = dim,
  ntrain = ntrain,
  mtry = sample(1:dim, nsamples, replace = TRUE),
  min_node_size_spl = sample(c(1, 3, 5, 10, 30, 100), nsamples, replace = TRUE),
  min_node_size_ave = sample(c(1, 3, 5, 10, 30, 100), nsamples, replace = TRUE),
  splitratio = sample(seq(from = 0.1, to = 1, by = 0.1), nsamples, replace = TRUE),
  replace = sample(c(TRUE, FALSE), nsamples, replace = TRUE),
  sample_fraction = sample(seq(from = 0.1, to = 1, by = 0.1), nsamples,
                                 replace = TRUE)
)


## Setting up where the data should be saved:
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <-
  paste0(data_folder_name, "tuning_", setup, "_", ntrain, "_", dim, "_", Sys.Date(), ".csv")
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


# loop through rows and fill in the NA columns:
for (i in 1:nsamples) {
  print(paste("Starting with", i, "of", nsamples))
  stat.time <- Sys.time()
  # run the experiment:
  set.seed(78136)
  # train the RF:
  MSE <-
    tryCatch({
      L <- honestRF(
        x = dt$trainX,
        y = dt$trainY,
        ntree = 500,
        mtry = Rand_tune$mtry[i],
        nodesizeSpl = Rand_tune$min_node_size_spl[i],
        nodesizeAvg = Rand_tune$min_node_size_ave[i],
        splitratio = Rand_tune$splitratio[i],
        replace = Rand_tune$replace[i],
        sampsize = Rand_tune$sample_fraction[i] * ntrain,
        nthread = nthread
      )
      mean((dt$testY - predict(L, dt$testX)) ^ 2)
    },
    error = function(e) {
      print(e)
      NA
    })

  Rand_tune[i, setup] <- MSE
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
