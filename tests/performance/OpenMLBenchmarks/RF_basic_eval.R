options( java.parameters = "-Xmx1000g")
library("OpenML")
## temporarily set API key to read only key
setOMLConfig(apikey = "6e7606dcedb2a6810d88dfaa550f7f07", arff.reader = "RWeka")
#library("OpenML");setOMLConfig(apikey = "6e7606dcedb2a6810d88dfaa550f7f07", arff.reader = "farff")

# ------------------------------------------------------------------------------
library(dplyr)
library(ranger)
library(randomForest)
library(hte)

args <- commandArgs(TRUE)
seed <- -as.numeric(args[1])
learner <- as.character(args[2])
print(seed) #
print(learner)
# setup_i <- 1; learner = ranger

ntree = 500
mtry = function(features) max(round(ncol(features) / 3), 1)
nodesize = 5
replace = TRUE
sampsize = function(target) length(target)

estimator_trainer <- list(
  'ranger' = function(features, target) {
    ranger(
      y ~ .,
      data = data.frame(features, y = target),
      num.trees = ntree,
      mtry = mtry(features),
      min.node.size = nodesize,
      replace = replace,
      sample.fraction = 1
      )
  },
  'randomForest' = function(features, target) {
    randomForest(
      x = features,
      y = target,
      ntree = ntree,
      mtry = mtry(features),
      replace = replace,
      sampsize = sampsize(target),
      nodesize = nodesize
    )
  },
  'hte_adaptive_nomsp' = function(features, target) {
    honestRF(
      x = features,
      y = target,
      ntree = ntree,
      replace = replace,
      sampsize = sampsize(target),
      mtry = mtry(features),
      nodesizeSpl = nodesize,
      nodesizeAvg = nodesize,
      splitratio = 1,
      middleSplit = FALSE
    )
  },
  'hte_adaptive_wmsp' = function(features, target) {
    honestRF(
      x = features,
      y = target,
      ntree = ntree,
      replace = replace,
      sampsize = sampsize(target),
      mtry = mtry(features),
      nodesizeSpl = nodesize,
      nodesizeAvg = nodesize,
      splitratio = 1,
      middleSplit = TRUE
    )
  },
  'hte_honest_wmsp' = function(features, target) {
    honestRF(
      x = features,
      y = target,
      ntree = ntree,
      replace = replace,
      sampsize = sampsize(target),
      mtry = mtry(features),
      nodesizeSpl = nodesize,
      nodesizeAvg = nodesize,
      splitratio = .5,
      middleSplit = TRUE
    )
  }
)
estimator_predictor <- list(
  'ranger' = function(object, features_to_predict) {
    predict(object, features_to_predict)$predictions
  },
  'randomForest' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  },
  'hte_adaptive_nomsp' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  },
  'hte_adaptive_wmsp' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  },
  'hte_honest_wmsp' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  }
)

tasks = listOMLTasks(limit = 100000)
regression_tasks <-
  tasks[tasks$task.type == "Supervised Regression", ]
n_datasets <- nrow(regression_tasks)
regression_tasks <- regression_tasks[!duplicated(regression_tasks[ , c('target.feature', 'data.id')]), ]

regression_tasks <- regression_tasks[order(regression_tasks$number.of.instances),]

data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <-
  paste0(data_folder_name, "MSE_openML_basic_", learner, ".csv")


if(file.exists(filename)) {
  done_tasks <-
    unique(apply(read.csv(filename)[, c('task.id', 'seed')],
                 1,
                 function(x)
                   paste0(x, collapse = '')))
} else{
  done_tasks <- character()
}


for (i in 1:nrow(regression_tasks)) {
  #n_datasets) {
  # seed <- 123; i <- 1; learner = names(estimator_trainer)[1]
  # skip instances which have more than
  #if (regression_tasks$number.of.instances[i] > 100000)
  #  next

  print(paste('Starting with', i))

  data.id <- regression_tasks[i, "data.id"]
  task.id <- regression_tasks[i, "task.id"]


  if (paste0(task.id, seed)  %in% done_tasks) {
    print("This data set target combination ran before. We will skip it.")
    next
  }

  if (i == 1374)
    next
  # the read function sometimes fails. In that case run the next data set
  data_set <- tryCatch({
    getOMLDataSet(data.id = data.id)
  },
  error = function(e) {
    print(e)
    NA
  })
  if (is.na(data_set))
    next
  if (is.na(data_set$target.features[1]))
    next

  # Check if the file is already in the data set. If so, skip it and run the
  # next file

  done_tasks <- c(done_tasks, paste0(task.id, seed))



  non_missing_rows <-
    apply(!is.na(data_set$data), 1, all) # only take rows which
  # which don't have missing values

  features <-
    data_set$data[non_missing_rows, colnames(data_set$data) != data_set$target.features]
  target <- as.numeric(data_set$data[non_missing_rows,  colnames(data_set$data) == data_set$target.features])
  # split the data into training and test set
  n_smp <- length(target)
  set.seed(seed)
  idx_1 <- sample(1:n_smp, round(n_smp / 2))
  idx_2 <- (1:n_smp)[-idx_1]
  features_1 <- features[idx_1,]
  features_2 <- features[idx_2,]
  y_1 <- target[idx_1]
  y_2  <- target[idx_2]


  # for (learner in names(estimator_trainer))
  {
    train_time_diff <- NA
    predict_time_diff <- NA
    MSE_1 <- NA
    MSE_2 <- NA
    error <- ""
    tryCatch({
      # train the estimator
      evalWithTimeout(
        {
          train_time_start <- Sys.time()
          estimator_1 <- estimator_trainer[[learner]](features_1, y_1)
          estimator_2 <- estimator_trainer[[learner]](features_2, y_2)
          train_time_diff <- as.numeric(difftime(Sys.time(),
                                                 train_time_start,
                                                 tz,
                                                 units = "mins"))
          # evaluate the estimator
          predict_time_start <- Sys.time()
          MSE_1 <-
            mean((y_2 - estimator_predictor[[learner]](estimator_1, features_2)) ^
                   2)
          MSE_2 <-
            mean((y_1 - estimator_predictor[[learner]](estimator_2, features_1)) ^
                   2)
          predict_time_diff <- as.numeric(difftime(Sys.time(),
                                                   predict_time_start,
                                                   tz,
                                                   units = "mins"))
        },
        timeout = 1500, # stop if training takes more than 25 min
        onTimeout = "error")
    },
    error = function(e) {
      print(e)
      warning(paste("Something went wrong with", learner))
      error <- paste(e)
    })
    # save the results
    Residuals <- data.frame(
      seed = seed,
      data.id = data.id,
      task.id = task.id,
      estimator = learner,
      MSE_1 = MSE_1,
      MSE_2 = MSE_2,
      train_time = train_time_diff,
      predict_time = predict_time_diff,
      error = error,
      date = Sys.Date(),
      hte_version = packageVersion('hte'),
      ranger_version = packageVersion('ranger'),
      randomForests_version = packageVersion('randomForest')
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
      paste0(
        "      Done with i = ",
        i,
        " of ",
        n_datasets,
        ", estimator = ",
        learner,
        " -- it took ",
        round(train_time_diff + predict_time_diff, 2),
        " min",
        ", MSE = ",
        round(mean(MSE_1 + MSE_2), 2)
      )
    )
  }
    clearOMLCache()
    gc(verbose = getOption("verbose"), reset=FALSE)
}
