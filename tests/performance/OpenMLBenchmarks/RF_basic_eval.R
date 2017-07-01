library("OpenML")
## temporarily set API key to read only key
setOMLConfig(apikey = "6e7606dcedb2a6810d88dfaa550f7f07") # https://www.openml.org/u/3454#!api

# ------------------------------------------------------------------------------
library(dplyr)
library(ranger)
library(randomForest)
library(hte)

stop("Fix all parameters of all the learners !")

estimator_trainer <- list(
  'ranger' = function(features, target) {
    ranger(y ~ ., data = data.frame(features, y = target))
  },
  'randomForest' = function(features, target) {
    randomForest(x = features, y = target)
  },
  'hte_adaptive' = function(features, target) {
    honestRF(x = features, y = target)
  },
  'hte_honest' = function(features, target) {
    honestRF(x = features, y = target)
  }
)
estimator_predictor <- list(
  'ranger' = function(object, features_to_predict) {
    predict(object, features_to_predict)$predictions
  },
  'randomForest' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  },
  'hte_adaptive' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  },
  'hte_honest' = function(object, features_to_predict) {
    predict(object, features_to_predict)
  }
)

seed_list <- c(8543478, 7684240, 7742039, 5772717, 5198406, 841135, 3271857,
               2334580, 3652130, 3227302)

tasks = listOMLTasks()
regression_tasks <-
  tasks[tasks$task.type == "Supervised Regression", ]
n_datasets <- nrow(regression_tasks)

data_folder_name <- "tests/performance/OpenMLBenchmarks/sim_data/"
if (!dir.exists(data_folder_name))
  dir.create(data_folder_name)
filename <-
  paste0(data_folder_name, "MSE_openML_basic.csv")


for (seed in seed_list) {
  for (i in 1:5) { #n_datasets) {
    # seed <- 123; i <- 1; learner = names(estimator_trainer)[1]
    # skip instances which have more than
    if (regression_tasks$number.of.instances[i] > 100000)
      next

    data.id <- regression_tasks[i, "data.id"]
    data_set <- getOMLDataSet(data.id = data.id)

    features <-
      data_set$data[,  colnames(data_set$data) != data_set$target.features]
    target <-
      data_set$data[,  colnames(data_set$data) == data_set$target.features]

    # split the data into training and test set
    n_smp <- length(target)
    set.seed(seed)
    idx_1 <- sample(1:n_smp, round(n_smp / 2))
    idx_2 <- (1:n_smp)[-idx_1]
    features_1 <- features[idx_1, ]
    features_2 <- features[idx_2, ]
    y_1 <- target[idx_1]
    y_2  <- target[idx_2]


    for (learner in names(estimator_trainer)) {
      set.seed(seed + 1)
      # train the estimator
      train_time_start <- Sys.time()
      estimator_1 <- estimator_trainer[[learner]](features_1, y_1)
      estimator_2 <- estimator_trainer[[learner]](features_2, y_2)
      train_time_diff <- as.numeric(difftime(Sys.time(),
                                             train_time_start,
                                             tz,
                                             units = "mins"))
      # evaluate the estimator
      predict_time_start <- Sys.time()
      MSE_1 <- mean((y_2 - estimator_predictor[[learner]](estimator_1, features_2))^2)
      MSE_2 <- mean((y_1 - estimator_predictor[[learner]](estimator_2, features_1))^2)
      predict_time_diff <- as.numeric(difftime(Sys.time(),
                                               predict_time_start,
                                               tz,
                                               units = "mins"))
      # save the results
      Residuals <- data.frame(
        seed = seed,
        data.id = data.id,
        estimator = learner,
        MSE_1 = MSE_1,
        MSE_2 = MSE_2,
        train_time = train_time_diff,
        predict_time = predict_time_diff,
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
          " of ", n_datasets, ", estimator = ",
          learner,
          " -- it took ",
          round(train_time_diff + predict_time_diff, 2),
          " min",
          ", MSE = ", round(mean(MSE_1 + MSE_2), 2)
        )
      )
    }
  }
}
