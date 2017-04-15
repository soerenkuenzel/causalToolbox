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

dim_grid <- c(5, 10, 20, 50)
ntrain_grid <- c(100, 316, 1000, 3162, 10000, 31622, 100000)
ntest <- 5000
seed_grid <- 1:500
alpha_grid <- c(0, .3, 1, 2)

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


#############################
# Simulation Data Generator #
#############################
#' @title simulate a random dataset for RF simulation
#' @name simulate_RF_experiment
#' @rdname simulate_RF_experiment
#' @description This function simulates observational dataset from linear models
#' to test compare different estimators.
#' @param ntrain Number of training examples.
#' @param ntest Number of test examples.
#' @param dim Dimension of the data set.
#' @param alpha Only used if given_features is not set and feat_distribution is
#' chosen to be normal. It specifies how correlated the features can be. If
#' alpha = 0, then the features are independent if alpha is very large, then the
#' features can be very correlated.
#' @param feat_distribution Only used if given_features is not specified. Either
#' "normal" or "unif". It specifies the distribution of the features.
#' @param setup This is used to specify the function form of the potential
#' outcomes and the treatment assignment. One of
#' RespSparseTau1strong, RsparseT2weak, complexTau,
#' Conf1, rare1, STMpp, Ufail, Usual1, Wager1, Wager2, Wager3.
#' @param testseed is the seed used to generate the testing data, if NULL, then
#' the seed of the main session is used.
#' @param trainseed is the seed used to generate the training data, if NULL,
#' then the seed of the main session is used.
#' @return A list of `trainX`, `trainY`, `testX` and `testY`.
#' @export simulate_RF_experiment
simulate_RF_experiment <- function(
  ntrain,
  ntest,
  dim,
  alpha = .1,
  feat_distribution = "normal",
  setup = "randomNoise",
  testseed = NULL,
  trainseed = NULL
) {

  ## First we define a base function which will later be called with different
  # setups:
  # the following function is the base function when creating the features.
  # the different setups below specify the specific form of the potential
  # outcomes and the treatment assignment and then call this function
  createTrainAndTest_base <- function(ntrain, ntest, dim, alpha,
                                      feat_distribution, truth_function) {

    given_features_fkt <- function(n, dim) {
      if (feat_distribution == "normal") {
        Sigma <- matrix(
          runif(dim ^ 2, 0, alpha),
          nrow = dim,
          ncol = dim
        )
        diag(Sigma) <- 1
        correlation_mat <- simulate_correlation_matrix(dim, alpha = alpha)
        mu <- rep(0, dim)
        #' @import MASS
        feat <-
          data.frame(mvrnorm(
            n = n,
            mu = mu,
            Sigma = correlation_mat
          ))
      }
      if (feat_distribution == "unif") {
        feat <- data.frame(matrix(runif(n * dim), nrow = n))
      }
      colnames(feat) <- paste0("x", 1:(dim))
      return(feat)
    }

    getYobs <- function(feat) {
      nn <- nrow(feat)
      return(truth_function(feat) + rnorm(nn, 0, 1))
    }

    ## fixing the training and testing seed (if given) without changing the
    # seed of the seesion:
    # first save the current random seed, then set the seed to what we like,
    # and then set the seed back to what it was.

    if (!is.null(trainseed)) {
      current_seed <- .Random.seed  # saves the current random stage
      set.seed(trainseed)           # introduces a new seed to stay consistent
    }

    feat_tr <- given_features_fkt(ntrain, dim)
    Yobs_tr <- getYobs(feat_tr)

    if (!is.null(trainseed)) {
      .Random.seed <- current_seed  # sets back the current random stage
    }

    if (!is.null(testseed)) {
      current_seed <- .Random.seed  # saves the current random stage
      set.seed(testseed)          # introduces a new seed to stay consistent
    }

    feat_te <- given_features_fkt(ntest, dim)
    Yobs_te <- getYobs(feat_te)

    if (!is.null(testseed)) {
      .Random.seed <- current_seed  # sets back the current random stage
    }

    return(
      list(
        testX = feat_te,
        testY = Yobs_te,
        trainX = feat_tr,
        trainY = Yobs_tr
      )
    )
  }

  if (setup == "randomNoise") {
    m_truth <-
      function(feat)
        0

    return(
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        alpha,
        feat_distribution,
        m_truth
      )
    )
  }

  if (setup == "HighSignalToNoiseLinearModel") {
    m_truth <-
      function(feat)
        50 * feat$x1 + 20 * feat$x2  + 30 * feat$x3

    return(
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        alpha,
        feat_distribution,
        m_truth
      )
    )
  }

  if (setup == "LowSignalToNoiseLinearModel") {
    m_truth <-
      function(feat)
        0.5 * feat$x1 + 0.2 * feat$x2  + 0.3 * feat$x3

    return(
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        alpha,
        feat_distribution,
        m_truth
      )
    )
  }

  ## If nothing was returned by now, then something went wrong and we want to
  # throw an error:
  stop(
    "setup does not exist"
  )
}

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
