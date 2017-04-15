library(dplyr)

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
