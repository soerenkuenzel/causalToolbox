##########################
# Example data generator #
##########################




#' @title simulate a RCT or observational data for causal effect estimation
#' @name simulate_correlation_matrix
#' @rdname simulate_correlation_matrix
#' @description This function uses the C-vine method for simulating correlation
#'   matrixes as in the paper "Generating random correlation matrices based on
#'   vines and extended onion method" by Daniel Lewandowskia, Dorota Kurowickaa,
#'   Harry Joe
#' @param dim Number of training examples.
#' @param alpha level of random correlation beteween the features. Choose 0 for
#'   independent featues. The bigger alpha the bigger the correlation. For the
#'   partial correlations the coefficients are created according to
#'   beta(1/alpha, 1/alpha)
#' @return A correlation matrix
#' @export simulate_correlation_matrix
simulate_correlation_matrix <- function(dim, alpha) {

  betaparam <- 1/ alpha

  P <-
    matrix(nrow = dim, ncol = dim)           # storing partial correlations
  S <- diag(dim)

  for (k in 1:(dim - 1)) {
    for (i in (k + 1):dim) {
      P[k, i] <- rbeta(1, betaparam, betaparam) # sampling from beta
      P[k, i] <- (P[k, i] - 0.5) * 2     # linearly shifting to [-1, 1]
      p <- P[k, i]
      if (k > 1) {
        for (l in (k - 1):1) {
          # converting partial correlation to raw correlation
          p <- p * sqrt((1 - P[l, i] ^ 2) * (1 - P[l, k] ^ 2)) + P[l, i] *
            P[l, k]
          # p  = p * sqrt((1-P(l,i)^2)*(1-P(l,k)^2)) + P(l,i)*P(l,k);
        }
      }
      S[k, i] <- p
      S[i, k] <- p
    }
  }

  # permuting the variables to make the distribution permutation-invariant
  permutation <- sample(1:dim)
  S <- S[permutation, permutation]
  return(S)
}







#' @title simulate a RCT or observational data for causal effect estimation
#' @name simulate_causal_experiment
#' @rdname simulate_causal_experiment
#' @description This function simulates a RCT or observational data for causal
#' effect estimation to test different heterogenuous treatment effect estimation
#' strategies.
#' @param ntrain Number of training examples.
#' @param ntest Number of test examples.
#' @param dim Dimension of the data set.
#' @param alpha Only used if given_features is not set and feat_distribution is
#' chosen to be normal. It specifies how correlated the features can be. If
#' alpha = 0, then the features are independent if alpha is very large, then the
#' features can be very correlated.
#' @param feat_distribution Only used if given_features is not specified. Either
#' "normal" or "unif". It specifies the distribution of the features.
#' @param given_features This is used, if we already have features and want to test
#' the performance of different estimators for a particular set of features.
#' @param setup This is used to specify the function form of the potential
#' outcomes and the treatment assignment. One of
#' RespSparseTau1strong, RsparseT2weak, complexTau,
#' Conf1, rare1, STMpp, Ufail, Usual1, Wager1, Wager2, Wager3.
#' @param testseed is the seed used to generate the testing data, if NULL, then
#' the seed of the main session is used.
#' @param trainseed is the seed used to generate the training data, if NULL,
#' then the seed of the main session is used.
#' @return A list of the transformed object `x`, and encoding information
#' `labels`.
#' @export simulate_causal_experiment
simulate_causal_experiment <- function(ntrain,
                                       ntest,
                                       dim = ncol(given_features),
                                       alpha = .1,
                                       feat_distribution = "normal",
                                       given_features = NULL,
                                       setup = "RespSparseTau1strong",
                                       testseed = NULL,
                                       trainseed = NULL) {

  ## First we define a base function which will later be called with different
  # setups:
  # the following function is the base function when creating the features.
  # the different setups below specify the specific form of the potential
  # outcomes and the treatment assignment and then call this function
  createTrainAndTest_base <-
    function(ntrain,
             ntest,
             dim,
             m_t_truth,
             m_c_truth,
             propscore,
             alpha,
             feat_distribution,
             given_features) {
      # this is the base creator, which is called by all other setups:
      tau <- function(feat) {
        m_t_truth(feat) - m_c_truth(feat)
      }
      if (is.null(given_features)) {
        given_features_fkt <- function(n, dim) {
          if (feat_distribution == "normal") {
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
      } else{
        given_features_fkt <- function(n, dim) {
          colnames(given_features) <- paste0("x", 1:(ncol(given_features)))
          return(given_features[1:n, ])
        }
      }

      getW <- function(feat) {
        nn <- nrow(feat)
        propS <- propscore(feat)
        return(rbinom(nn, 1, propS))
      }
      getYobs <- function(feat, W) {
        nn <- nrow(feat)
        return(ifelse(
          W == 1,
          m_t_truth(feat) + rnorm(nn, 0, 1),
          # treated
          m_c_truth(feat) + rnorm(nn, 0, 1)   # control
        ))
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
      feat_tr_numeric <- as.data.frame(sapply(feat_tr, function(x) {
        if (is.numeric(x)) {
          return(x)
        } else{
          as.numeric(as.factor(x))
        }
      }))

      W_tr <- getW(feat_tr_numeric)
      Yobs_tr <- getYobs(feat_tr_numeric, W_tr)
      if (!is.null(trainseed)) {
        .Random.seed <- current_seed  # sets back the current random stage
      }
      if (!is.null(testseed)) {
        current_seed <- .Random.seed  # saves the current random stage
        set.seed(testseed)          # introduces a new seed to stay consistent
      }
      feat_te <- given_features_fkt(ntest, dim)
      feat_te_numeric <- as.data.frame(sapply(feat_te, function(x) {
        if (is.numeric(x)) {
          return(x)
        } else{
          as.numeric(as.factor(x))
        }
      }))
      W_te <- getW(feat_te_numeric)
      Yobs_te <- getYobs(feat_te_numeric, W_te)
      if (!is.null(testseed)) {
        .Random.seed <- current_seed  # sets back the current random stage
      }
      return(
        list(
          alpha = alpha,
          feat_te = feat_te,
          W_te = W_te,
          tau_te = tau(feat_te_numeric),
          Yobs_te = Yobs_te,
          feat_tr = feat_tr,
          W_tr = W_tr,
          tau_tr = tau(feat_tr_numeric),
          Yobs_tr = Yobs_tr
        )
      )
    }

  ## Now we introduce different setups:
  # 1.) RespSparseTau1strong:
  if (setup == "RespSparseTau1strong") {
    if (dim < 3)
      stop("For RespSparseTau1strong the dimension must be at least 3")

    m_t_truth <-
      function(feat)
        3 * feat$x1 + 5 * feat$x2  + 30 * feat$x3 # mu^t
    m_c_truth <-
      function(feat)
        3 * feat$x1 + 5 * feat$x2               # mu^c
    propscore <-
      function(feat)
        .1                                      # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 2.) Response is sparse; Tau has only two weak predictor:
  if (setup == "RsparseT2weak") {
    if (dim < 3)
      stop("For RsparseT2weak the dimension must be at least 3")

    m_t_truth <-
      function(feat)
        3 * feat$x1 + 3 * feat$x2  + 4 * feat$x3 # mu^t
    m_c_truth <-
      function(feat)
        3 * feat$x1 + 5 * feat$x2               # mu^c
    propscore <-
      function(feat)
        .1                                      # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 3.) complex tau : control and treated group have nothing in common
  if (setup == "complexTau") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(1421)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, 1, 30)
    beatt_raw <- runif(dim, 1, 30)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_t_truth <- function(feat) {
      betac_trunc <- beatc_raw[1:ncol(feat)]
      as.matrix(feat) %*% betac_trunc                 # mu^t
    }
    m_c_truth <- function(feat) {
      beatt_trunc <- beatt_raw[1:ncol(feat)]
      as.matrix(feat) %*% beatt_trunc                  # mu^t
    }
    propscore <-
      function(feat)
        .5                    # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 4.) treatment effect and treatment assignment are dependent
  if (setup == "Conf1") {
    m_t_truth <- function(feat) {
      2 * feat$x1 - 100 * feat$x2      # mu^t
    }
    m_c_truth <- function(feat) {
      2 * feat$x1 + 2 * feat$x2      # mu^c
    }
    propscore <-
      function(feat)
        max(0.05, min(.95, feat$x1 / 2 + 1 / 4))             # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 5.) few treated
  if (setup == "rare1") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(1444)                # introduces a new seed to stay consistent
    beat_raw <- runif(dim, 1, 5)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_t_truth <- function(feat) {
      beta_m <- beat_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m  + ifelse(feat$x1 > .5, 5, 0) + 8  # mu^t
    }
    m_c_truth <- function(feat) {
      beta_m <- beat_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m  + ifelse(feat$x1 > .5, 5, 0)  # mu^c
    }
    propscore <-
      function(feat)
        .01                    # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 6.) no treatment effect
  if (setup == "STMpp") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(112)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, 1, 30)
    beatt_raw <- runif(dim, 1, 30)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_t_truth <- function(feat) {
      beta_m <- beatc_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m                      # mu^t
    }
    m_c_truth <- function(feat) {
      beta_m <- beatc_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m                      # mu^t
    }
    propscore <-
      function(feat)
        .5                    # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 7.) Ufail
  if (setup == "Ufail") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    if(dim < 6) stop("For Ufail the dimension must be at least 6.")

    beat_raw <- (1:5 - 3)

    m_t_truth <- function(feat) {
      dim <- ncol(feat)
      beta_m <- c(beat_raw, rep(0, dim - 5))
      as.matrix(feat) %*% beta_m                  # mu^t
    }
    m_c_truth <- function(feat) {
      dim <- ncol(feat)
      beta_m <- rep(0, dim)
      as.matrix(feat) %*% beta_m                  # mu^c
    }
    propscore <-
      function(feat)
        .5                    # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 8.) Usual 1
  if (setup == "Usual1") {
    if (dim < 3)
      stop("For Usual1 the dimension must be at least 3")

    m_t_truth <-
      function(feat)
        3 * feat$x1 + 4 * feat$x2  + 30 * feat$x3  # mu^t
    m_c_truth <-
      function(feat)
        3 * feat$x1 + 5 * feat$x2               # mu^c
    propscore <-
      function(feat)
        .5                                     # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 9.) Wager1
  if (setup == "Wager1") {
    if (dim < 3)
      stop("For Wager1 the dimension must be at least 3")

    m_t_truth <-
      function(feat)
        2 * (feat$x1  - 0.5)  # mu^t
    m_c_truth <-
      function(feat)
        2 * (feat$x1  - 0.5)  # mu^c
    propscore <-
      function(feat)
        0.25 + dbeta(feat$x1, 2, 4) / 4  # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 10.) Wager2
  if (setup == "Wager2") {
    if (dim < 2)
      stop("For Wager2 the dimension must be at least 2")
    effect <- function(feat) {
      (1 + 1 / (1 + exp(-20 * (feat$x1 - 1 / 3)))) *
        (1 + 1 / (1 + exp(-20 * (feat$x2 - 1 / 3))))
    }
    m_t_truth <-
      function(feat)
        1 / 2 * effect(feat) # mu^t
    m_c_truth <-
      function(feat)
        - 1 / 2 * effect(feat) # mu^c
    propscore <-
      function(feat)
        .5                                      # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }

  # 11.) Wager 3
  if (setup == "Wager3") {
    if (dim < 2)
      stop("For Wager3 the dimension must be at least 2")

    effect <- function(feat) {
      4 / ((1 + exp(-12 * (feat$x1 - 0.5))) *
             (1 + exp(-12 * (feat$x2 - 0.5))))
    }

    m_t_truth <-
      function(feat)
        1 / 2 * effect(feat)  # mu^t
    m_c_truth <-
      function(feat)
        - 1 / 2 * effect(feat) # mu^c
    propscore <-
      function(feat)
        .5 # propensity score

    return(c(
      list(
        setup_name = setup,
        m_t_truth = m_t_truth,
        m_c_truth = m_c_truth,
        propscore = propscore
      ),
      createTrainAndTest_base(
        ntrain,
        ntest,
        dim,
        m_t_truth,
        m_c_truth,
        propscore,
        alpha,
        feat_distribution,
        given_features
      )
    ))
  }
  ## If nothing was returned by now, then something went wrong and we want to
  # throw an error:
  stop(
    "setup must be one of RespSparseTau1strong, RsparseT2weak, complexTau,
    Conf1, rare1, STMpp, Ufail, Usual1, Wager1, Wager2, Wager3"
  )
}
