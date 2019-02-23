# This file implements different heterogeneous treatment effect estimation
# problems

# Correlation Matrix Simulator -------------------------------------------------
#' @title Simulate a Correlation Matrix
#' @name simulate_correlation_matrix
#' @description This function uses the C-vine method for simulating correlation
#'   matrixes. Refer to the referenced paper for details.
#' @param dim dimension of the correlation matrix.
#' @param alpha level of random correlation beteween the features. Choose 0 for
#'   independent featues. The bigger alpha the bigger the correlation. For the
#'   partial correlations the coefficients are created according to
#'   beta(1/alpha, 1/alpha)
#' @return A correlation matrix
#' @references \itemize{\item Daniel Lewandowskia, Dorota Kurowickaa, Harry Joe
#'   (2009). Generating random correlation matrices based on vines and extended
#'   onion method.}
simulate_correlation_matrix <- function(dim, alpha) {

  betaparam <- 1 / alpha

  P <- matrix(nrow = dim, ncol = dim)           # storing partial correlations
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


# Causal Experiment Simulator --------------------------------------------------
#' @title Simulate a Causal Experiment
#' @description This function simulates a RCT or observational data for causal
#'   effect estimation. It is mainly used to test different heterogenuous
#'   treatment effect estimation strategies.
#' @param ntrain Number of training examples.
#' @param ntest Number of test examples.
#' @param dim Dimension of the data set.
#' @param alpha Only used if given_features is not set and feat_distribution is
#'   chosen to be normal. It specifies how correlated the features can be. If
#'   alpha = 0, then the features are independent if alpha is very large, then
#'   the features can be very correlated. Use the
#'   \code{simulate_correlation_matrix} function to get a better understanding
#'   of the impact of alpha.
#' @param feat_distribution Only used if given_features is not specified. Either
#'   "normal" or "unif". It specifies the distribution of the features.
#' @param given_features This is used, if we already have features and want to
#'   test the performance of different estimators for a particular set of
#'   features.
#' @param setup This is used to specify the function form of the response
#'   functions and the treatment assignment. Refer to the details for a complete
#'   description of the different choices.
#' @param testseed is the seed used to generate the testing data, if NULL, then
#'   the seed of the main session is used.
#' @param trainseed is the seed used to generate the training data, if NULL,
#'   then the seed of the main session is used.
#' @param setupseed For some of the setups, betas are created randomly. This
#'   seed is per default chosen, but can be changed here to generate more
#'   setups.
#' @return A list with the following elements 
#'   \item{\code{setup_name}}{Name of the setup} 
#'   \item{\code{m_t_truth}}{Function contraining the response function of the
#'   treated units}
#'   \item{\code{m_c_truth}}{Function contraining the response function of the
#'   control units}
#'   \item{\code{propscore}}{Propensity score function} 
#'   \item{\code{alpha}}{Chosen alpha} 
#'   \item{\code{feat_te}}{Data.frame containing the features of the test
#'   samples}
#'   \item{\code{W_te}}{Numeric vector containing the treatment assignment of
#'   the test samples}
#'   \item{\code{tau_te}}{Numeric vector containing the true conditional average
#'   treatment effects of the test samples}
#'   \item{\code{Yobs_te}}{Numeric vector containing the observed Y values of
#'   the test samples}
#'   \item{\code{feat_tr}}{Data.frame containing the features of the training 
#'   samples}
#'   \item{\code{W_tr}}{Numeric vector containing the treatment assignment of
#'   the training samples}
#'   \item{\code{tau_tr}}{Numeric vector containing the true conditional average
#'   treatment effects of the training samples}
#'   \item{\code{Yobs_tr}}{Numeric vector containing the observed Y values of
#'   the training samples}
#' @details The function simulates causal experiments by generating the
#'   features, the treatment assignment, the observed Y values, and the CATE for
#'   a test set and a training set. The different setups define the response
#'   functions and the propensity score. The following options are implemented:
#'   \code{RespSparseTau1strong, RsparseT2weak, complexTau, complexTau2,
#'   complexTau, complexTau2, complexTau3, complexTau4, Conf1, rare1, rare2,
#'   rare3, STMpp, STMpp2, STMpp3, STMpp4, Ufail, Usual1, WA1, WA2, WA3, WA4}
#'   See the example code to find the exact definition of \eqn{\mu_0, \mu_1},
#'   and \eqn{e}. We also give the following examples:
#'   \itemize{
#'      \item \code{RsparseT2weak} 
#'        \itemize{
#'        \item \eqn{\mu_0(x) = 3 x_1 + 5 x_2}, 
#'        \item \eqn{\mu_1(x) = 3 x_1 + 3 x_2  + 4 x_3}, 
#'        \item \eqn{e(x) = 0.1.}}
#'      \item \code{complexTau}
#'      
#'        Create \eqn{\beta_0, \beta_1 \epsilon R^dim} independently from a
#'        Unif[1,30]
#'        \itemize{
#'        \item \eqn{\mu_0(x) = x^T \beta_0}, 
#'        \item \eqn{\mu_1(x) = x^T \beta_1}, 
#'        \item \eqn{e(x) = 0.5}}
#'      \item \code{Conf1} 
#'        \itemize{
#'        \item \eqn{\mu_0(x) =   2 x_1 + 2 x_2}, 
#'        \item \eqn{\mu_1(x) =   2 x_1 + 2  x_2}, 
#'        \item \eqn{e(x) = max(0.05, min(.95, x_1 / 2 + 1 / 4)).}}
#'      \item \code{RespSparseTau1strong} 
#'        \itemize{
#'        \item \eqn{\mu_0(x) = 3  x_1 + 5  x_2   }, 
#'        \item \eqn{\mu_1(x) = 3 x_1 + 5 x_2  + 30 x_3}, 
#'        \item \eqn{e(x) = 0.1.}}
#'      }
#' @seealso \code{\link{X_RF}}
#' @references \itemize{
#'   \item Daniel Lewandowskia, Dorota Kurowickaa, Harry Joe (2009). Generating
#'   random correlation matrices based on vines and extended onion method.
#'   \item Sören Künzel, Jasjeet Sekhon, Peter Bickel, and Bin Yu (2017). 
#'     Meta-learners for estimating heterogeneous treatment effects using
#'     machine learning.
#'   \item Wager, Stefan and Athey, Susan (2017). Estimation and inference of
#'   heterogeneous treatment effects using random forests.}
#' @examples
#' require(causalToolbox)
#' 
#' ce_sim <- simulate_causal_experiment(
#'   ntrain = 20,
#'   ntest = 20,
#'   dim = 7,
#'   setup = "RsparseT2weak",
#'   testseed = 293901,
#'   trainseed = 307017
#' )
#' 
#' # To see the treatment response function use:
#' ce_sim$m_t_truth
#' ce_sim$m_c_truth
#' ce_sim$propscore
#' @import MASS
#' @export 
simulate_causal_experiment <- function(ntrain,
                                       ntest,
                                       dim = ncol(given_features),
                                       alpha = .1,
                                       feat_distribution = "normal",
                                       given_features = NULL,
                                       setup = "RespSparseTau1strong",
                                       testseed = NULL,
                                       trainseed = NULL, 
                                       setupseed = 23987214) {

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
        as.numeric(m_t_truth(feat) - m_c_truth(feat))
      }
      if (is.null(given_features)) {
        given_features_fkt <- function(n, dim) {
          if (feat_distribution == "normal") {
            correlation_mat <- simulate_correlation_matrix(dim, alpha = alpha)
            mu <- rep(0, dim)
            feat <-
              data.frame(MASS::mvrnorm(
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

  # Definition of different setups ---------------------------------------------
  ## Now we introduce different setups:
  # 1.) RespSparseTau1strong ---------------------------------------------------
  if (setup == "RespSparseTau1strong") {
    if (dim < 3)
      stop("For RespSparseTau1strong the dimension must be at least 3")

    m_t_truth <-
      function(feat)
      {
        3 * feat$x1 + 5 * feat$x2  + 30 * feat$x3
      } # mu^t
    m_c_truth <-
      function(feat)
      {
        3 * feat$x1 + 5 * feat$x2
      }               # mu^c
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

  # 2.) RsparseT2weak ----------------------------------------------------------
  #
  if (setup == "RsparseT2weak") {
    if (dim < 3)
      stop("For RsparseT2weak the dimension must be at least 3")
    m_t_truth <-
      function(feat)
      {
        3 * feat$x1 + 3 * feat$x2  + 4 * feat$x3
      } # mu^t
    m_c_truth <-
      function(feat) {
        3 * feat$x1 + 5 * feat$x2               # mu^c
      }
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

  # 3.) complex tau ------------------------------------------------------------
  # control and treated group have nothing in common
  if (setup == "complexTau") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
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

  # 4.) complexTau2 ------------------------------------------------------------
  if (setup == "complexTau2") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, 1, 30)
    beatt_raw <- runif(dim, 1, 30)
    if (dim >= 20) {
      non_zero_coef <- sample(1:dim, 20)
      beatc_raw[-non_zero_coef[1:10]] <- 0
      beatt_raw[-non_zero_coef[11:20]] <- 0
    } else if (dim >= 10) {
      beatc_raw[-(1:10)] <- 0
      beatt_raw[-((dim - 9):dim)] <- 0
    }
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_t_truth <- function(feat) {
      betac_trunc <- beatc_raw
      as.matrix(feat) %*% betac_trunc                 # mu^t
    }
    m_c_truth <- function(feat) {
      beatt_trunc <- beatt_raw
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

  # 5.) complexTau3 ------------------------------------------------------------
  if (setup == "complexTau3") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, -5, 5)
    beatt_raw <- runif(dim, -5, 5)
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
 # 6.) complexTau4 -----------------------------------------------------------------
  if (setup == "complexTau4") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw1 <- runif(dim, -5, 5)
    beatt_raw1 <- runif(dim, -5, 5)
    beatc_raw2 <- runif(dim, -5, 5)
    beatt_raw2 <- runif(dim, -5, 5)
    beatc_raw3 <- runif(dim, -5, 5)
    beatt_raw3 <- runif(dim, -5, 5)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_t_truth <- function(feat) {
      betac_trunc1 <- beatc_raw1[1:ncol(feat)]
      betac_trunc2 <- beatc_raw2[1:ncol(feat)]
      betac_trunc3 <- beatc_raw3[1:ncol(feat)]

      ifelse(
        feat[, ncol(feat)] < -0.4,
        as.matrix(feat) %*% betac_trunc1,
        ifelse(
          feat[, ncol(feat)] < 0.4,
          as.matrix(feat) %*% betac_trunc2,
          as.matrix(feat) %*% betac_trunc3
        )
      )
    }
    m_c_truth <- function(feat) {
      betat_trunc1 <- beatt_raw1[1:ncol(feat)]
      betat_trunc2 <- beatt_raw2[1:ncol(feat)]
      betat_trunc3 <- beatt_raw3[1:ncol(feat)]

      ifelse(
        feat[, ncol(feat)] < -0.4,
        as.matrix(feat) %*% betat_trunc1,
        ifelse(
          feat[, ncol(feat)] < 0.4,
          as.matrix(feat) %*% betat_trunc2,
          as.matrix(feat) %*% betat_trunc3
        )
      )
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

  # 7.) Conf1 ------------------------------------------------------------------
  # treatment effect and treatment assignment are dependent
  if (setup == "Conf1") {
    m_t_truth <- function(feat) {
      2 * feat$x1 + 2 * feat$x2      # mu^t
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

  # 8.) rare1 ------------------------------------------------------------------
  if (setup == "rare1") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
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

  # 9.) rare2 ------------------------------------------------------------------
  if (setup == "rare2") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beat_raw <- runif(dim, -5, 5)
    .Random.seed <-
      current_seed  # sets back the current random stage
    m_c_truth <- function(feat) {
      beta_m <- beat_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m  + ifelse(feat$x1 > .5, 5, 0)  # mu^c
    }
    m_t_truth <- function(feat) {
      beta_m <- beat_raw[1:ncol(feat)]
      m_c_truth(feat) + ifelse(feat$x2 > .1, 8, 0)  # mu^t
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

  # 10.) rare3 -----------------------------------------------------------------
  if (setup == "rare3") {
    if (dim < 2)
      stop("For WA3 the dimension must be at least 2")

    effect <- function(feat) {
      sin(feat$x1) *
      sin(feat$x2) *
      sin(feat$x3) *
      sin(feat$x4)
    }

    m_c_truth <-
      function(feat)
        effect(feat) # mu^c
    m_t_truth <-
      function(feat)
        m_c_truth(feat) + ifelse(feat$x2 > .1, .3, 0)  # mu^t
    propscore <-
      function(feat)
        .01 # propensity score

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
  
  # 11.) STMpp -----------------------------------------------------------------
  if (setup == "STMpp") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, 1, 30)
    beatt_raw <- runif(dim, 1, 30)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_c_truth <- function(feat) {
      beta_m <- beatc_raw[1:ncol(feat)]
      as.matrix(feat) %*% beta_m                      # mu^c
    }
    m_t_truth <- function(feat) {
      m_c_truth(feat)                     # mu^t
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

  # 12.) STMpp2 ----------------------------------------------------------------
  if (setup == "STMpp2") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:

    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, -15, 15)
    beatt_raw <- runif(dim, -15, 15)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_c_truth <- function(feat) {
      beta_mdim <- min(ncol(feat), 5)
      beta_m <- beatc_raw[1:beta_mdim]
      as.matrix(feat)[ , 1:beta_mdim] %*% beta_m                      # mu^c
    }
    m_t_truth <- function(feat) {
      m_c_truth(feat)                     # mu^t
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

  # 13.) STMpp3 ----------------------------------------------------------------
  if (setup == "STMpp3") {
    if (dim < 2)
      stop("For WA3 the dimension must be at least 2")

    effect <- function(feat) {
      4 / ((1 + exp(-12 * (feat$x1 - 0.5))) *
             (1 + exp(-12 * (feat$x2 - 0.5))) *
             (1 + exp(-12 * (feat$x3 - 0.5))) *
             (1 + exp(-12 * (feat$x4 - 0.5))) *
             (1 + exp(-12 * (feat$x5 - 0.5)))
      )
    }

    m_c_truth <-
      function(feat)
        1 / 2 * effect(feat) # mu^c
    m_t_truth <-
      function(feat)
        m_c_truth(feat)  # mu^t
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

  # 14.) STMpp4 ----------------------------------------------------------------
  if (setup == "STMpp4") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:
    if(dim < 20) stop("dim must be at least 20")
    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, -15, 15)
    beatt_raw <- runif(dim, -15, 15)
    .Random.seed <-
      current_seed  # sets back the current random stage

    m_c_truth <- function(feat) {
      beta_mdim <- min(ncol(feat), 5)
      beta_m <- beatc_raw[1:beta_mdim]
      return(
        ifelse(
          feat[, 20] < -0.4,
          as.matrix(feat)[, 1:beta_mdim] %*% beta_m,
          ifelse(
            feat[, 20] < 0.4,
            as.matrix(feat)[, (beta_mdim + 1):(2 * beta_mdim)] %*% beta_m,
            as.matrix(feat)[, (2 * beta_mdim + 1): (3 * beta_mdim)] %*% beta_m
          )
        )
      )
    }
    m_t_truth <- function(feat) {
      m_c_truth(feat)                     # mu^t
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



  # 15.) Ufail -----------------------------------------------------------------
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

  # 16.) Usual1 ----------------------------------------------------------------
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

  # 17.) WA1 -------------------------------------------------------------------
  if (setup == "WA1") {
    if (dim < 3)
      stop("For WA1 the dimension must be at least 3")

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

  # 18.) WA2 -------------------------------------------------------------------
  if (setup == "WA2") {
    if (dim < 2)
      stop("For WA2 the dimension must be at least 2")
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

  # 19.) WA 3 ------------------------------------------------------------------
  if (setup == "WA3") {
    if (dim < 2)
      stop("For WA3 the dimension must be at least 2")

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

 # 20.) WA4 --------------------------------------------------------------------
  if (setup == "WA4") {
    if (dim < 2)
      stop("For WA3 the dimension must be at least 2")

    effect <- function(feat) {
      4 / ((1 + exp(-12 * (feat$x1 - 0.5))) *
             (1 + exp(-12 * (feat$x2 - 0.5))) *
             (1 + exp(-12 * (feat$x3 - 0.5))) *
             (1 + exp(-12 * (feat$x4 - 0.5))) *
             (1 + exp(-12 * (feat$x5 - 0.5)))
      )
    }

    m_c_truth <-
      function(feat)
        1 / 2 * effect(feat) # mu^c
    m_t_truth <-
      function(feat)
        - m_c_truth(feat)  # mu^t
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
  
  # 22.) sameCate_differentResponseFkts ----------------------------------------
  if (setup == "sameCate_differentResponseFkts") {
    # the following is used so that the seed is fixed for the creation of this
    # data set, but th seed is set back afterwards:
    
    current_seed <- .Random.seed  # saves the current random stage
    set.seed(setupseed)                # introduces a new seed to stay consistent
    beatc_raw <- runif(dim, -5, 5)
    beattau_raw <- runif(dim, -5, 5)
    .Random.seed <- current_seed  # sets back the current random stage
    
    m_c_truth <- function(feat) {
      betac_trunc <- beatc_raw[1:ncol(feat)]
      as.matrix(feat) %*% betac_trunc                 # mu^c
    }
    m_t_truth <- function(feat) {
      browser()
      rel_feature_ids <- 1:(min(5, ncol(feat)))
      beattau_trunc <- beattau_raw[rel_feature_ids]
      m_c_truth(feat) + as.matrix(feat[,rel_feature_ids]) %*% beattau_trunc 
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


  ## If nothing was returned by now, then something went wrong and we want to
  # throw an error:
  stop(
    "setup must be one of RespSparseTau1strong, RsparseT2weak, complexTau,
    Conf1, rare1, STMpp, Ufail, Usual1, WA1, WA2, WA3"
  )
}
