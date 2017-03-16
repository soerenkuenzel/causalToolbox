##########################
# Example data generator #
##########################

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
#' outcomes and the treatment assignment.
#' @return A list of the transformed object `x`, and encoding information
#' `labels`.
#' @export simulate_causal_experiment
simulate_causal_experiment <- function(ntrain,
                                       ntest,
                                       dim,
                                       alpha = .1,
                                       feat_distribution = "normal",
                                       given_features = NULL,
                                       setup = "RespSparseTau1strong") {
  if (!is.null(given_features))
    stop("We have not implemented this yet.")

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
            Sigma <- matrix(runif(dim ^ 2, 0, alpha),
                            nrow = dim,
                            ncol = dim)
            diag(Sigma) <- 1
            Sigma <- Sigma %*% t(Sigma) # make it positive definite.
            mu <- rep(0, dim)
            #' @import MASS
            feat <-
              data.frame(mvrnorm(
                n = n,
                mu = mu,
                Sigma = Sigma %*% t(Sigma)
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
          return(given_features)
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
      feat_te <- given_features_fkt(ntest, dim)
      W_te <- getW(feat_te)
      feat_tr <- given_features_fkt(ntrain, dim)
      W_tr <- getW(feat_tr)
      return(
        list(
          alpha = alpha,
          feat_te = feat_te,
          W_te = W_te,
          tau_te = tau(feat_te),
          Yobs_te = getYobs(feat_te, W_te),
          feat_tr = feat_tr,
          W_tr = W_tr,
          tau_tr = tau(feat_tr),
          Yobs_tr = getYobs(feat_tr, W_tr)
        )
      )
    }

  ## Now we introduce different setups:
  # 1.) RespSparseTau1strong:
  if (setup == "RespSparseTau1strong") {
    if(dim < 3)
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
    if(dim < 3)
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

  ## If nothing was returned by now, then something went wrong and we want to
  # throw an error:
  stop("setup must be one of RespSparseTau1strong, RsparseT2weak")
}

# #
# setup_Conf1 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Conf1"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   m_t_truth <<- function(feat) 2 * feat$x1 - 100 * feat$x2      # mu^t
#   m_c_truth <<- function(feat) 2 * feat$x1 + 2 * feat$x2      # mu^c
#   propscore <- function(feat) max(0.05, min(.95, feat$x1 / 2 + 1/4))             # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# # Response is sparse; Tau has only two weak predictor:
# setup_rare1 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "rare1"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   # set.seed(11)
#   beat.raw <- runif(dim, 1, 30)
#   m_t_truth <<- function(feat){
#     beta.m <- beat.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m  + ifelse(feat$x1 > .5, 5, 0) + 8  # mu^t
#   }
#   m_c_truth <<- function(feat){
#     beta.m <- beat.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m  + ifelse(feat$x1 > .5, 5, 0)  # mu^c
#   }
#   propscore <- function(feat) .01                               # propensity score
#   # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# #
# setup_STMpp <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "STMpp"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   # set.seed(112)
#   beat.raw <- runif(dim,1, 30)
#
#   m_t_truth <<- function(feat){
#     beta.m <- beat.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m                      # mu^t
#   }
#   m_c_truth <<- function(feat){
#     beta.m <- beat.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m                      # mu^t
#   }
#   propscore <- function(feat) .5                    # propensity score
#   # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# #
# setup_TTMpp <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "TTMpp"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   # set.seed(1145)
#   beatc.raw <- runif(dim,1, 30)
#   beatt.raw <- runif(dim,1, 30)
#
#   m_t_truth <<- function(feat){
#     beta.m <- beatc.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m                  # mu^t
#   }
#   m_c_truth <<- function(feat){
#     beta.m <- beatt.raw[1:ncol(feat)]
#     as.matrix(feat) %*% beta.m                  # mu^t
#   }
#   propscore <- function(feat) .5                    # propensity score
#
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# setup_Ufail <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Ufail"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   beat.raw <- (1:5 - 3)
#
#   m_t_truth <<- function(feat){
#     dim <- ncol(feat)
#     beta.m <- c(beat.raw, rep(0, dim - 5))
#     as.matrix(feat) %*% beta.m                  # mu^t
#   }
#   m_c_truth <<- function(feat){
#     dim <- ncol(feat)
#     beta.m <- rep(0, dim)
#     as.matrix(feat) %*% beta.m                  # mu^c
#   }
#   propscore <- function(feat) .5                    # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# setup_Usual1 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Usual1"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   m_t_truth <<- function(feat) 3 * feat$x1 + 4 * feat$x2  + 30 * feat$x3    # mu^t
#   m_c_truth <<- function(feat) 3 * feat$x1 + 5 * feat$x2               # mu^c
#   propscore <- function(feat) .5
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# setup_Wager1 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Wager1"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   m_t_truth <<- function(feat) 2 * (feat$x1  - 0.5)         # mu^t
#   m_c_truth <<- function(feat) 2 * (feat$x1  - 0.5)         # mu^c
#   propscore <- function(feat) 0.25 + dbeta(feat$x1, 2, 4) / 4  # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# setup_Wager2 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Wager2"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   effect <- function(feat){
#     (1 + 1 / (1 + exp(-20 * (feat$x1 - 1 / 3)))) *
#       (1 + 1 / (1 + exp(-20 * (feat$x2 - 1 / 3))))
#   }
#   m_t_truth <<- function(feat)    1/2 * effect(feat)        # mu^t
#   m_c_truth <<- function(feat)   -1/2 * effect(feat)        # mu^c
#   propscore <- function(feat) 0.5                           # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, alpha, ...)
# }
#
#
# setup_Wager3 <- function(ntrain, ntest, dim, alpha = 0, ...){
#   setup_name <<- "Wager3"
#   if(exists("alpha")) setup_name <<- paste0(setup_name, alpha)
#
#   # Setup potential outcome functions:
#   effect <- function(feat){
#     4 / ((1 + exp(-12 * (feat$x1 - 0.5))) *
#            (1 + exp(-12 * (feat$x2 - 0.5))))
#   }
#   m_t_truth <<- function(feat)    1/2 * effect(feat)        # mu^t
#   m_c_truth <<- function(feat)   -1/2 * effect(feat)        # mu^c
#   propscore <- function(feat) 0.5                           # propensity score
#
#   createTrainAndTest_base(ntrain, ntest, dim, m_t_truth, m_c_truth, propscore, ...)
# }
#
