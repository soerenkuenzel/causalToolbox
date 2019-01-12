#' This file implements the X-Learner (https://arxiv.org/pdf/1706.03461.pdf)
#' with the forestry implementation (https://github.com/soerenkuenzel/forestry)
#' as base learner.
#' @include CATE_estimators.R
#' @include helper_functions.R


# X- RF class ------------------------------------------------------------------
#' @title XhRF constructor
#' @name X_RF-class
#' @rdname X_RF-class
#' @description The `X_RF` object is X-learner combined with honest random
#' forest used for the propensity score estimate, the first stage and the second
#' stage.
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector containing 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_0 contains an honest random forest predictor for the control group of
#' the first stage.
#' @slot m_1 contains an honest random forest predictor for the treated group of
#' the first stage.
#' @slot m_tau_0 contains an honest random forest predictor for the control
#' group of the second stage.
#' @slot m_tau_1 contains an honest random forest predictor for the treated
#' group of the second stage.
#' @slot m_prop contains an honest random forest predictor for the propensity 
#' score.
#' @slot hyperparameter_list A list of lists of hyper parameters used for the
#'   honest random forest algorithm of the forestry package
#' @exportClass X_RF
setClass(
  "X_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_0 = "forestry",
    m_1 = "forestry",
    m_tau_0 = "forestry",
    m_tau_1 = "forestry",
    m_prop = "forestry",
    hyperparameter_list = "list",
    creator = "function"
  )
)

# X_RF generator ---------------------------------------------------------------
#' @title X-Learner with honest RF for both stages
#' @details 
#' The X-Learner with random forest 
#' \enumerate{
#'  \item
#'     Estimate the response functions 
#'     \deqn{\mu_0(x) = E[Y(0) | X = x]}
#'     \deqn{\mu_1(x) = E[Y(1) | X = x]} 
#'     using the
#'     \href{https://github.com/soerenkuenzel/forestry}{\code{forestry}} random
#'     forest version with the hyperparameters specified in \code{mu.forestry}
#'     and denote the estimates as \eqn{\hat \mu_0} and \eqn{\hat \mu_1}.
#'  \item
#'     Impute the treatment effects for the individuals in the treated group,
#'     based on the control outcome estimator, and the treatment effects for the
#'     individuals in the control group, based on the treatment outcome
#'     estimator, that is,
#'     \deqn{D^1_i = Y_i(1) - \hat \mu_0(X_i)}
#'     \deqn{D^0_i = \hat \mu_1(X_i) - Y_i(0).}
#'     Now employ the 
#'     \href{https://github.com/soerenkuenzel/forestry}{\code{forestry}} random
#'     forest version with the hyperparameters specified in \code{tau.forestry}
#'     in two ways: using \eqn{D^1_i} as the dependent variable to obtain \eqn{\hat
#'     \tau_1(x)}, and using \eqn{D^1_i} as the dependent variable to obtain
#'     \eqn{\hat \tau_0(x)}.
#'  \item 
#'     Define the CATE estimate by a weighted average of the two estimates in
#'     Stage 2: 
#'     \deqn{\tau(x) = g(x) \hat \tau_0(x) + (1 - g(x)) \hat \tau_1(x).} 
#'     If \code{predmode = propmean}, then \eqn{g(x) = e(x)} where
#'     \eqn{e(x)} is an estimate of the propensity score using the 
#'     \href{https://github.com/soerenkuenzel/forestry}{\code{forestry}} random
#'     forest version with the hyperparameters specified in \code{e.forestry}.
#'     If \code{predmode = control}, then \eqn{g(x) = 1} and if 
#'     \code{predmode = treated}, then \eqn{g(x) = 0}.
#' }
#' @description This is an implementation of the X-learner with honest random
#' forest in the first and second stage. The function returns an X-RF object.
#' @param feat A data frame containing the features.
#' @param tr A numeric vector with 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param predmode One of propmean, control, treated. It specifies how
#'   the two estimators of the second stage should be aggregated. The default is
#'   propmean which refers to propensity score weighting.
#' @param nthread Number of threads which should be used to work in parallel.
#' @param verbose TRUE for detailed output FALSE for no output:
#' @param mu.forestry A list containing the hyperparameters for the
#'   \code{forestry} package that are used in the first stage of the X-learner.
#'   These hyperparameters are passed to the \code{forestry} package. Please
#'   refer to \url{https://github.com/soerenkuenzel/forestry} for a more
#'   detailed documentation of the hyperparamters.
#'   \itemize{
#'      \item \code{relevant.Variable} Variables that are only used in the first 
#'            stage.
#'      \item \code{ntree} Numbers of trees in the first stage.
#'      \item \code{replace} Sample with or without replacement in the first 
#'            stage.
#'      \item \code{sample.fraction} The size of total samples to draw for the 
#'            training data in the first stage.
#'      \item \code{mtry} The number of variables randomly selected at each 
#'            splitting point.
#'      \item \code{nodesizeSpl} minimum nodesize in the first stage for 
#'            the observations in the splitting set. (see the details of the 
#'            \code{forestry} package)
#'      \item \code{nodesizeAvg} minimum nodesize in the first stage for 
#'            the observations in the averaging set.
#'      \item \code{splitratio} Proportion of the training data used as the 
#'            splitting dataset in the first stage.
#'      \item \code{middleSplit} If true, the split value will be exactly in the 
#'            middle between two observations. Otherwise, it will take a point 
#'            based on a uniform distribution between the two observations. 
#'   }
#' @param tau.forestry hyperparameters for the second stage of the X-learner.
#'   Refer to the \code{mu.forestry} description for details.
#' @param e.forestry hyperparameters for estimating the propensity score. This
#'   is only relevant if \code{predmode = propmean} and \code{e = NULL}. 
#'   Refer to the \code{mu.forestry} description for details.
#' @return Object of class \code{X_RF}. It should be used with one of the 
#'   following functions \code{EstimateCATE}, \code{CateCI}, \code{CateBIAS}, 
#'   and \code{EstimateAllSampleStatistics}. The object has the following slots:
#'   \item{\code{feature_train}}{A copy of feat.}
#'   \item{\code{tr_train}}{A copy of tr.}
#'   \item{\code{yobs_train}}{A copy of yobs.}
#'   \item{\code{m_0}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the control group as the dependent variable.}
#'   \item{\code{m_1}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the treated group as the dependent variable.}
#'   \item{\code{m_tau_0}}{An object of class forestry that is fitted with the 
#'      \eqn{D_0} as the dependent variable.}
#'   \item{\code{m_tau_1}}{An object of class forestry that is fitted with the 
#'      \eqn{D_1} as the dependent variable.}
#'   \item{\code{m_prop}}{An object of class forestry that is fitted with tr as 
#'      the dependent variable.}
#'   \item{\code{hyperparameter_list}}{List containting the hyperparameters of 
#'      the three random forest algorithms used}
#'   \item{\code{creator}}{Function call of X_RF. This is used for different 
#'      bootstrap procedures.}
#' @author Soeren R. Kuenzel
#' @references
#' \itemize{
#'   \item Sören Künzel, Jasjeet Sekhon, Peter Bickel, and Bin Yu (2017). 
#'     Meta-learners for estimating heterogeneous treatment effects using
#'     machine learning. 
#'     \url{https://arxiv.org/pdf/1706.03461.pdf}
#'   \item 
#'     Sören Künzel, Simon Walter, and Jasjeet Sekhon (2018).
#'     Causaltoolbox---Estimator Stability for Heterogeneous Treatment Effects.
#'     \url{https://arxiv.org/pdf/1811.02833.pdf}
#'   \item Sören Künzel, Bradly Stadie, Nikita Vemuri, Varsha Ramakrishnan, 
#'     Jasjeet Sekhon, and Pieter Abbeel (2018). 
#'     Transfer learning for estimating causal effects using neural networks. 
#'     \url{https://arxiv.org/pdf/1808.07804.pdf}
#'   }
#' @seealso \code{\link{X_RF_fully_specified}}
#' @examples
#' require(causalToolbox)
#' 
#' # create example data set
#' simulated_experiment <- simulate_causal_experiment(
#'   ntrain = 1000,
#'   ntest = 1000,
#'   dim = 10,
#'   setup = "complexTau",
#'   testseed = 293901,
#'   trainseed = 307017
#' )
#' feat <- simulated_experiment$feat_tr
#' tr <- simulated_experiment$W_tr
#' yobs <- simulated_experiment$Yobs_tr
#' 
#' # create the hte object using honest Random Forests (RF)
#' xl_rf <- X_RF(feat = feat, tr = tr, yobs = yobs)
#' 
#' cate_esti_rf <- EstimateCate(xl_rf, feature_test)

#' # evaluate the performance
#' cate_true <- simulated_experiment$tau_te
#' mean((cate_esti_rf - cate_true) ^ 2)
#' #' \dontrun{
#' # Create confidence intervals via bootstrapping. 
#' xl_ci_rf <- CateCI(xl_rf, feature_test, B = 500)
#' }
#' @export 
X_RF <-
  function(feat,
           tr,
           yobs,
           predmode = "propmean",
           nthread = 0,
           verbose = TRUE,
           mu.forestry =
             list(
               relevant.Variable = 1:ncol(feat),
               ntree = 1000,
               replace = TRUE,
               sample.fraction = 0.8,
               mtry = round(ncol(feat) * 13 / 20),
               nodesizeSpl = 2,
               nodesizeAvg = 1,
               splitratio = 1,
               middleSplit = TRUE
             ),
           tau.forestry =
             list(
               relevant.Variable = 1:ncol(feat),
               ntree = 1000,
               replace = TRUE,
               sample.fraction = 0.7,
               mtry = round(ncol(feat) * 17 / 20),
               nodesizeSpl = 5,
               nodesizeAvg = 6,
               splitratio = 0.8,
               middleSplit = TRUE
             ),
           e.forestry =
             list(
               relevant.Variable = 1:ncol(feat),
               ntree = 500,
               replace = TRUE,
               sample.fraction =  0.5,
               mtry = ncol(feat),
               nodesizeSpl = 11,
               nodesizeAvg = 33,
               splitratio = .5,
               middleSplit = FALSE
             )) {
    
    # Cast input data to a standard format -------------------------------------
    feat <- as.data.frame(feat)
  
    # Catch misspecification erros ---------------------------------------------
    if (!is.integer(nthread) | nthread < 0) {
      stop("nthread must be a positive integer!")
    }
    
    if (!is.boolean(verbose)) {
      stop("verbose must be either TRUE or FALSE.")
    }
    
    if (predmode %in% c("propmean", "extreme", "control", "treated")) {
      stop("predmode should be one of propmean, extreme, control, or treated.")
    }
    
    catch_input_errors(feat, yobs, tr)
    
    # Set relevant relevant.Variable -------------------------------------------
    # User often sets the relevant variables by column names and not numerical
    # values. We translate it here to the index of the columns.
    
    if (is.null(mu.forestry$relevant.Variable)) {
      mu.forestry$relevant.Variable <- 1:ncol(feat)
    } else{
      if (is.character(mu.forestry$relevant.Variable))
        mu.forestry$relevant.Variable <-
          which(colnames(feat) %in% mu.forestry$relevant.Variable)
    }
    
    if (is.null(tau.forestry$relevant.Variable)) {
      tau.forestry$relevant.Variable <- 1:ncol(feat)
    } else{
      if (is.character(tau.forestry$relevant.Variable))
        tau.forestry$relevant.Variable <-
          which(colnames(feat) %in% tau.forestry$relevant.Variable)
    }
    
    if (is.null(e.forestry$relevant.Variable)) {
      e.forestry$relevant.Variable <- 1:ncol(feat)
    } else{
      if (is.character(e.forestry$relevant.Variable))
        e.forestry$relevant.Variable <-
          which(colnames(feat) %in% e.forestry$relevant.Variable)
    }
    
    # Translate the settings to a feature list ---------------------------------
    general_hyperpara <- list("predmode" = predmode,
                              "nthread" = nthread)
    
    hyperparameter_list <- list(
      "general" = general_hyperpara,
      "l_first_0" = mu.forestry,
      "l_first_1" = mu.forestry,
      "l_second_0" = tau.forestry,
      "l_second_1" = tau.forestry,
      "l_prop" = e.forestry
    )
    
    return(
      X_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = hyperparameter_list,
        verbose = verbose
      )
    )
  }

# X-RF basic constructor -------------------------------------------------------
#' @title X_RF fully specified constructor
#' @description This is the most basic X-learner with honest random forest
#'   constructor. It should not be called by the user, since the list of
#'   parameters is too big. Instead call the simpler version X_RF or one of the
#'   self tuning versions should be called. This function mainly exists to be
#'   called from other functions.
#' @param feat A feature data frame.
#' @param tr A vector of treatment assignment: 0 for control and 1 for
#'   treatment.
#' @param yobs A vector of all the observed outcomes.
#' @param hyperparameter_list A list of lists of hyper parameters
#' @param verbose TRUE for detailed output FALSE for no output
#' @return A `X_RF` object.
#' @seealso \code{\link{X_RF}}
#' @export X_RF_fully_specified
#' @import methods
X_RF_fully_specified <-
  function(feat,
           tr,
           yobs,
           hyperparameter_list,
           verbose) {
    
    # First stage --------------------------------------------------------------
    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]
    
    X_0 <- feat[tr == 0, ]
    X_1 <- feat[tr == 1, ]
    
    m_0 <-
      forestry::forestry(
        x = X_0[, hyperparameter_list[["l_first_0"]]$relevant.Variable],
        y = yobs_0,
        ntree = hyperparameter_list[["l_first_0"]]$ntree,
        replace = hyperparameter_list[["l_first_0"]]$replace,
        sample.fraction = hyperparameter_list[["l_first_0"]]$sample.fraction,
        mtry = hyperparameter_list[["l_first_0"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_first_0"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_first_0"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_first_0"]]$splitratio
      )
    
    m_1 <-
      forestry::forestry(
        x = X_1[, hyperparameter_list[["l_first_1"]]$relevant.Variable],
        y = yobs_1,
        ntree = hyperparameter_list[["l_first_1"]]$ntree,
        replace = hyperparameter_list[["l_first_1"]]$replace,
        sample.fraction = hyperparameter_list[["l_first_1"]]$sample.fraction,
        mtry = hyperparameter_list[["l_first_1"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_first_1"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_first_1"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_first_1"]]$splitratio
      )
    
    if (verbose) {
      print("Done with the first stage.")
    }
    
    # Second Stage -------------------------------------------------------------
    r_0 <- 
      predict(m_1, 
              X_0[, hyperparameter_list[["l_first_0"]]$relevant.Variable]) - 
      yobs_0
    r_1 <-
      yobs_1 - 
      predict(m_0, X_1[, hyperparameter_list[["l_first_1"]]$relevant.Variable])
    
    m_tau_0 <-
      forestry::forestry(
        x = X_0[, hyperparameter_list[["l_second_0"]]$relevant.Variable],
        y = r_0,
        ntree = hyperparameter_list[["l_second_0"]]$ntree,
        replace = hyperparameter_list[["l_second_0"]]$replace,
        sample.fraction = hyperparameter_list[["l_second_0"]]$sample.fraction,
        mtry = hyperparameter_list[["l_second_0"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_second_0"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_second_0"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_second_0"]]$splitratio
      )
    
    m_tau_1 <-
      forestry::forestry(
        x = X_1[, hyperparameter_list[["l_second_1"]]$relevant.Variable],
        y = r_1,
        ntree = hyperparameter_list[["l_second_1"]]$ntree,
        replace = hyperparameter_list[["l_second_1"]]$replace,
        sample.fraction = hyperparameter_list[["l_second_1"]]$sample.fraction,
        mtry = hyperparameter_list[["l_second_1"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_second_1"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_second_1"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_second_1"]]$splitratio
      )
    if (verbose) {
      print("Done with the second stage.")
    }
    
    # Prop score estimation ----------------------------------------------------
    m_prop <-
      forestry::forestry(
        x = feat[, hyperparameter_list[["l_prop"]]$relevant.Variable],
        y = tr,
        ntree = hyperparameter_list[["l_prop"]]$ntree,
        replace = hyperparameter_list[["l_prop"]]$replace,
        sample.fraction = hyperparameter_list[["l_prop"]]$sample.fraction,
        mtry = hyperparameter_list[["l_prop"]]$mtry,
        nodesizeSpl = hyperparameter_list[["l_prop"]]$nodesizeSpl,
        nodesizeAvg = hyperparameter_list[["l_prop"]]$nodesizeAvg,
        nthread = hyperparameter_list[["general"]]$nthread,
        splitrule = "variance",
        splitratio = hyperparameter_list[["l_prop"]]$splitratio
      )
    if (verbose) {
      print("Done with the propensity score estimation.")
    }
    return(
      new(
        "X_RF",
        feature_train = feat,
        tr_train = tr,
        yobs_train = yobs,
        m_0 = m_0,
        m_1 = m_1,
        m_tau_0 = m_tau_0,
        m_tau_1 = m_tau_1,
        m_prop = m_prop,
        hyperparameter_list = hyperparameter_list,
        creator = function(feat, tr, yobs) {
          X_RF_fully_specified(feat,
                               tr,
                               yobs,
                               hyperparameter_list,
                               verbose)
        }
      )
    )
  }

# Estimate CATE Method ---------------------------------------------------------
#' EstimateCate-X_hRF
#' @name EstimateCate-X_RF
#' @rdname EstimateCate
#' @inherit EstimateCate
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "X_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)

    catch_feat_input_errors(feature_new)
    
    predmode <- theObject@hyperparameter_list[["general"]]$predmode
    prop_scores <- predict(theObject@m_prop, feature_new)
    
    
    
    if (predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@m_tau_0, feature_new) +
          (1 - prop_scores)  * predict(theObject@m_tau_1, feature_new)
      )
    }
    
    if (predmode == "extreme") {
      return(ifelse(
        prop_scores > .5,
        predict(theObject@m_tau_0, feature_new),
        predict(theObject@m_tau_1, feature_new)
      ))
    }
    
    if (predmode == "control") {
      return(predict(theObject@m_tau_0, feature_new))
    }
    
    if (predmode == "treated") {
      return(predict(theObject@m_tau_1, feature_new))
    }
    
    stop("predmode should be one of propmean, extreme, control, or treated.")
    
  }
)

#' EstimateAllSampleStatistics-X_RF
#' @name EstimateAllSampleStatistics-X_RF
#' @rdname EstimateAllSampleStatistics-X_RF
#' @param theObject A `X_hRF` object.
#' @inheritParams EstimateAllSampleStatistics
#' @description Return the estimated CATE
#' @aliases EstimateAllSampleStatistics,X_RF-method
#' @exportMethod EstimateAllSampleStatistics
setMethod(
  f = "EstimateAllSampleStatistics",
  signature = "X_RF",
  definition = function(theObject,
                        method,
                        B,
                        nthread,
                        verbose) {
    ## shortcuts:
    # theObject = xl; method = "maintain_group_ratios"; B = 4; nthread = 2; verbose = TRUE
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    yobs <- theObject@yobs_train
    creator <- theObject@creator
    ntrain <- length(tr)
    if (method == "maintain_group_ratios") {
      createbootstrappedData <- function() {

        smpl_0 <- sample((1:ntrain)[tr == 0],
                         replace = TRUE,
                         size = sum(1-tr))
        smpl_1 <- sample((1:ntrain)[tr == 1],
                         replace = TRUE,
                         size = sum(tr))
        smpl <- sample(c(smpl_0, smpl_1))

        return(list(
          feat_b = feat[smpl, ],
          tr_b = tr[smpl],
          yobs_b = yobs[smpl]
        ))
      }
    }

    #### Run the bootstrap CI estimation #####################################

    # pred_B will contain for each simulation the prediction of each of the B
    # simulaions:
    pred_B <-
      as.data.frame(matrix(NA, nrow = nrow(feat), ncol = B))

    known_warnings <- c()
    # this is needed such that bootstrapped warnings are only printed once

    SATE_bootstrap_samples <- rep(NA, B)
    SATT_bootstrap_samples <- rep(NA, B)
    SATC_bootstrap_samples <- rep(NA, B)
    for (b in 1:B) {
      if (verbose)
        print(b)
      went_wrong <- 0
      # if that is 100 we really cannot fit it and bootstrap
      # seems to be infeasible.

      while (is.na(pred_B[1, b])) {
        if (went_wrong == 100)
          stop("one of the groups might be too small to
               do valid inference.")
        learner_bi <-
          tryCatch({
            bs <- createbootstrappedData()
            withCallingHandlers(
              # this is needed such that bootstrapped warnings are only
              # printed once
              creator(
                feat = bs$feat_b,
                tr = bs$tr_b,
                yobs = bs$yobs_b
              ),
              warning = function(w) {
                if (w$message %in% known_warnings) {
                  # message was already printed and can be ignored
                  invokeRestart("muffleWarning")
                } else{
                  # message is added to the known_warning list:
                  known_warnings <<- c(known_warnings, w$message)
                }
              }
            )
          },
          error = function(e) {
            return(NA)
          })

        CATE_bi <-   tryCatch({
          EstimateCate(learner_bi, feature_new = bs$feat_b)
        },
        error = function(e) {
          return(NA)
        })

        SATE_bootstrap_samples[b] <- mean(CATE_bi)
        SATT_bootstrap_samples[b] <- mean(CATE_bi[bs$tr_b == 1])
        SATC_bootstrap_samples[b] <- mean(CATE_bi[bs$tr_b == 0])
        pred_B[, b] <-
          tryCatch({
            EstimateCate(learner_bi, feature_new = feat)
          },
          error = function(e) {
            return(NA)
          })
        went_wrong <- went_wrong + 1
      }
    }

    # Compute Sample Statistics ------------------------------------------------
    # pred_B is a matrix each column consist of one bootstrapped prediciton.
    CateEstimates_mainlearner <- EstimateCate(theObject, feature_new = feat)
    # SATE_bootstrap_samples <- apply(pred_B, 2, mean)
    SATE_estimate <- mean(CateEstimates_mainlearner)
    SATE_sd <- sd(SATE_bootstrap_samples)
    SATE_lower <- SATE_estimate - 2 * SATE_sd
    SATE_upper <- SATE_estimate + 2 * SATE_sd

    # SATT_bootstrap_samples <- apply(pred_B[tr == 1, ], 2, mean)
    SATT_estimate <- mean(CateEstimates_mainlearner[tr == 1])
    SATT_sd <- sd(SATT_bootstrap_samples)
    SATT_lower <- SATT_estimate - 2 * SATT_sd
    SATT_upper <- SATT_estimate + 2 * SATT_sd

    # SATC_bootstrap_samples <- apply(pred_B[tr == 0, ], 2, mean)
    SATC_estimate <- mean(CateEstimates_mainlearner[tr == 0])
    SATC_sd <- sd(SATC_bootstrap_samples)
    SATC_lower <- SATC_estimate - 2 * SATC_sd
    SATC_upper <- SATC_estimate + 2 * SATC_sd

    # Compute the CATE intervals -----------------------------------------------

    # get the predictions from the original method
    CATE_pred <- EstimateCate(theObject, feature_new = feat)
    # the the 5% and 95% CI from the bootstrapped procedure
    CATE_sd <- apply(pred_B, 1, function(x) sd(x))
    CATE_lower <- CATE_pred - 1.96 * CATE_sd
    CATE_upper <- CATE_pred + 1.96 * CATE_sd

    # Transform Output for easy access------------------------------------------
    ATE <-
      rbind(
        data.frame(
          method = "all estimated",
          estimate = SATE_estimate,
          "lower" = SATE_lower,
          "upper" = SATE_upper
        )
      )
    ATT <-
      rbind(
        data.frame(
          method = "all estimated",
          estimate = SATT_estimate,
          "lower" = SATT_lower,
          "upper" = SATT_upper
        )
      )
    ATC <-
      rbind(
        data.frame(
          method = "all estimated",
          estimate = SATC_estimate,
          "lower" = SATC_lower,
          "upper" = SATC_upper
        )
      )

    CATE <-
      rbind(
        data.frame(
          method = "all estimated",
          estimate = CATE_pred,
          "lower" = CATE_lower,
          "upper" = CATE_upper
        )
      )

    row.names(ATE) <- row.names(ATT) <- row.names(ATC) <- NULL

    return(list("SATE" = ATE,
                "SATT" = ATT,
                "SATC" = ATC,
                "CATE" = CATE))
    }
)













