#' @title Autotuning for X-Learner with honest RF for both stages
#' @name X_RF_autotune_hyperband
#' @rdname X_RF_autotune_hyperband
#' @description This function tunes
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param Niter number of iterations.
#' @export X_RF_autotune_hyperband
setGeneric(
  name = "X_RF_autotune_hyperband",
  def = function(feat,
                 tr,
                 yobs,
                 sampsize = as.integer(nrow(x) * 0.75),
                 num_iter = 81,
                 eta = 3,
                 firststageVar = NULL,
                 secondstageVar = NULL,
                 verbose = FALSE,
                 seed = 24750371,
                 nthread = 0) {
    standardGeneric("X_RF_autotune_hyperband")
  }
)


X_RF_autotune_hyperband <-
  function(feat,
           tr,
           yobs,
           sampsize = as.integer(nrow(x) * 0.75),
           num_iter = 81,
           eta = 3,
           firststageVar = NULL,
           secondstageVar = NULL,
           verbose = TRUE,
           seed = 24750371,
           nthread = 0) {

    if (is.null(firststageVar)) {
      firststageVar <- 1:ncol(feat)
    } else{
      if (is.character(firststageVar))
        firststageVar <-
          which(colnames(feat) %in% firststageVar)
    }
    if (is.null(secondstageVar)) {
      secondstageVar <- 1:ncol(feat)
    } else{
      if (is.character(secondstageVar))
        secondstageVar <-
          which(colnames(feat) %in% secondstageVar)
    }

    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]

    X_0 <- feat[tr == 0, ]
    X_1 <- feat[tr == 1, ]

    m_0 <- autohonestRF(
      x = X_0[, firststageVar],
      y = yobs_0,
      sampsize = sampsize,
      num_iter = num_iter,
      eta = eta,
      verbose = FALSE,
      seed = seed,
      nthread = nthread
    )

    m_1 <- autohonestRF(
      x = X_1[, firststageVar],
      y = yobs_1,
      sampsize = sampsize,
      num_iter = num_iter,
      eta = eta,
      verbose = FALSE,
      seed = seed,
      nthread = nthread
    )

    if (verbose) {
      print("Done with the first stage.")
    }
    r_0 <- predict(m_1, X_0[, firststageVar]) - yobs_0
    r_1 <- yobs_1 - predict(m_0, X_1[, firststageVar])

    m_tau_0 <- autohonestRF(
      x = X_0[, secondstageVar],
      y = r_0,
      sampsize = sampsize,
      num_iter = num_iter,
      eta = eta,
      verbose = FALSE,
      seed = seed,
      nthread = nthread
    )

    m_tau_1 <-
      autohonestRF(
        x = X_1[, secondstageVar],
        y = yobs_1,
        sampsize = sampsize,
        num_iter = num_iter,
        eta = eta,
        verbose = FALSE,
        seed = seed,
        nthread = nthread
      )

    if (verbose) {
      print("Done with the second stage.")
    }

    m_prop <-
      honestRF(
        x = feat,
        y = tr,
        ntree = 500,
        nthread = nthread,
        splitratio = .5,
        nodesizeAvg = 10
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
        predmode = "propmean",
        firststageVar = firststageVar,
        secondstageVar = secondstageVar,
        creator = function(feat, tr, yobs) {
          X_RF(
            feat,
            tr,
            yobs,
            predmode = predmode,
            firststageVar = firststageVar,
            secondstageVar = secondstageVar,
            ntree_first = ntree_first,
            ntree_second = ntree_second,
            mtry_first = mtry_first,
            mtry_second = mtry_second,
            min_node_size_spl_first = min_node_size_spl_first,
            min_node_size_ave_first = min_node_size_ave_first,
            min_node_size_spl_second = min_node_size_spl_second,
            min_node_size_ave_second = min_node_size_ave_second,
            splitratio_first = splitratio_first,
            splitratio_second = splitratio_second,
            replace_first = replace_first,
            replace_second = replace_second,
            sample_fraction_first = sample_fraction_first,
            sample_fraction_second = sample_fraction_second,
            nthread = nthread,
            verbose = FALSE
          )
        }
      )
    )
  }


# # @title X_RF_autotune Constructor
# # @rdname X_RF-X_RF_autotune
# # @aliases X_RF_autotune, X_RF-X_RF_autotune
# # @return A `X_RF` object.
# X_RF_autotune <-
#   function(feat,
#            tr,
#            yobs,
#            ntree_first = 100,
#            ntree_second = 100,
#            Niter = 5,
#            K = 5,
#            nthread = 4) {
#     # define starting points 1 and 2
#     sp_1_firstStage <- data.frame(
#       mtry_first = round(ncol(feat) / 2),
#       min_node_size_spl_first = 1,
#       min_node_size_ave_first = 5,
#       splitratio_first = .5,
#       replace_first = TRUE,
#       sample_fraction_first = 0.8
#     )
#     sp_1_secondStage <- data.frame(
#       predmode = "propmean",
#       mtry_second = ncol(feat),
#       min_node_size_spl_second = 5,
#       min_node_size_ave_second = 3,
#       splitratio_second = .5,
#       replace_second = TRUE,
#       sample_fraction_second = 0.9
#     )
#
#     sp_2_firstStage <- data.frame(
#       mtry_first = round(ncol(feat) / 2),
#       min_node_size_spl_first = 1,
#       min_node_size_ave_first = 1,
#       splitratio_first = .4,
#       replace_first = FALSE,
#       sample_fraction_first = 0.7
#     )
#     sp_2_secondStage <- data.frame(
#       predmode = "propmean",
#       mtry_second = max(1, round(ncol(feat) / 5)),
#       min_node_size_spl_second = 10,
#       min_node_size_ave_second = 3,
#       splitratio_second = .75,
#       replace_second = FALSE,
#       sample_fraction_second = 0.8
#     )
#
#     ### tune the models of the first stage
#     yobs_0 <- yobs[tr == 0]
#     yobs_1 <- yobs[tr == 1]
#
#     X_0 <- feat[tr == 0,]
#     X_1 <- feat[tr == 1,]
#
#
#     ## tune the control response model (1a)
#
#     find_best_start_setting <- function(param1,
#                                         param2,
#                                         X,
#                                         Y,
#                                         ntree,
#                                         K,
#                                         nthread) {
#       test_parameter_setting_control <- function(param) {
#         test_firststage(
#           X = X_0,
#           Y = yobs_0,
#           ntree_first = ntree_first,
#           param = param,
#           K = K,
#           nthread = nthread
#         )
#       }
#       if (test_parameter_setting_control(sp_1_firstStage) >
#           test_parameter_setting_control(sp_2_firstStage)) {
#         return(sp_2_firstStage)
#       } else{
#         return(sp_1_firstStage)
#       }
#     }
#
#     tuned_setting_control <- start_setting_control
#
#     ## tune the treated response model (1b)
#     test_parameter_setting_treated <- function(param) {
#       test_firststage(
#         X = X_1,
#         Y = yobs_1,
#         ntree_first = ntree_first,
#         param = param,
#         K = K,
#         nthread = nthread
#       )
#     }
#     if (test_parameter_setting_treated(sp_1_firstStage) >
#         test_parameter_setting_treated(sp_2_firstStage)) {
#       start_setting_treated <- sp_2_firstStage
#     } else{
#       start_setting_treated <- sp_1_firstStage
#     }
#
#     tuned_setting_treated <- start_setting_treated
#
#
#     ### fix the first stage and tune the second stage
#     ## Fixing the firs stage estimators
#     m_0 <-
#       honestRF(
#         x = X_0[, firststageVar],
#         y = yobs_0,
#         ntree = ntree_first,
#         replace = tuned_setting_control$replace_first,
#         sampsize = round(
#           tuned_setting_control$sample_fraction_first * length(yobs_0)
#         ),
#         mtry = tuned_setting_control$mtry_first,
#         nodesizeSpl = tuned_setting_control$min_node_size_spl_first,
#         nthread = nthread,
#         splitrule =  'variance',
#         splitratio = tuned_setting_control$splitratio_first,
#         nodesizeAvg = tuned_setting_control$min_node_size_ave_first
#       )
#
#     m_1 <-
#       honestRF(
#         x = X_1[, firststageVar],
#         y = yobs_1,
#         ntree = ntree_first,
#         replace = tuned_setting_treated$replace_first,
#         sampsize = round(
#           tuned_setting_treated$sample_fraction_first * length(yobs_0)
#         ),
#         mtry = tuned_setting_treated$mtry_first,
#         nodesizeSpl = tuned_setting_treated$min_node_size_spl_first,
#         nthread = nthread,
#         splitrule =  'variance',
#         splitratio = tuned_setting_treated$splitratio_first,
#         nodesizeAvg = tuned_setting_treated$min_node_size_ave_first
#       )
#
#     if (verbose) {
#       print("Done with the first stage.")
#     }
#
#     r_0 <- predict(m_1, X_0[, firststageVar]) - yobs_0
#     r_1 <- yobs_1 - predict(m_0, X_1[, firststageVar])
#
#     ## Tuning the second stage
#
#     ## tune the control response model (1a)
#     test_parameter_setting_pseudo_control <- function(param) {
#       test_firststage(
#         X = X_0,
#         Y = r_0,
#         ntree_first = ntree_first,
#         param = param,
#         K = K,
#         nthread = nthread
#       )
#     }
#     if (test_parameter_setting_pseudo_control(sp_1_secondStage) >
#         test_parameter_setting_pseudo_control(sp_2_secondStage)) {
#       start_setting_control <- sp_2_secondStage
#     } else{
#       start_setting_control <- sp_1_secondStage
#     }
#
#     tuned_setting_control <- start_setting_control
#
#
#
#     m_tau_1 <-
#       honestRF(
#         x = X_1[, secondstageVar],
#         y = r_1,
#         ntree = ntree_second,
#         replace = replace_second,
#         sampsize = round(sample_fraction_second * length(r_1)),
#         mtry = mtry_second,
#         nodesizeSpl = min_node_size_spl_second,
#         nthread = nthread,
#         splitrule =  'variance',
#         splitratio = splitratio_second,
#         nodesizeAvg = min_node_size_ave_second
#       )
#
#     if (verbose) {
#       print("Done with the second stage.")
#     }
#
#
#     print("Hallo")
#
#   }
