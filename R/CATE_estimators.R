#' @include honestRF.R

setClass("CATE-estimators")
setClass(
  "Meta-learner",
  contains = "CATE-estimators",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    creator = "function"
  )
)


setGeneric(
  name = "EstimateCate",
  def = function(theObject, feature_new, ...)
  {
    standardGeneric("EstimateCate")
  }
)


setGeneric(
  name = "CateCI",
  def = function(theObject,
                 feature_new,
                 method = "maintain_group_ratios",
                 B = 200,
                 nthread = 0,
                 verbose = TRUE,
                 ...)
  {
    standardGeneric("CateCI")
  }
)

setGeneric(
  name = "EstimateATT",
  def = function(theObject,
                 feature_new,
                 method = "maintain_group_ratios",
                 B = 200,
                 nthread = 0,
                 verbose = TRUE,
                 ...){
    standardGeneric("EstimateATT")
  }
)

setGeneric(
  name = "EstimateAllSampleStatistics",
  def = function(theObject,
                 feature_new,
                 method = "maintain_group_ratios",
                 B = 200,
                 nthread = 0,
                 verbose = TRUE,
                 ...){
    standardGeneric("EstimateAllSampleStatistics")
  }
)



#' CateCI-X_hRF
#' @name CateCI-X_hRF
#' @rdname CateCI-X_hRF
#' @description Return the estimated confidence intervals for the CATE
#' @param object A `X_hRF` object.
#' @param feature_new A data frame.
#' @param method different versions of the bootstrap. Only n2TBS implemented
#' @param B number of bootstrap samples.
#' @param nthread number of threats used.
#' @return A data frame of estimated CATE Confidence Intervals
#' @aliases CateCI, X_hRF-method
#' @exportMethod CateCI
setMethod(
  f = "CateCI",
  signature = "Meta-learner",
  definition = function(theObject,
                        feature_new,
                        method,
                        B,
                        nthread,
                        verbose) {
    ## shortcuts:
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
      as.data.frame(matrix(NA, nrow = nrow(feature_new), ncol = B))

    known_warnings <- c()
    # this is needed such that bootstrapped warnings are only printed once
    for (b in 1:B) { # b= 1
      if (verbose)
        print(b)
      went_wrong <- 0
      # if that is 100 we really cannot fit it and bootstrap
      # seems to be infeasible.

      while (is.na(pred_B[1, b])) {
        if (went_wrong == 100)
          stop("one of the groups might be too small to
               do valid inference.")
        pred_B[, b] <-
          tryCatch({
            bs <- createbootstrappedData()

            withCallingHandlers(
              # this is needed such that bootstrapped warnings are only
              # printed once
              EstimateCate(
                creator(
                  feat = bs$feat_b,
                  tr = bs$tr_b,
                  yobs = bs$yobs_b
                ),
                feature_new = feature_new
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
        went_wrong <- went_wrong + 1
      }
    }


    # get the predictions from the original method
    pred <- EstimateCate(theObject, feature_new = feature_new)
    # the the 5% and 95% CI from the bootstrapped procedure
    CI_b <- data.frame(
      X5. =  apply(pred_B, 1, function(x)
        quantile(x, c(.025))),
      X95. = apply(pred_B, 1, function(x)
        quantile(x, c(.975))),
      sd = apply(pred_B, 1, function(x)
        sd(x))
    )

    return(data.frame(
      pred = pred,
      X5. =  pred - 1.96 * CI_b$sd,
      X95. = pred + 1.96 * CI_b$sd
      # X5. =  pred - (CI_b$X95. - CI_b$X5.) / 2,
      # X95. = pred + (CI_b$X95. - CI_b$X5.) / 2
      # X5. =  2 * pred - CI_b$X95.,
      # X95. = 2 * pred - CI_b$X5.
    ))
    }
)




#' EstimateAllSampleStatistics-Meta-learner
#' @name EstimateAllSampleStatistics-Meta-learner
#' @rdname EstimateAllSampleStatistics-Meta-learner
#' @description Return the estimated CATE
#' @exportMethod EstimateAllSampleStatistics
setMethod(
  f = "EstimateAllSampleStatistics",
  signature = "Meta-learner",
  definition = function(theObject,
                        method,
                        B,
                        nthread,
                        verbose) {
    ## shortcuts:
    # theObject = sl; method = "maintain_group_ratios"; B = 4; nthread = 2; verbose = TRUE
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
        pred_B[, b] <-
          tryCatch({
            bs <- createbootstrappedData()

            withCallingHandlers(
              # this is needed such that bootstrapped warnings are only
              # printed once
              EstimateCate(
                creator(
                  feat = bs$feat_b,
                  tr = bs$tr_b,
                  yobs = bs$yobs_b
                ),
                feature_new = feat
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
        went_wrong <- went_wrong + 1
      }
    }

    # Compute Sample Statistics ------------------------------------------------
    # pred_B is a matrix each column consist of one bootstrapped prediciton.

    SATE_bootstrap_samples <- apply(pred_B, 2, mean)
    SATE_estimate <- mean(SATE_bootstrap_samples)
    SATE_sd <- sd(SATE_bootstrap_samples)
    SATE_lower <- SATE_estimate - 2 * SATE_sd
    SATE_upper <- SATE_estimate + 2 * SATE_sd

    SATT_bootstrap_samples <- apply(pred_B[tr == 1, ], 2, mean)
    SATT_estimate <- mean(SATT_bootstrap_samples)
    SATT_sd <- sd(SATT_bootstrap_samples)
    SATT_lower <- SATT_estimate - 2 * SATT_sd
    SATT_upper <- SATT_estimate + 2 * SATT_sd

    SATC_bootstrap_samples <- apply(pred_B[tr == 0, ], 2, mean)
    SATC_estimate <- mean(SATC_bootstrap_samples)
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





