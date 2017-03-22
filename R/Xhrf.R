# setOldClass("honestRF")

############################
### Xlearner - hRF - hRF ###
############################
#' @title XhRF constructor
#' @name X_RF-class
#' @rdname X_RF-class
#' @description The `X_RF` object is X-learner combined with honest random
#' forest used for the propensity score estimate, the first stage and the second
#' stage.
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector contain 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_0 contains an honest random forest predictor for the control group of
#' the first stage.
#' @slot m_1 contains an honest random forest predictor for the treated group of
#' the first stage.
#' @slot m_tau_0 contains an honest random forest predictor for the control
#' group of the second stage.
#' @slot m_tau_1 contains an honest random forest predictor for the treated
#' group of the second stage.
#' @slot m_prop contains an honest random forest predictor the propensity score.
#' @slot firststageVar contains the indices of variables, which are only used in
#' the first stage.
#' @slot secondstageVar contains the numbers of variables, which are only used
#' in the second stage.
#' @exportClass X_RF
setClass(
  "X_RF",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    m_0 = "honestRF",
    m_1 = "honestRF",
    m_tau_0 = "honestRF",
    m_tau_1 = "honestRF",
    m_prop = "honestRF",
    predmode = "character",
    firststageVar = "numeric",
    secondstageVar = "numeric"
  )
)


#' @title honstRF Constructor
#' @name X_RF-X_RF
#' @rdname X_RF-X_RF
#' @description This is an implementation of the X-learner with honest random
#' forest in the first and second stage. The function returns an X-RF object.
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param predmode One of propmean, control, treated, extreme. It specifies how
#' the two estimators of the second stage should be aggregated. The default is
#' propmean which refers to propensity score weighting.
#' @param firststageVar Variables which are only used in the first stage.
#' @param secondstageVar Variables which are only used in the second stage.
#' @param ntree_first Numbers of trees in the first stage.
#' @param ntree_second Numbers of trees in the second stage.
#' @param min_node_size_spl_first minimum nodesize in the first stage for the
#' observations in the splitting set.
#' @param min_node_size_spl_second minimum nodesize in the second stage for the
#' observations in the splitting set.
#' @param min_node_size_ave_first minimum nodesize in the first stage for the
#' observations in the average set.
#' @param min_node_size_ave_second minimum nodesize in the second stage for the
#' observations in the averaging set.
#' @param splitratio_first Proportion of the training data used as the splitting
#' dataset in the first stage.
#' @param splitratio_second Proportion of the training data used as the
#' splitting dataset in the second stage.
#' @param replace_first Sample with or without replacement in the first stage.
#' @param replace_second Sample with or without replacement in the first stage.
#' @param sample_fraction_first The size of total samples to draw for the
#' training data in the first stage.
#' @param sample_fraction_second The size of total samples to draw for the
#' training data in the second stage.
#' @param nthread number of threats which should be used to work in parallel.
#' @export X_RF
setGeneric(
  name = "X_RF",
  def = function(feat,
                 tr,
                 yobs,
                 predmode,
                 firststageVar,
                 secondstageVar,
                 ntree_first,
                 ntree_second,
                 mtry_first,
                 mtry_second,
                 min_node_size_spl_first,
                 min_node_size_ave_first,
                 min_node_size_spl_second,
                 min_node_size_ave_second,
                 splitratio_first,
                 splitratio_second,
                 replace_first,
                 replace_second,
                 sample_fraction_first,
                 sample_fraction_second,
                 nthread) {
    standardGeneric("X_RF")
  }
)

#' @title X_RF Constructor
#' @rdname X_RF-X_RF
#' @aliases X_RF, X_RF-X_RF
#' @return A `X_RF` object.
X_RF <-
  function(feat,
           tr,
           yobs,
           predmode = "propmean",
           firststageVar = NULL,
           secondstageVar = NULL,
           ntree_first = 100,
           ntree_second = 100,
           mtry_first = round(ncol(feat) / 2),
           mtry_second = ncol(feat),
           min_node_size_spl_first = 3,
           min_node_size_ave_first = 3,
           min_node_size_spl_second = 5,
           min_node_size_ave_second = 3,
           splitratio_first = .5,
           splitratio_second = .5,
           replace_first = TRUE,
           replace_second = TRUE,
           sample_fraction_first = 0.9,
           sample_fraction_second = 0.9,
           nthread = 4) {
    # if firststageVar is not set, then set it to select all:
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
    if ((!is.null(mtry_first)) &&
        (mtry_first > ncol(feat))) {
      warning(
        "mtry_first is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry_first <- ncol(feat)
    }
    if ((!is.null(mtry_second)) &&
        (mtry_second > ncol(feat))) {
      warning(
        "mtry_second is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
      mtry_second <- ncol(feat)
    }

    yobs_0 <- yobs[tr == 0]
    yobs_1 <- yobs[tr == 1]

    X_0 <- feat[tr == 0,]
    X_1 <- feat[tr == 1,]

    m_0 <-
      honestRF(
        x = X_0[, firststageVar],
        y = yobs_0,
        ntree = ntree_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_0)),
        mtry = mtry_first,
        nodesizeSpl = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    m_1 <-
      honestRF(
        x = X_1[, firststageVar],
        y = yobs_1,
        ntree = ntree_first,
        replace = replace_first,
        sampsize = round(sample_fraction_first * length(yobs_1)),
        mtry = mtry_first,
        nodesizeSpl = min_node_size_spl_first,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_first,
        nodesizeAvg = min_node_size_ave_first
      )

    print("Done with the first stage.")

    r_0 <- predict(m_1, X_0[, firststageVar]) - yobs_0
    r_1 <- yobs_1 - predict(m_0, X_1[, firststageVar])

    m_tau_0 <-
      honestRF(
        x = X_0[, secondstageVar],
        y = r_0,
        ntree = ntree_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_0)),
        mtry = mtry_second,
        nodesizeSpl = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )

    m_tau_1 <-
      honestRF(
        x = X_1[, secondstageVar],
        y = r_1,
        ntree = ntree_second,
        replace = replace_second,
        sampsize = round(sample_fraction_second * length(r_1)),
        mtry = mtry_second,
        nodesizeSpl = min_node_size_spl_second,
        nthread = nthread,
        splitrule =  'variance',
        splitratio = splitratio_second,
        nodesizeAvg = min_node_size_ave_second
      )

    print("Done with the second stage.")

    m_prop <-
      honestRF(
        x = feat,
        y = tr,
        ntree = 50,
        nthread = nthread
      )

    print("Done with the propensity score estimation.")

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
        predmode = predmode,
        firststageVar = firststageVar,
        secondstageVar = secondstageVar
      )
    )
  }


############################
### Estimate CATE Method ###
############################
setGeneric(
  name = "EstimateCate",
  def = function(theObject, feature_new)
  {
    standardGeneric("EstimateCate")
  }
)

#' EstimateCate-X_hRF
#' @name EstimateCate-X_hRF
#' @rdname EstimateCate-X_hRF
#' @description Return the estimated CATE
#' @param object A `X_hRF` object.
#' @param feature_new A data frame.
#' @return A vector of predicted CATE
#' @aliases EstimateCate, X_hRF-method
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "X_RF",
  definition = function(theObject, feature_new)
  {
    prop_scores <- predict(theObject@m_prop, feature_new)
    if (theObject@predmode == "propmean") {
      return(
        prop_scores        * predict(theObject@m_tau_0, feature_new) +
          (1 - prop_scores)  * predict(theObject@m_tau_1, feature_new)
      )
    }
    if (theObject@predmode == "extreme") {
      return(ifelse(
        prop_scores > .5,
        predict(theObject@m_tau_0, feature_new),
        predict(theObject@m_tau_1, feature_new)
      ))
    }
    if (theObject@predmode == "control") {
      return(predict(theObject@m_tau_0, feature_new))
    }
    if (theObject@predmode == "treated") {
      return(predict(theObject@m_tau_1, feature_new))
    }
  }
)

############################
### CateCI Method ###
############################
setGeneric(
  name = "CateCI",
  def = function(theObject,
                 feature_new,
                 method = "n2TBS",
                 B,
                 nthread = 8)
  {
    standardGeneric("CateCI")
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
  signature = "X_RF",
  definition = function(theObject,
                        feature_new,
                        method,
                        B,
                        nthread)
  {
    ## shortcuts:
    feat <- theObject@feature_train
    tr <- theObject@tr_train
    yobs <- theObject@yobs_train
    predmode <- theObject@predmode
    ntrain <- length(tr)

    ############################################################################
    # parametric bootstrap based ###############################################

    # if (method %in% c("pF1", "pF2")) {
    #   if (method == "pF1") {
    #     createbootstrappedData <- function() {
    #       feat_b <<- feat
    #       tr_b <<- tr
    #
    #       # retrain the first stage to be more variable in unsure positions:
    #       yobs_0 <- yobs[tr == 0]
    #       yobs_1 <- yobs[tr == 1]
    #       X_0 <- feat[tr == 0,]
    #       X_1 <- feat[tr == 1,]
    #       smpl0 <- sample(1:length(yobs_0),
    #                       size = round(length(yobs_0) * .66),
    #                       replace = FALSE)
    #       smpl1 <- sample(1:length(yobs_1),
    #                       size = round(length(yobs_1) * .66),
    #                       replace = FALSE)
    #
    #       m_0 <<-
    #         ranger(yobs_0 ~ .,
    #                data = cbind(X_0, yobs_0)[smpl0, ],
    #                write.forest = TRUE)
    #       m_1 <<-
    #         ranger(yobs_1 ~ .,
    #                data = cbind(X_1, yobs_1)[smpl1, ],
    #                write.forest = TRUE)
    #
    #       # pretend m1 and m2 are correct
    #       y_fitted <- ifelse(
    #         tr_b == 1,
    #         predict(m_1, data = feat_b)$predictions,
    #         predict(m_0, data = feat_b)$predictions
    #       )
    #
    #       y_res <- yobs - y_fitted
    #       yobs_b <<- y_fitted + sample(y_res, replace = TRUE)
    #     }
    #
    #     pred_B <-
    #       as.data.frame(matrix(NA, nrow = nrow(feature_new), ncol = B))
    #     for (b in 1:B) {
    #       print(b)
    #       createbootstrappedData()
    #       tau_estimate <-
    #         EstimateCate(X_RF(feat_b, tr_b, yobs_b,  predmode),
    #                      feature_new = feature_new)
    #       tau_truth <-
    #         predict(m_1, data = feature_new)$predictions -
    #         predict(m_0, data = feature_new)$predictions
    #       pred_B[, b] <- tau_estimate - tau_truth
    #     }
    #     # get the predictions from the original method
    #     pred <- EstimateCate(theObject, feature_new = feature_new)
    #
    #     # the the 5% and 95% CI from the bootstrapped procedure
    #     CI_b <- data.frame(
    #       X5. =  apply(pred_B, 1, function(x)
    #         quantile(x, c(.025))),
    #       X95. = apply(pred_B, 1, function(x)
    #         quantile(x, c(.975))),
    #       sd = apply(pred_B, 1, function(x)
    #         sd(x))
    #     )
    #
    #     return(data.frame(
    #       pred = pred,
    #       # X5. =  pred - 1.96 * CI_b$sd,
    #       # X95. = pred + 1.96 * CI_b$sd
    #       # X5. =  pred - (CI_b$X95. - CI_b$X5.) / 2,
    #       # X95. = pred + (CI_b$X95. - CI_b$X5.) / 2
    #       X5. =  pred + CI_b$X5.,
    #       X95. = pred + CI_b$X95.
    #     ))
    #   }
    #
    #   if (method == "pF2") {
    #     createbootstrappedData <- function() {
    #       feat_b <<- feat
    #       tr_b <<- tr
    #
    #       #####
    #       ##### retrain the first stage to be more variable in unsure positions:
    #       yobs_0 <- yobs[tr == 0]
    #       yobs_1 <- yobs[tr == 1]
    #       X_0 <- feat[tr == 0,]
    #       X_1 <- feat[tr == 1,]
    #       smpl0 <- sample(1:length(yobs_0),
    #                       size = round(length(yobs_0) * .66),
    #                       replace = FALSE)
    #       smpl1 <- sample(1:length(yobs_1),
    #                       size = round(length(yobs_1) * .66),
    #                       replace = FALSE)
    #       estm_b <<- X_RF(
    #         feat = rbind(X_0[smpl0,], X_1[smpl1,]),
    #         tr = c(rep(0, length(smpl0)), rep(1, length(smpl1))),
    #         yobs = c(yobs_0[smpl0], yobs_1[smpl1])
    #       )
    #
    #       # (a) create bernoulli with propscore:
    #       prop_scores <-
    #         predict(estm_b@m_prop, data = feat_b)$predictions
    #       M_b <- rbinom(length(tr_b), 1, prop_scores)
    #
    #       # (b)
    #       y_fitted <-
    #         ifelse(
    #           tr_b == 1,
    #           ifelse(
    #             M_b == 1,
    #             predict(estm_b@m_1,     data = feat_b)$predictions,
    #             predict(estm_b@m_0,     data = feat_b)$predictions +
    #               predict(estm_b@m_tau_1, data = feat_b)$predictions
    #           ),
    #           ifelse(
    #             M_b == 1,
    #             predict(estm_b@m_1,     data = feat_b)$predictions -
    #               predict(estm_b@m_tau_0, data = feat_b)$predictions,
    #             predict(estm_b@m_0,     data = feat_b)$predictions
    #           )
    #         )
    #
    #       y_res <- yobs - y_fitted
    #       yobs_b <<- y_fitted
    #       yobs_b[tr_b == 1] <<-
    #         yobs_b[tr_b == 1] + sample(y_res[tr_b == 1], replace = TRUE)
    #       yobs_b[tr_b == 0] <<-
    #         yobs_b[tr_b == 0] + sample(y_res[tr_b == 0], replace = TRUE)
    #     }
    #
    #     pred_B <-
    #       as.data.frame(matrix(NA, nrow = nrow(feature_new), ncol = B))
    #     for (b in 1:B) {
    #       print(b)
    #       createbootstrappedData()
    #       tau_estimate <-
    #         EstimateCate(X_RF(feat_b, tr_b, yobs_b, predmode),
    #                      feature_new = feature_new)
    #       tau_truth <-     EstimateCate(estm_b,
    #                                     feature_new = feature_new)
    #       pred_B[, b] <- tau_estimate - tau_truth
    #     }
    #     # get the predictions from the original method
    #     pred <- EstimateCate(theObject, feature_new = feature_new)
    #
    #     # the the 5% and 95% CI from the bootstrapped procedure
    #     CI_b <- data.frame(
    #       X5. =  apply(pred_B, 1, function(x)
    #         quantile(x, c(.025))),
    #       X95. = apply(pred_B, 1, function(x)
    #         quantile(x, c(.975))),
    #       sd = apply(pred_B, 1, function(x)
    #         sd(x))
    #     )
    #
    #     return(data.frame(
    #       pred = pred,
    #       # X5. =  pred - 1.96 * CI_b$sd,
    #       # X95. = pred + 1.96 * CI_b$sd
    #       # X5. =  pred - (CI_b$X95. - CI_b$X5.) / 2,
    #       # X95. = pred + (CI_b$X95. - CI_b$X5.) / 2
    #       X5. =  pred + CI_b$X5.,
    #       X95. = pred + CI_b$X95.
    #     ))
    #   }
    # }

    ############################################################################
    # standard bootstrap based #################################################

    if (method %in% c("defaultBS", "n2FBS", "n2TBS")) {
      # if (method == "defaultBS") {
      #   createbootstrappedData <- function() {
      #     smpl <- sample(1:ntrain, replace = TRUE, size = ntrain)
      #     feat_b <<- feat[smpl,]
      #     tr_b <<- tr[smpl]
      #     yobs_b <<- yobs[smpl]
      #   }
      # }
      # if (method == "n2FBS") {
      #   createbootstrappedData <- function() {
      #     smpl <- sample(1:ntrain,
      #                    replace = FALSE,
      #                    size = round(ntrain / 2))
      #     feat_b <<- feat[smpl,]
      #     tr_b <<- tr[smpl]
      #     yobs_b <<- yobs[smpl]
      #   }
      # }

      if (method == "n2TBS") {
        createbootstrappedData <- function() {
          smpl <- sample(1:ntrain,
                         replace = TRUE,
                         size = round(ntrain / 2))
          return(list(
            feat_b = feat[smpl,],
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
      for (b in 1:B) {
        print(b)
        went_wrong <-
          0 # if that is 100 we really cannot fit it and bootstrap
        # seems to be infeasible.
        while (is.na(pred_B[1, b])) {
          if (went_wrong == 100)
            stop("one of the groups might be too small to
                 do valid inference.")
          pred_B[, b] <-
            tryCatch({

              bs <- createbootstrappedData()


              EstimateCate(
                X_RF(
                  feat = bs$feat_b,
                  tr = bs$tr_b,
                  yobs = bs$yobs_b,
                  predmode,
                  nthread = nthread
                ),
                feature_new = feature_new
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
  }
  )
