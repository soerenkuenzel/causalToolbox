#' @include CATE_estimators.R

# S-RF class -------------------------------------------------------------------
#' @title ShRF constructor
#' @name S_RF-class
#' @rdname S_RF-class
#' @description The `S_RF` object is a S-learner combined with honest random
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector containing 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot forest A forest object
#' @slot creator A function which creates a S_RF
#' @exportClass S_RF
#' @noRd
setClass(
  "S_RF",
  contains = "Meta-learner",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",
    forest = "forestry",
    hyperparameter_list = "list",
    creator = "function"
  ),
  validity = function(object)
  {
    if (!all(object@tr_train %in% c(0, 1))) {
      return("TR is the treatment and must be either 0 or 1")
    }
    return(TRUE)
  }
)

# S_RF generator ---------------------------------------------------------------
#' @title S-Learner with honest RF
#' @details 
#' The S-Learner with random forest works in two steps: 
#' \enumerate{
#'  \item
#'     Estimate the joint response function \deqn{\mu(x, w) = E[Y | X = x, W =
#'     w]} using the
#'     \href{https://github.com/soerenkuenzel/forestry}{\code{forestry}} random
#'     forest version with the hyperparameters specified in \code{mu.forestry}.
#'     We denote the estimate as \eqn{\hat \mu}.
#'  \item 
#'     Define the CATE estimate as
#'     \deqn{\tau(x) = \hat \mu_1(x, 1) - \hat \mu_0(x, 0).}
#' }
#' @description This is an implementation of the S-learner combined with honest
#' @param mu.forestry A list containing the hyperparameters for the
#'   \code{forestry} package that are used in \eqn{\hat \mu_0}.
#'   These hyperparameters are passed to the \code{forestry} package. 
#' @return Object of class \code{S_RF}. It should be used with one of the 
#'   following functions \code{EstimateCATE}, \code{CateCI}, \code{CateBIAS}, 
#'   and \code{EstimateAllSampleStatistics}. The object has the following slots:
#'   \item{\code{feature_train}}{A copy of feat.}
#'   \item{\code{tr_train}}{A copy of tr.}
#'   \item{\code{yobs_train}}{A copy of yobs.}
#'   \item{\code{m_0}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the control group as the dependent variable.}
#'   \item{\code{m_1}}{An object of class forestry that is fitted with the 
#'      observed outcomes of the treated group as the dependent variable.}
#'   \item{\code{hyperparameter_list}}{A list containting the hyperparameters of 
#'      the three random forest algorithms used}
#'   \item{\code{creator}}{Function call of S_RF. This is used for different 
#'      bootstrap procedures.}
#' @inherit X_RF
#' @family metalearners
#' @export
S_RF <-
  function(feat,
           tr,
           yobs, 
           nthread = 0,
           verbose = TRUE,
           alwaysTr = FALSE,
           takeOutATE = FALSE, 
           mu.forestry =
             list(
               relevant.Variable = 1:ncol(feat),
               ntree = 1000,
               replace = TRUE,
               sample.fraction = 0.9,
               mtry = ncol(feat),
               nodesizeSpl = 1,
               nodesizeAvg = 3,
               splitratio = .5,
               middleSplit = FALSE
             )) {
    #TODO
    if (alwaysTr) {
      stop(paste("always trying to split on the treatment variable is", 
                 "currently not implemented."))
    }
    #TODO
    if (takeOutATE) {
      stop(paste("taking out the ATE before applying the S-learner is", 
                 "currently not implemented."))
    }
    
    # Cast input data to a standard format -------------------------------------
    feat <- as.data.frame(feat)
    
    # Catch misspecification erros ---------------------------------------------
    if (!(nthread - round(nthread) == 0) | nthread < 0) {
      stop("nthread must be a positive integer!")
    }
    
    if (!is.logical(verbose)) {
      stop("verbose must be either TRUE or FALSE.")
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
    
    # Translate the settings to a feature list ---------------------------------
    general_hyperpara <- list("nthread" = nthread, alwaysTr = alwaysTr,
                              takeOutATE = takeOutATE)
    
    hyperparameter_list <- list(
      "general" = general_hyperpara,
      "mu.forestry" = mu.forestry
    )
    
    return(
      S_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = hyperparameter_list,
        verbose = verbose
      )
    )
  }
    
# S-RF basic constructor -------------------------------------------------------
#' @title S_RF fully specified constructor
#' @description This is the most basic S-learner with honest random forest
#'   constructor. It should not be called directly, since the list of
#'   parameters is too big. Instead, call the simpler version S_RF or one of the
#'   self tuning versions. This function exists mainly to be called from other
#'   functions.
#' @inherit X_RF_fully_specified
#' @seealso \code{\link{S_RF}}
#' @export
S_RF_fully_specified <-
  function(feat,
           tr,
           yobs,
           hyperparameter_list,
           verbose) {
    
    m <- forestry::forestry(
      x = cbind(feat[, hyperparameter_list[["mu.forestry"]]$relevant.Variable], 
                tr),
      y = yobs,
      ntree = hyperparameter_list[["mu.forestry"]]$ntree,
      replace = hyperparameter_list[["mu.forestry"]]$replace,
      sample.fraction = hyperparameter_list[["mu.forestry"]]$sample.fraction,
      mtry = hyperparameter_list[["mu.forestry"]]$mtry,
      nodesizeSpl = hyperparameter_list[["mu.forestry"]]$nodesizeSpl,
      nodesizeAvg = hyperparameter_list[["mu.forestry"]]$nodesizeAvg,
      nthread = hyperparameter_list[["general"]]$nthread,
      splitrule = "variance",
      splitratio = hyperparameter_list[["mu.forestry"]]$splitratio
    )
    
    new(
      "S_RF",
      feature_train = feat,
      tr_train = tr,
      yobs_train = yobs,
      forest = m,
      hyperparameter_list = hyperparameter_list,
      creator = function(feat, tr, yobs) {
        S_RF_fully_specified(feat,
                             tr,
                             yobs,
                             hyperparameter_list,
                             verbose)
      }
    )
  }

############################
### Estimate CATE Method ###
############################
#' EstimateCate-S_hRF
#' @name EstimateCate-S_RF
#' @rdname EstimateCate
#' @inherit EstimateCate
#' @exportMethod EstimateCate
setMethod(
  f = "EstimateCate",
  signature = "S_RF",
  definition = function(theObject, feature_new)
  {
    feature_new <- as.data.frame(feature_new)
    catch_feat_input_errors(feature_new)

    return(
      predict(theObject@forest, cbind(feature_new, tr = 1)) -
        predict(theObject@forest, cbind(feature_new, tr = 0))
    )
  }
)
