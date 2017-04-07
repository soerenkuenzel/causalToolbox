#' @include EstimateCate.R
#' @include honestRF.R
setOldClass("honestRF")

############################
############################
#' @name S_RF-class
#' @rdname S_RF-class
#' forest used for both response functions
#' @slot feature_train A data frame of all training features.
#' @slot tr_train A vector contain 0 for control and 1 for treated variables.
#' @slot yobs_train A vector containing the observed outcomes.
#' @slot m_y_t contains an honest random forest predictor for the treated group
#' @slot m_y_c contains an honest random forest predictor for the control group
#' @slot creator function which creates a S_RF
#' @exportClass S_RF
setClass(
  "S_RF",
  slots = list(
    feature_train = "data.frame",
    tr_train = "numeric",
    yobs_train = "numeric",

      warning(
        "mtry is chosen bigger than number of features. It will be set
        to be equal to the number of features"
      )
    }

