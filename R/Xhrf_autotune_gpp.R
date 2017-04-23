#' #' @include Xhrf.R
#' #' @include Xhrf_autotune_simple.R
#'

library(rBayesianOptimization)

#'
#' #' @title Autotuning for X-Learner with honest RF for both stages
#' #' @name X_RF_autotune_gpp
#' #' @rdname X_RF_autotune_gpp
#' #' @description This function tunes
#' #' @param feat A data frame of all the features.
#' #' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' #' @param yobs A numeric vector containing the observed outcomes.
#' #' @param ntree ..
#' #' @param Niter ..
#' #' @param nthread ..
#' #' @export X_RF_autotune_gpp
X_RF_autotune_gpp <-
  function(feat,
           tr,
           yobs,
           ntree = 20000,
           nthread = 0,
           verbose = TRUE) {

    starting_settings <- list(
      "start_setting_1" = get_setting_strong(feat,  tr, ntree, nthread),
      "start_setting_2" = get_setting_weak(feat, tr, ntree, nthread)
    )
    setup_eval <-
      check_setups(starting_settings, feat, tr, yobs, ntree,
                   nthread, verbose)

    starting_point <-
      starting_settings[[which.min(setup_eval$comb)]]

    # improve current state over
    starting_point_optimized <- GP_optimize(starting_point)

    if (verbose)
      print(paste(names(starting_settings)[which.min(setup_eval$comb)],
                  "is the best."))
    return(
      X_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = starting_settings[[which.min(setup_eval$comb)]],
        verbose = verbose
      )
    )
  }


GP_optimize <- function(starting_point, feat, tr, yobs,  ntr = sum(tr), ncr = sum(1-tr)) {

  Test_Fun <- function(...){
    Test_Fun_generic(starting_point, feat, tr, yobs, ...)
  }

  bounds <- list(
    mtry_first_0 = c(1, dim),
    mtry_first_1 = c(1, dim),
    mtry_second_0 = c(1, dim),
    mtry_second_1 = c(1, dim),
    nodesizeAvg_first_0 = c(1, ncr/30),
    nodesizeAvg_first_1 = c(1, ntr/30),
    nodesizeAvg_second_0 = c(1, ncr/30),
    nodesizeAvg_second_1 = c(1, ntr/30),
    nodesizeSpl_first_0 = c(1, ncr/30),
    nodesizeSpl_first_1 = c(1, ntr/30),
    nodesizeSpl_second_0 = c(1, ncr/30),
    nodesizeSpl_second_1 = c(1, ntr/30),
    sampsize_first_0 = c(ncr/5, ncr),
    sampsize_first_1 = c(ntr/5, ntr),
    sampsize_second_0 = c(ncr/5, ncr),
    sampsize_second_1 = c(ntr/5, ntr),
    splitratio_second_0 = c(0.2,1),
    splitratio_second_1 = c(0.2,1)
  )
  bounds <- list(
    mtry_first_0 = c(1, 1.1),
    mtry_first_1 = c(1, 1.1),
    mtry_second_0 = c(1, 1.1),
    mtry_second_1 = c(1, 1.1),
    nodesizeAvg_first_0 = c(1, 1.1),
    nodesizeAvg_first_1 = c(1, 1.1),
    nodesizeAvg_second_0 = c(1, 1.1),
    nodesizeAvg_second_1 = c(1, 1.1),
    nodesizeSpl_first_0 = c(1, 1.1),
    nodesizeSpl_first_1 = c(1, 1.1),
    nodesizeSpl_second_0 = c(1, 1.1),
    nodesizeSpl_second_1 = c(1, 1.1),
    sampsize_first_0 = c(ncr,ncr)/4,
    sampsize_first_1 = c(ntr,ntr)/4,
    sampsize_second_0 = c(ncr, ncr)/4,
    sampsize_second_1 = c(ntr, ntr)/4,
    splitratio_second_0 = c(0.5,.5),
    splitratio_second_1 = c(0.5,.5)
  )



  ## Set larger init_points and n_iter for better optimization result
  OPT_Res <- BayesianOptimization(
    Test_Fun,
    bounds = bounds,
    init_points = 2,
    n_iter = 5,
    acq = "ucb",
    kappa = 2.576,
    eps = 0.0,
    verbose = TRUE
  )
}

Test_Fun_generic <- function(starting_point, feat, tr, yobs,
                             mtry_first_0,
                             mtry_first_1,
                             mtry_second_0,
                             mtry_second_1,
                             nodesizeAvg_first_0,
                             nodesizeAvg_first_1,
                             nodesizeAvg_second_0,
                             nodesizeAvg_second_1,
                             nodesizeSpl_first_0,
                             nodesizeSpl_first_1,
                             nodesizeSpl_second_0,
                             nodesizeSpl_second_1,
                             sampsize_first_0,
                             sampsize_first_1,
                             sampsize_second_0,
                             sampsize_second_1,
                             splitratio_second_0,
                             splitratio_second_1) {
  current_setting <- change_setting(
    starting_point,
    round(mtry_first_0),
    round(mtry_first_1),
    round(mtry_second_0),
    round(mtry_second_1),
    round(nodesizeAvg_first_0),
    round(nodesizeAvg_first_1),
    round(nodesizeAvg_second_0),
    round(nodesizeAvg_second_1),
    round(nodesizeSpl_first_0),
    round(nodesizeSpl_first_1),
    round(nodesizeSpl_second_0),
    round(nodesizeSpl_second_1),
    round(sampsize_first_0),
    round(sampsize_first_1),
    round(sampsize_second_0),
    round(sampsize_second_1),
    splitratio_second_0,
    splitratio_second_1
  )

  evalval <-
    mean(as.numeric(evaluate_setting(current_setting, feat, tr, yobs)[1,]))

  return(list(Score = -evalval,
              Pred = 0))
}



change_setting <- function(this_setting,
                           mtry_first_0,
                           mtry_first_1,
                           mtry_second_0,
                           mtry_second_1,
                           nodesizeAvg_first_0,
                           nodesizeAvg_first_1,
                           nodesizeAvg_second_0,
                           nodesizeAvg_second_1,
                           nodesizeSpl_first_0,
                           nodesizeSpl_first_1,
                           nodesizeSpl_second_0,
                           nodesizeSpl_second_1,
                           sampsize_first_0,
                           sampsize_first_1,
                           sampsize_second_0,
                           sampsize_second_1,
                           splitratio_second_0,
                           splitratio_second_1) {
  this_setting$l_first_0$mtry <- mtry_first_0
  this_setting$l_first_1$mtry <- mtry_first_1
  this_setting$l_first_0$nodesizeAvg <- nodesizeAvg_first_0
  this_setting$l_first_1$nodesizeAvg <- nodesizeAvg_first_1
  this_setting$l_first_0$nodesizeSpl <- nodesizeSpl_first_0
  this_setting$l_first_1$nodesizeSpl <- nodesizeSpl_first_1
  this_setting$l_first_0$sampsize <- sampsize_first_0
  this_setting$l_first_1$sampsize <- sampsize_first_1

  this_setting$l_second_0$mtry <- mtry_second_0
  this_setting$l_second_1$mtry <- mtry_second_1
  this_setting$l_second_0$nodesizeAvg <- nodesizeAvg_second_0
  this_setting$l_second_1$nodesizeAvg <- nodesizeAvg_second_1
  this_setting$l_second_0$nodesizeSpl <- nodesizeSpl_second_0
  this_setting$l_second_1$nodesizeSpl <- nodesizeSpl_second_1
  this_setting$l_second_0$sampsize <- sampsize_second_0
  this_setting$l_second_1$sampsize <- sampsize_second_1
  this_setting$l_second_0$splitratio <- splitratio_second_0
  this_setting$l_second_1$splitratio <- splitratio_second_1

  return(this_setting)
}
