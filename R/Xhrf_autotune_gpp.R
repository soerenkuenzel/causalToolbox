#' @include Xhrf.R
#' @include Xhrf_autotune_simple.R


#' @title Autotuning for X-Learner with honest RF for both stages
#' @name X_RF_autotune_gpp
#' @rdname X_RF_autotune_gpp
#' @description DO NOT USE YET
#' @param feat A data frame of all the features.
#' @param tr A numeric vector contain 0 for control and 1 for treated variables.
#' @param yobs A numeric vector containing the observed outcomes.
#' @param ntree ..
#' @param Niter ..
#' @param nthread ..
#' @export X_RF_autotune_gpp
X_RF_autotune_gpp <-
  function(feat,
           tr,
           yobs,
           ntree = 500,
           nthread = 0,
           verbose = TRUE,
           init_points = 5,
           n_iter = 1,
           ...) {

    # Exploring which of the starting settings is the best:
    starting_settings <- get_starting_settings(feat = feat, tr = tr,
                                               ntree = ntree, nthread = nthread)
    setup_eval <-
      check_setups(starting_settings, feat, tr, yobs, ntree,
                   nthread, verbose)
    if (verbose) {
      print(paste(
        "Of the starting setups",
        names(starting_settings)[which.min(setup_eval$comb)],
        "was the best."
      ))
      print("Starting to tune it using gaussian process priors.")
    }


    # Improving the current setting using Gaussain process priors.
    starting_point <- starting_settings[[which.min(setup_eval$comb)]]
    starting_point_optimized <-
      GP_optimize_small(starting_point, feat, tr, yobs, init_points, n_iter, verbose, ...)

    return(
      X_RF_fully_specified(
        feat = feat,
        tr = tr,
        yobs = yobs,
        hyperparameter_list = starting_point_optimized,
        verbose = verbose
      )
    )
  }






GP_optimize_small <- function(starting_point, feat, tr, yobs, init_points, n_iter, verbose, ...) {
  # dim = ncol(feat); ntr = sum(tr); ncr = sum(1-tr)
  Test_Fun <- function(...) {
    Test_Fun_generic(starting_point, feat, tr, yobs, ...)
  }

  upper_bounds <- get_upper_bounds_for_nodesize(starting_point, tr)

  bounds <- list(
    mtry_first = c(1, ncol(feat)),
    mtry_second = c(1, ncol(feat)),
    nodesizeAvg_first = c(1,  as.numeric(upper_bounds["nodesizeAvg_first_upper"])),
    nodesizeAvg_second = c(1, as.numeric(upper_bounds["nodesizeAvg_second_upper"])),
    nodesizeSpl_first = c(1,  as.numeric(upper_bounds["nodesizeSpl_first_upper"])),
    nodesizeSpl_second = c(1, as.numeric(upper_bounds["nodesizeSpl_second_upper"]))
  )

  OPT_Res <- rBayesianOptimization::BayesianOptimization(
    Test_Fun,
    bounds = bounds,
    init_grid_dt = data.frame(
      mtry_first = starting_point$l_first_0$mtry,
      mtry_second = starting_point$l_second_0$mtry,
      nodesizeAvg_first = starting_point$l_first_0$nodesizeAvg,
      nodesizeAvg_second = starting_point$l_second_0$nodesizeAvg,
      nodesizeSpl_first = starting_point$l_first_0$nodesizeSpl,
      nodesizeSpl_second = starting_point$l_second_0$nodesizeSpl
    ),
    init_points = init_points,
    n_iter = n_iter,
    acq = "ucb",
    kappa = 2.576,
    eps = 0.0,
    verbose = verbose,
    control = c(20, 8, 2),
    nug_thres = 10,
    maxit = 10,
    ...
  )

  best_setting <- change_setting(
    starting_point,
    round(OPT_Res$Best_Par["mtry_first"]),
    round(OPT_Res$Best_Par["mtry_second"]),
    round(OPT_Res$Best_Par["nodesizeAvg_first"]),
    round(OPT_Res$Best_Par["nodesizeAvg_second"]),
    round(OPT_Res$Best_Par["nodesizeSpl_first"]),
    round(OPT_Res$Best_Par["nodesizeSpl_second"])
  )
  return(best_setting)
}


# this is the generic function for the baysian optimization procedure.
Test_Fun_generic <- function(starting_point,
                             feat,
                             tr,
                             yobs,
                             mtry_first,
                             mtry_second,
                             nodesizeAvg_first,
                             nodesizeAvg_second,
                             nodesizeSpl_first,
                             nodesizeSpl_second) {
  current_setting <- change_setting(
    starting_point,
    round(mtry_first),
    round(mtry_second),
    round(nodesizeAvg_first),
    round(nodesizeAvg_second),
    round(nodesizeSpl_first),
    round(nodesizeSpl_second)
  )

  evalval <-
    mean(as.numeric(evaluate_setting(current_setting, feat, tr, yobs)[1, ]))

  return(list(Score = -evalval,
              Pred = 0))
}

# This function changes this_setting to have mtry first etc.
change_setting <- function(this_setting,
                           mtry_first,
                           mtry_second,
                           nodesizeAvg_first,
                           nodesizeAvg_second,
                           nodesizeSpl_first,
                           nodesizeSpl_second) {
  this_setting$l_first_0$mtry <- mtry_first
  this_setting$l_first_1$mtry <- mtry_first
  this_setting$l_first_0$nodesizeAvg <- nodesizeAvg_first
  this_setting$l_first_1$nodesizeAvg <- nodesizeAvg_first
  this_setting$l_first_0$nodesizeSpl <- nodesizeSpl_first
  this_setting$l_first_1$nodesizeSpl <- nodesizeSpl_first

  this_setting$l_second_0$mtry <- mtry_second
  this_setting$l_second_1$mtry <- mtry_second
  this_setting$l_second_0$nodesizeAvg <- nodesizeAvg_second
  this_setting$l_second_1$nodesizeAvg <- nodesizeAvg_second
  this_setting$l_second_0$nodesizeSpl <- nodesizeSpl_second
  this_setting$l_second_1$nodesizeSpl <- nodesizeSpl_second
  return(this_setting)
}
