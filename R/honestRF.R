##################
# Sanity Checker #
##################
#' @title training_data_checker-hoenstRF
#' @name training_data_checker-honestRF
#' @rdname training_data_checker-honestRF
#' @description Check the input to honestRF constructor
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @param ntree The number of trees to grow in the forest. The default value is
#' 500.
#' @param replace An indicator of whether sampling of training data is with
#' replacement. The default value is TRUE.
#' @param sampsize The size of total samples to draw for the training data. If
#' sampling with replacement, the default value is the length of the training
#' data. If samplying without replacement, the default value is two-third of
#' the length of the training data.
#' @param mtry The number of variables randomly selected at each split point.
#' The default value is set to be one third of total number of features of the
#' training data.
#' @param nodesizeSpl The minimum observations contained in terminal nodes. The
#' default value is 3.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 3.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @param nthread Number of threads to train and predict thre forest. The
#' default number is 0 which represents using all cores.
#' @param middleSplit if the split value is taking the average of two feature
#' values. If false, it will take a point based on a uniform distribution
#' between two feature values. (Default = FALSE)
#' @export honestRF
training_data_checker <- function(
  x,
  y,
  ntree,
  replace,
  sampsize,
  mtry,
  nodesizeSpl,
  nodesizeAvg,
  splitratio,
  nthread,
  middleSplit
){
  x <- as.data.frame(x)

  # Check if the input dimension of x matches y
  if (nrow(x) != length(y)) {
    stop("The dimension of input dataset x doesn't match the output vector y.")
  }

  # Check if x and y contain missing values
  if (any(is.na(x))) {
    stop("x contains missing data.")
  }
  if (any(is.na(y))) {
    stop("y contains missing data.")
  }

  nrows <- nrow(x)
  nfeatures <- ncol(x)

  if (!is.logical(replace)) {
    stop("replace must be TRUE or FALSE.")
  }

  if (ntree <= 0 || ntree %% 1 != 0) {
    stop("ntree must be a positive integer.")
  }

  if (sampsize <= 0 || sampsize %% 1 != 0) {
    stop("sampsize must be a positive integer.")
  }

  if (!replace && sampsize > nrow(x)) {
    stop("You cannot sample without replacement with size more than total
         number of oberservations.")
  }
  if (mtry <= 0 || mtry %% 1 != 0) {
    stop("mtry must be a positive integer.")
  }
  if (mtry > nfeatures) {
    stop("mtry cannot exceed total amount of features in x.")
  }

  if (nodesizeSpl <= 0 || nodesizeSpl %% 1 != 0) {
    stop("nodesizeSpl must be a positive integer.")
  }
  if (nodesizeAvg <= 0 || nodesizeAvg %% 1 != 0) {
    stop("nodesizeAvg must be a positive integer.")
  }

  # if the splitratio is 1, then we use adaptive rf and avgSampleSize is the
  # equal to the total sampsize
  if (splitratio == 0 || splitratio == 1){
    splitSampleSize <- sampsize
    avgSampleSize <- sampsize
  } else {
    splitSampleSize <- splitratio * sampsize
    avgSampleSize <- sampsize - splitSampleSize
  }

  if (nodesizeSpl > splitSampleSize) {
    warning("nodesizeSpl cannot exceed splitting sample size. We have set
            nodesizeSpl to be the maximum")
    nodesizeSpl <<- splitSampleSize
  }
  if (nodesizeAvg > avgSampleSize) {
    warning("nodesizeAvg cannot exceed averaging sample size. We have set
            nodesizeAvg to be the maximum")
    nodesizeAvg <<- avgSampleSize
  }

  if (splitratio < 0 || splitratio > 1){
    stop("splitratio must in between 0 and 1.")
  }

  if (splitratio == 0 || splitratio == 1){

    warning("honestRF is used as adaptive random forest.")

#   } else {
#
#     if (splitSampleSize < 2 * nodesizeSpl){
#       stop("splitratio is too small such that splitting data cannot even be splitted!")
#     }
#
#     if (avgSampleSize < 2 * nodesizeAvg) {
#       stop("splitratio is too big such that averaging data cannot even be splitted!")
#     }

  }

  if (nthread < 0 || nthread %% 1 != 0) {
    stop("nthread must be a nonegative integer.")
  }

  if (nthread > 0) {
    #' @import parallel
    library(parallel)
    if (nthread > detectCores()) {
      stop(paster0(
        "nthread cannot exceed total cores in the computer: ", detectCores()
        ))
    }
  }

  if (!is.logical(middleSplit)) {
    stop("middleSplit must be TRUE or FALSE.")
  }

}

#' @title testing_data_checker-hoenstRF
#' @name testing_data_checker-honestRF
#' @rdname testing_data_checker-honestRF
#' @description Check the testing data to do prediction
#' @param feature.new A data frame of testing predictors.
#' @export honestRF
testing_data_checker <- function(
  feature.new
){
  feature.new <- as.data.frame(feature.new)

  if (any(is.na(feature.new))) {
    stop("x contains missing data.")
  }

}

########################################
### Honest Random Forest Constructor ###
########################################
#' @title honstRF Constructor
#' @name honstRF-class
#' @rdname honestRF-class
#' @description `honestRF` object implementing the most basic version of
#' a random forest.
#' @slot forest An external pointer pointing to a C++ honestRF object
#' @slot dataframe An external pointer pointing to a C++ DataFrame object
#' @slot y A vector of all training responses.
#' @slot categoricalFeatureCols A list of index for all categorical data. Used
#' for trees to detect categorical columns.
#' @slot categoricalFeatureMapping A list of encoding details for each
#' categorical column, including all unique factor values and their
#' corresponding numeric representation.
#' @slot ntree The number of trees to grow in the forest. The default value is
#' 500.
#' @slot replace An indicator of whether sampling of training data is with
#' replacement. The default value is TRUE.
#' @slot sampsize The size of total samples to draw for the training data. If
#' sampling with replacement, the default value is the length of the training
#' data. If samplying without replacement, the default value is two-third of
#' the length of the training data.
#' @slot mtry The number of variables randomly selected at each split point.
#' The default value is set to be one third of total number of features of the
#' training data.
#' @slot nodesizeSpl The minimum observations contained in terminal nodes. The
#' default value is 3.
#' @slot nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 3.
#' @slot splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @slot middleSplit if the split value is taking the average of two feature
#' values. If false, it will take a point based on a uniform distribution
#' between two feature values. (Default = FALSE)
#' @exportClass honestRF
setClass(
  Class="honestRF",
  slots=list(
    forest="externalptr",
    dataframe="externalptr",
    categoricalFeatureCols="list",
    categoricalFeatureMapping="list",
    ntree="numeric",
    replace="logical",
    sampsize="numeric",
    mtry="numeric",
    nodesizeSpl="numeric",
    nodesizeAvg="numeric",
    splitratio="numeric",
    middleSplit="logical"
  )
)

#' @title honestRF-Constructor
#' @name honestRF-honestRF
#' @rdname honestRF-honestRF
#' @description Initialize a `honestRF` object.
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @param ntree The number of trees to grow in the forest. The default value is
#' 500.
#' @param replace An indicator of whether sampling of training data is with
#' replacement. The default value is TRUE.
#' @param sampsize The size of total samples to draw for the training data. If
#' sampling with replacement, the default value is the length of the training
#' data. If samplying without replacement, the default value is two-third of
#' the length of the training data.
#' @param sample.fraction if this is given, then sampsize is ignored and set to
#' be round(length(y) * sample.fraction). It must be a real number between 0 and
#' 1
#' @param mtry The number of variables randomly selected at each split point.
#' The default value is set to be one third of total number of features of the
#' training data.
#' @param nodesizeSpl The minimum observations contained in terminal nodes. The
#' default value is 3.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 3.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @param seed random seed
#' @param verbose if training process in verbose mode
#' @param nthread Number of threads to train and predict thre forest. The
#' default number is 0 which represents using all cores.
#' @param splitrule only variance is implemented at this point and it contains
#' specifies the loss function according to which the splits of random forest
#' should be made
#' @param middleSplit if the split value is taking the average of two feature
#' values. If false, it will take a point based on a uniform distribution
#' between two feature values. (Default = FALSE)
#' @param reuseHonestRF pass in an `honestRF` object which will recycle the
#' dataframe the old object created. It will save some space working on the same
#' dataset.
#' @export honestRF
setGeneric(
  name="honestRF",
  def=function(
    x,
    y,
    ntree,
    replace,
    sampsize,
    sample.fraction,
    mtry,
    nodesizeSpl,
    nodesizeAvg,
    splitratio,
    seed,
    verbose,
    nthread,
    splitrule,
    middleSplit,
    reuseHonestRF
    ){
    standardGeneric("honestRF")
  }
)

#' @title honestRF-Constructor
#' @rdname honestRF-honestRF
#' @aliases honestRF, honestRF-method
#' @importFrom Rcpp evalCpp
#' @useDynLib hte
#' @return A `honestRF` object.
honestRF <- function(
  x,
  y,
  ntree=500,
  replace=TRUE,
  sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
  sample.fraction = NULL,
  mtry=max(floor(ncol(x)/3), 1),
  nodesizeSpl=3,
  nodesizeAvg=3,
  splitratio=1,
  seed=as.integer(runif(1)*1000),
  verbose=FALSE,
  nthread=0,
  splitrule="variance",
  middleSplit=FALSE,
  reuseHonestRF=NULL
  ){
  # only if sample.fraction is given, update sampsize
  if(!is.null(sample.fraction))
    sampsize <- ceiling(sample.fraction * nrow(x))

  x <- as.data.frame(x)
  # Preprocess the data
  training_data_checker(x, y, ntree,replace, sampsize, mtry, nodesizeSpl,
                        nodesizeAvg, splitratio, nthread, middleSplit)
  # Total number of obervations
  nObservations <- length(y)
  numColumns <- ncol(x)

  if (is.null(reuseHonestRF)) {
    preprocessedData <- preprocess_training(x, y)
    processed_x <- preprocessedData$x
    categoricalFeatureCols <- preprocessedData$categoricalFeatureCols
    categoricalFeatureMapping <- preprocessedData$categoricalFeatureMapping

    categoricalFeatureCols_cpp <- unlist(categoricalFeatureCols)
    if (is.null(categoricalFeatureCols_cpp)){
      categoricalFeatureCols_cpp <- vector(mode="numeric", length=0)
    } else {
      categoricalFeatureCols_cpp <- categoricalFeatureCols_cpp - 1
    }

    # Create rcpp object
    # Create a forest object
    forest <- tryCatch({
      rcppDataFrame <- rcpp_cppDataFrameInterface(
        processed_x, y,
        categoricalFeatureCols_cpp,
        nObservations,
        numColumns
      )

      rcppForest <- rcpp_cppBuildInterface(
        processed_x, y,
        categoricalFeatureCols_cpp,
        nObservations,
        numColumns, ntree, replace, sampsize, mtry,
        splitratio, nodesizeSpl, nodesizeAvg, seed,
        nthread, verbose, middleSplit, TRUE, rcppDataFrame
      )
      return(
        new(
          "honestRF",
          forest=rcppForest,
          dataframe=rcppDataFrame,
          categoricalFeatureCols=categoricalFeatureCols,
          categoricalFeatureMapping=categoricalFeatureMapping,
          ntree=ntree,
          replace=replace,
          sampsize=sampsize,
          mtry=mtry,
          nodesizeSpl=nodesizeSpl,
          nodesizeAvg=nodesizeAvg,
          splitratio=splitratio,
          middleSplit=middleSplit
        )
      )
    }, error = function(err) {
      print(err)
      return(NULL)
    })

  } else {

    categoricalFeatureCols_cpp <- unlist(reuseHonestRF@categoricalFeatureCols)
    if (is.null(categoricalFeatureCols_cpp)){
      categoricalFeatureCols_cpp <- vector(mode="numeric", length=0)
    } else {
      categoricalFeatureCols_cpp <- categoricalFeatureCols_cpp - 1
    }

    categoricalFeatureMapping <- reuseHonestRF@categoricalFeatureMapping

    # Create rcpp object
    # Create a forest object
    forest <- tryCatch({
      rcppForest <- rcpp_cppBuildInterface(
        x, y,
        categoricalFeatureCols_cpp,
        nObservations,
        numColumns, ntree, replace, sampsize, mtry,
        splitratio, nodesizeSpl, nodesizeAvg, seed,
        nthread, verbose, middleSplit, TRUE, reuseHonestRF@dataframe
      )

      return(
        new(
          "honestRF",
          forest=rcppForest,
          dataframe=reuseHonestRF@dataframe,
          categoricalFeatureCols=reuseHonestRF@categoricalFeatureCols,
          categoricalFeatureMapping=categoricalFeatureMapping,
          ntree=ntree,
          replace=replace,
          sampsize=sampsize,
          mtry=mtry,
          nodesizeSpl=nodesizeSpl,
          nodesizeAvg=nodesizeAvg,
          splitratio=splitratio,
          middleSplit=middleSplit
        )
      )
    }, error = function(err) {
      print(err)
      return(NULL)
    })

  }

  return(forest)
}


######################
### Predict Method ###
######################
#' predict-honestRF
#' @name predict-honestRF
#' @rdname predict-honestRF
#' @description Return the prediction from the forest.
#' @param object A `honestRF` object.
#' @param feature.new A data frame of testing predictors.
#' @return A vector of predicted responses.
#' @aliases predict, honestRF-method
#' @exportMethod predict
setMethod(
  f="predict",
  signature="honestRF",
  definition=function(
    object,
    feature.new
  ){

    # Preprocess the data
    testing_data_checker(feature.new)

    processed_x <- preprocess_testing(
      feature.new,
      object@categoricalFeatureCols,
      object@categoricalFeatureMapping
    )

    rcppPrediction <- tryCatch({
      return(rcpp_cppPredictInterface(object@forest, processed_x))
    }, error = function(err) {
      print(err)
      return(NULL)
    })

    return(rcppPrediction)
  }
)


###########################
### Calculate OOB Error ###
###########################
#' @title getOOB-honestRF
#' @name getOOB-honestRF
#' @rdname getOOB-honestRF
#' @description Calculate the out-of-bag error of a given forest.
#' @param object A `honestRF` object.
#' @param noWarning flag to not display warnings
setGeneric(
  name="getOOB",
  def=function(
    object,
    noWarning=FALSE
  ){
    standardGeneric("getOOB")
  }
)

#' @title getOOB-honestRF
#' @aliases getOOB, honestRF-method
#' @return The OOB error of the forest.
#' @exportMethod getOOB
setMethod(
  f="getOOB",
  signature="honestRF",
  definition=function(
    object,
    noWarning
  ){

    # (all) TODO: find a better threshold for throwing such warning. 25 is
    # currently set up arbitrarily.
    if (!object@replace &&
        object@ntree * (rcpp_getObservationSizeInterface(object@dataframe) - object@sampsize) < 10) {
      if (!noWarning) {
        warning("Samples are drawn without replacement and sample size is too big!")
      }
      return(NA)
    }

    rcppOOB <- tryCatch({
      return(rcpp_OBBPredictInterface(object@forest))
    }, error = function(err) {
      print(err)
      return(NA)
    })

    return(rcppOOB)
  }
)



######################
### Add More Trees ###
######################
#' @title addTrees-honestRF
#' @name addTrees-honestRF
#' @rdname addTrees-honestRF
#' @description Add more trees to the existing forest.
#' @param object A `honestRF` object.
#' @param ntree Number of new trees to add
setGeneric(
  name="addTrees",
  def=function(
    object,
    ntree
  ){
    standardGeneric("addTrees")
  }
)

#' @title addTrees-honestRF
#' @aliases addTrees, honestRF-method
#' @exportMethod addTrees
#' @return A `honestRF` object
setMethod(
  f="addTrees",
  signature="honestRF",
  definition=function(
    object,
    ntree
  ){

    if (ntree <= 0 || ntree %% 1 != 0) {
      stop("ntree must be a positive integer.")
    }

    tryCatch({
      rcpp_AddTreeInterface(object@forest, ntree)
      object@ntree = object@ntree + ntree
      return(object)
    }, error = function(err) {
      print(err)
      return(NA)
    })

  }
)


#################
### Auto-Tune ###
#################
#' @title autohonestRF-honestRF
#' @name autohonestRF-honestRF
#' @rdname autohonestRF-honestRF
#' @description Autotune a honestRF based on the input dataset. The methodology
#' is based on paper `Hyperband: A Novel Bandit-Based Approach to
#' Hyperparameter Optimization` by Lisha Li, et al.
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @param sampsize The size of total samples to draw for the training data.
#' @param num_iter Maximum iterations/epochs per configuration. Default is 1024.
#' @param eta Downsampling rate. Default value is 2.
#' @param verbose if tuning process in verbose mode
#' @param seed random seed
#' @param nthread Number of threads to train and predict thre forest. The
#' default number is 0 which represents using all cores.
setGeneric(
  name="autohonestRF",
  def=function(
    x,
    y,
    sampsize,
    num_iter,
    eta,
    verbose,
    seed,
    nthread
  ){
    standardGeneric("autohonestRF")
  }
)

#' @title autohonestRF-honestRF
#' @aliases autohonestRF, honestRF-method
#' @return A `honestRF` object
#' @export autohonestRF
autohonestRF <- function(x,
                         y,
                         sampsize = as.integer(nrow(x) * 0.75),
                         num_iter = 1024,
                         eta = 2,
                         verbose = FALSE,
                         seed = 24750371,
                         nthread = 0) {
  if (verbose) {
    print("Start auto-tuning.")
  }

  # Creat a dummy tree just to reuse its data.
  dummy_tree <- honestRF(x, y, ntree=1, nodesizeSpl=nrow(x), nodesizeAvg=nrow(x))

  # Number of unique executions of Successive Halving (minus one)
  s_max <- as.integer(log(num_iter) / log(eta))

  # Total number of iterations (without reuse) per execution of
  # successive halving (n,r)
  B <- (s_max + 1) * num_iter

  if (verbose) {
    print(
      paste(
        "Hyperband will run successive halving in",
        s_max,
        "times, with",
        B,
        "iterations per execution."
      )
    )
  }

  # Begin finite horizon hyperband outlerloop
  models <- vector("list", s_max + 1)
  models_OOB <- vector("list", s_max + 1)

  set.seed(seed)

  for (s in s_max:0) {
    if (verbose) {
      print(paste("Hyperband successive halving round", s_max + 1 - s))
    }

    # Initial number of configurations
    n <- as.integer(ceiling(B / num_iter / (s + 1) * eta ^ s))

    # Initial number of iterations to run configurations for
    r <- num_iter * eta ^ (-s)

    if (verbose) {
      print(paste(">>> Total number of configurations:", n))
      print(paste(
        ">>> Number of iterations per configuration:",
        as.integer(r)
      ))
    }

    # Begin finite horizon successive halving with (n,r)
    # Generate parameters:
    allConfigs <- data.frame(
      mtry = sample(1:ncol(x), n, replace = TRUE),
      min_node_size_spl = NA, #sample(1:min(30, nrow(x)), n, replace = TRUE),
      min_node_size_ave = NA, #sample(1:min(30, nrow(x)), n, replace = TRUE),
      splitratio = runif(n, min = 0.1, max = 1),
      replace = sample(c(TRUE, FALSE), n, replace = TRUE),
      middleSplit = sample(c(TRUE, FALSE), n, replace = TRUE)
    )

    min_node_size_spl_raw <- floor(allConfigs$splitratio * sampsize *
                                     rbeta(n, 1, 3))
    allConfigs$min_node_size_spl <- ifelse(min_node_size_spl_raw == 0, 1,
                                           min_node_size_spl_raw)
    min_node_size_ave <- floor((1 - allConfigs$splitratio) * sampsize *
                                     rbeta(n, 1, 3))
    allConfigs$min_node_size_ave <- ifelse(min_node_size_ave == 0, 1,
                                           min_node_size_ave)

    if (verbose) {
      print(paste(">>>", n, " configurations have been generated."))
    }

    val_models <- vector("list", nrow(allConfigs))
    r_old <- 1
    for (j in 1:nrow(allConfigs)) {
      tryCatch({
        val_models[[j]] <- honestRF(
          x = x,
          y = y,
          ntree = r_old,
          mtry = allConfigs$mtry[j],
          nodesizeSpl = allConfigs$min_node_size_spl[j],
          nodesizeAvg = allConfigs$min_node_size_ave[j],
          splitratio = allConfigs$splitratio[j],
          replace = allConfigs$replace[j],
          sampsize = sampsize,
          nthread = nthread,
          middleSplit = allConfigs$middleSplit[j],
          reuseHonestRF=dummy_tree
        )
      }, error = function(err) {
        val_models[[j]] <- NULL
      })
    }

    if (s != 0) {
      for (i in 0:(s - 1)) {
        # Run each of the n_i configs for r_i iterations and keep best
        # n_i/eta
        n_i <- as.integer(n * eta ^ (-i))
        r_i <- as.integer(r * eta ^ i)
        r_new <- r_i - r_old

        # if (verbose) {
        #   print(paste("Iterations", i))
        #   print(paste("Total number of configurations:", n_i))
        #   print(paste("Number of iterations per configuration:", r_i))
        # }

        val_losses <- vector("list", nrow(allConfigs))

        # Iterate to evaluate each parameter combination and cut the
        # parameter pools in half every iteration based on its score
        for (j in 1:nrow(allConfigs)) {
          if (r_new > 0 && !is.null(val_models[[j]])) {
            val_models[[j]] <- addTrees(val_models[[j]], r_new)
          }
          if (!is.null(val_models[[j]])) {
            val_losses[[j]] <- getOOB(val_models[[j]], noWarning = TRUE)
            if (is.na(val_losses[[j]])) {
              val_losses[[j]] <- Inf
            }
          } else {
            val_losses[[j]] <- Inf
          }
        }

        r_old <- r_i

        val_losses_idx <-
          sort(unlist(val_losses), index.return = TRUE)
        val_top_idx <- val_losses_idx$ix[0:as.integer(n_i / eta)]
        allConfigs <- allConfigs[val_top_idx,]
        val_models <- val_models[val_top_idx]
        gc()
        rownames(allConfigs) <- 1:nrow(allConfigs)

        # if (verbose) {
        #   print(paste(length(val_losses_idx$ix) - nrow(allConfigs),
        #               "configurations have been eliminated."))
        # }

      }

    }
    # End finite horizon successive halving with (n,r)
    if (!is.null(val_models[[1]])) {
      best_OOB <- getOOB(val_models[[1]], noWarning = TRUE)
      if (is.na(best_OOB)) {
        stop()
        best_OOB <- Inf
      }
    } else {
      stop()
      best_OOB <- Inf
    }
    if (verbose) {
      print(paste(">>> Successive halving ends and the best model is saved."))
      print(paste(">>> OOB:", best_OOB))
    }

    if (!is.null(val_models[[1]]))
      models[[s + 1]] <- val_models[[1]]
    models_OOB[[s + 1]] <- best_OOB

  }

  # End finite horizon hyperband outlerloop and sort by performance
  model_losses_idx <- sort(unlist(models_OOB), index.return = TRUE)

  if (verbose) {
    print(
      paste(
        "Best model is selected from best-performed model in",
        s_max,
        "successive halving, with OOB",
        models_OOB[model_losses_idx$ix[1]]
      )
    )
  }

  return(models[[model_losses_idx$ix[1]]])

}
