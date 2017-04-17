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
#' default value is 5.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @param nthread Number of threads to train and predict thre forest. The
#' default number is 0 which represents using all cores.
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
  nthread
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
    stop("nodesizeSpl cannot exceed splitting sample size.")
  }
  if (nodesizeAvg > avgSampleSize) {
    stop("nodesizeAvg cannot exceed averaging sample size.")
  }

  if (splitratio < 0 || splitratio > 1){
    stop("splitratio must in between 0 and 1.")
  }

  if (splitratio == 0 || splitratio == 1){

    warning("honestRF is used as adaptive random forest.")

  } else {

    if (splitSampleSize < 2 * nodesizeSpl){
      stop("splitratio is too small such that splitting data cannot even be splitted!")
    }

    if (avgSampleSize < 2 * nodesizeAvg) {
      stop("splitratio is too big such that averaging data cannot even be splitted!")
    }

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

}

#' @title testing_data_checker-hoenstRF
#' @name testing_data_checker-honestRF
#' @rdname testing_data_checker-honestRF
#' @description Check the testing data to do prediction
#' @param x A data frame of all training predictors.
#' @param feature.new A data frame of testing predictors.
#' @export honestRF
testing_data_checker <- function(
  x,
  feature.new
){
  feature.new <- as.data.frame(feature.new)
  x <- as.data.frame(x)

  if (any(is.na(feature.new))) {
    stop("x contains missing data.")
  }

  if (ncol(feature.new) != ncol(x)) {
    stop("training data and testing data do not have same dimensionality.")
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
#' @slot forest A list of `RFTree` objects in the forest. If the class is
#' @slot x A data frame of all training predictors.
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
#' default value is 5.
#' @slot nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
#' @slot splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @exportClass honestRF
setClass(
  Class="honestRF",
  slots=list(
    forest="externalptr",
    x="data.frame",
    y="numeric",
    categoricalFeatureCols="list",
    categoricalFeatureMapping="list",
    ntree="numeric",
    replace="logical",
    sampsize="numeric",
    mtry="numeric",
    nodesizeSpl="numeric",
    nodesizeAvg="numeric",
    splitratio="numeric"
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
#' @param mtry The number of variables randomly selected at each split point.
#' The default value is set to be one third of total number of features of the
#' training data.
#' @param nodesizeSpl The minimum observations contained in terminal nodes. The
#' default value is 5.
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
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
#' @export honestRF
setGeneric(
  name="honestRF",
  def=function(
    x,
    y,
    ntree,
    replace,
    sampsize,
    mtry,
    nodesizeSpl,
    nodesizeAvg,
    splitratio,
    seed,
    verbose,
    nthread,
    splitrule
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
  mtry=max(floor(ncol(x)/3), 1),
  nodesizeSpl=5,
  nodesizeAvg=5,
  splitratio=1,
  seed=as.integer(runif(1)*1000),
  verbose=FALSE,
  nthread=0,
  splitrule="variance"
  ){

  # Preprocess the data
  training_data_checker(x, y, ntree,replace, sampsize, mtry, nodesizeSpl,
                        nodesizeAvg, splitratio, nthread)

  preprocessedData <- preprocess_training(x, y)
  processed_x <- preprocessedData$x
  categoricalFeatureCols <- preprocessedData$categoricalFeatureCols
  categoricalFeatureMapping <- preprocessedData$categoricalFeatureMapping

  # Total number of obervations
  nObservations <- length(y)
  numColumns <- ncol(x)

  categoricalFeatureCols_cpp <- unlist(categoricalFeatureCols)
  if (is.null(categoricalFeatureCols_cpp)){
    categoricalFeatureCols_cpp <- vector(mode="numeric", length=0)
  } else {
    categoricalFeatureCols_cpp <- categoricalFeatureCols_cpp - 1
  }

  # Create rcpp object
  # Create a forest object
  forest <- tryCatch({
    rcppForest <- rcpp_cppBuildInterface(
      x, y,
      categoricalFeatureCols_cpp,
      nObservations,
      numColumns, ntree, replace, sampsize, mtry,
      splitratio, nodesizeSpl, nodesizeAvg, seed, nthread, verbose
    )
    return(
      new(
        "honestRF",
        forest=rcppForest,
        x=as.data.frame(x),
        y=y,
        categoricalFeatureCols=categoricalFeatureCols,
        categoricalFeatureMapping=categoricalFeatureMapping,
        ntree=ntree,
        replace=replace,
        sampsize=sampsize,
        mtry=mtry,
        nodesizeSpl=nodesizeSpl,
        nodesizeAvg=nodesizeAvg,
        splitratio=splitratio
      )
    )
  }, error = function(err) {
    print(err)
    return(NULL)
  })

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
    testing_data_checker(object@x, feature.new)

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
setGeneric(
  name="getOOB",
  def=function(
    object
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
    object
  ){

    if (!object@replace && 0.8 * nrow(object@x) < object@sampsize) {
      warning("Samples are drawn without replacement and sample size is too big!")
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
