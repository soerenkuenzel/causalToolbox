#################################
### Random Forest Constructor ###
#################################
#' @title honestRFRcpp-Constructor
#' @name honestRFRcpp-class
#' @rdname honestRFRcpp-class
#' @description `honestRFRcpp` object implementing the most basic version of
#' a random forest.
#' @slot forest A list of `RFTree` objects in the forest. If the class is
#' extended, the list may contain the corresponding extended `RFTree` object.
#' @slot categoricalFeatureCols A list of index for all categorical data. Used
#' for trees to detect categorical columns.
#' @slot categoricalFeatureMapping A list of encoding details for each
#' categorical column, including all unique factor values and their
#' corresponding numeric representation.
#' @exportClass honestRFRcpp
setClass(
  Class="honestRFRcpp",
  slots=list(
    forest="externalptr",
    categoricalFeatureCols="list",
    categoricalFeatureMapping="list"
  )
)

#' @title honestRFRcpp-Constructor
#' @name honestRFRcpp-honestRFRcpp
#' @rdname honestRFRcpp-honestRFRcpp
#' @description Initialize a `honestRFRcpp` object.
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
#' @export honestRFRcpp
setGeneric(
  name="honestRFRcpp",
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
    verbose
    ){
    standardGeneric("honestRFRcpp")
  }
)

#' @title honestRFRcpp-Constructor
#' @rdname honestRFRcpp-honestRFRcpp
#' @aliases honestRFRcpp, honestRFRcpp-method
#' @importFrom Rcpp evalCpp
#' @useDynLib hte
#' @return A `honestRFRcpp` object.
honestRFRcpp <- function(
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
  verbose=FALSE
  ){

  # Preprocess the data
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
  rcppForest <- rcpp_cppBuildInterface(
      x, y,
      categoricalFeatureCols_cpp,
      nObservations,
      numColumns, ntree, replace, sampsize, mtry,
      splitratio, nodesizeSpl, nodesizeAvg, seed, verbose
  )

  # Create a forest object
  forest <- new(
    "honestRFRcpp",
    forest=rcppForest,
    categoricalFeatureCols=categoricalFeatureCols,
    categoricalFeatureMapping=categoricalFeatureMapping
    )

  return(forest)
}


######################
### Predict Method ###
######################
#' predict-honestRFRcpp
#' @name predict-honestRFRcpp
#' @rdname predict-honestRFRcpp
#' @description Return the prediction from the forest.
#' @param object A `honestRFRcpp` object.
#' @param feature.new A data frame of testing predictors.
#' @return A vector of predicted responses.
#' @aliases predict, honestRFRcpp-method
#' @exportMethod predict
setMethod(
  f="predict",
  signature="honestRFRcpp",
  definition=function(
    object,
    feature.new
  ){

    # Preprocess the data
    processed_x <- preprocess_testing(
      feature.new,
      object@categoricalFeatureCols,
      object@categoricalFeatureMapping
    )

    return(rcpp_cppPredictInterface(object@forest, processed_x))
  }
)


######################
### Predict Method ###
######################
#' @title honestRFRcpp-free
#' @name free-honestRFRcpp
#' @rdname free-honestRFRcpp
#' @description Free the Rcpp object in the memory.
#' @param object A `honestRFRcpp` object.
#' @export free
setGeneric(
  name="free",
  def=function(
    object
  ){
    standardGeneric("free")
  }
)

#' @title honestRFRcpp-free
#' @rdname free-honestRFRcpp
#' @aliases free, honestRFRcpp-method
setMethod(
  f="free",
  signature="honestRFRcpp",
  definition=function(
    object
  ){
    rcpp_cppFreeInterface(object@forest)
  }
)
