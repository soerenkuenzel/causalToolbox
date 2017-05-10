######################################
### Methods for Preprocessing Data ###
######################################
#' @title preprocess_training
#' @description Perform preprocessing for the training data, including
#' converting data to dataframe, and encoding categorical data into numerical
#' representation.
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @import plyr
#' @return A list of two datasets along with necessary information that
#' encoding the preprocessing.
preprocess_training <- function(x, y){
  x <- as.data.frame(x)

  # Check if the input dimension of x matches y
  if (nrow(x) != length(y)) {
    stop("The dimension of input dataset x doesn't match the output vector y.")
  }

  # Track the order of all features
  featureNames <- colnames(x)
  if (is.null(featureNames)) {
    warning("No names are given for each column.")
  }

  # Track all categorical features (both factors and characters)
  featureFactorCols <- which(sapply(x, is.factor) == TRUE)
  featureCharacterCols <- which(sapply(x, is.character) == TRUE)
  categoricalFeatureCols <- c(featureFactorCols, featureCharacterCols)
  if (length(categoricalFeatureCols) == 0) {
    categoricalFeatureCols <- list()
  } else {
    categoricalFeatureCols <- list(categoricalFeatureCols)
  }

  # For each categorical feature, encode x into numeric representation and
  # save the encoding mapping
  categoricalFeatureMapping <- list()
  dummyIndex = 1
  for (categoricalFeatureCol in unlist(categoricalFeatureCols)) {
    uniqueFeatureValues <- unique(x[,categoricalFeatureCol])
    numericFeatureValues <- 1:length(uniqueFeatureValues)
    x[,categoricalFeatureCol] <- mapvalues(
      x=x[,categoricalFeatureCol],
      from=uniqueFeatureValues,
      to=numericFeatureValues
    )
    categoricalFeatureMapping[[dummyIndex]] <- list(
      "categoricalFeatureCol"=categoricalFeatureCol,
      "uniqueFeatureValues"=uniqueFeatureValues,
      "numericFeatureValues"=numericFeatureValues
    )
    dummyIndex <- dummyIndex + 1
  }

  # Return transformed data and encoding information
  return(list(
    "x"=x,
    "categoricalFeatureCols"=categoricalFeatureCols,
    "categoricalFeatureMapping"=categoricalFeatureMapping
  ))
}

#' @title preprocess_testing
#' @description Perform preprocessing for the testing data, including
#' converting data to dataframe, and testing if the columns are consistent
#' with the training data and encoding categorical data into numerical
#' representation in the same way as training data.
#' @param x A data frame of all training predictors.
#' @param categoricalFeatureCols A list of index for all categorical data. Used
#' for trees to detect categorical columns.
#' @param categoricalFeatureMapping A list of encoding details for each
#' categorical column, including all unique factor values and their
#' corresponding numeric representation.
#' @import plyr
#' @return A preprocessed training dataaset x
preprocess_testing <- function(x, categoricalFeatureCols,
                                categoricalFeatureMapping){
  x <- as.data.frame(x)

  # Track the order of all features
  testingFeatureNames <- colnames(x)
  if (is.null(testingFeatureNames)) {
    warning("No names are given for each column.")
  }

  # Track all categorical features (both factors and characters)
  featureFactorCols <- which(sapply(x, is.factor) == TRUE)
  featureCharacterCols <- which(sapply(x, is.character) == TRUE)
  testingCategoricalFeatureCols <- c(featureFactorCols, featureCharacterCols)
  if (length(testingCategoricalFeatureCols) == 0) {
    testingCategoricalFeatureCols <- list()
  } else {
    testingCategoricalFeatureCols <- list(testingCategoricalFeatureCols)
  }

  # TODO:
  # if (length(setdiff(categoricalFeatureCols,
  #                    testingCategoricalFeatureCols)) != 0) {
  #   stop("Categorical columns are different between testing and training data.")
  # }

  # For each categorical feature, encode x into numeric representation
  for (categoricalFeatureMapping_ in categoricalFeatureMapping) {
    categoricalFeatureCol <- categoricalFeatureMapping_$categoricalFeatureCol
    # Get all unique feature values
    testingUniqueFeatureValues <- unique(x[,categoricalFeatureCol])
    uniqueFeatureValues <- categoricalFeatureMapping_$uniqueFeatureValues
    numericFeatureValues <- categoricalFeatureMapping_$numericFeatureValues

    # If testing dataset contains more, adding new factors to the mapping list
    diffUniqueFeatureValues <- setdiff(testingUniqueFeatureValues,
                                       uniqueFeatureValues)
    if (length(diffUniqueFeatureValues) != 0) {
      uniqueFeatureValues <- c(uniqueFeatureValues, diffUniqueFeatureValues)
      numericFeatureValues <- 1:length(uniqueFeatureValues)
    }

    x[,categoricalFeatureCol] <- mapvalues(
      x=x[,categoricalFeatureCol],
      from=uniqueFeatureValues,
      to=numericFeatureValues
    )
  }

  # Return transformed data and encoding information
  return(x)
}

#################################
### Random Forest Constructor ###
#################################
#' @title RF-Constructor
#' @name RF-class
#' @rdname RF-class
#' @description `RF` object implementing the most basic version of a random
#' forest.
#' @slot x A data frame of all training predictors.
#' @slot y A vector of all training responses.
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
#' @slot nodesize The minimum observations contained in terminal nodes. The
#' default value is 5.
#' @slot splitrule A string to specify how to find the best split among all
#' candidate feature values. The current version only supports `variance` which
#' minimizes the overall MSE after splitting. The default value is `variance`.
#' @slot avgfunc An averaging function to average observations in the node. The
#' function is used for prediction. The input of this function should be a
#' dataframe of predictors `x` and a vector of outcomes `y`. The output is a
#' scalar. The default function is to take the mean of vector `y`.
#' @slot forest A list of `RFTree` objects in the forest. If the class is
#' extended, the list may contain the corresponding extended `RFTree` object.
#' @slot categoricalFeatureCols A list of index for all categorical data. Used
#' for trees to detect categorical columns.
#' @slot categoricalFeatureMapping A list of encoding details for each
#' categorical column, including all unique factor values and their
#' corresponding numeric representation.
#' @exportClass RF
setClass(
  Class="RF",
  slots=list(
    x="data.frame",
    y="vector",
    ntree="numeric",
    replace="logical",
    sampsize="numeric",
    mtry="numeric",
    nodesize="list",
    splitrule="character",
    avgfunc="function",
    forest="list",
    categoricalFeatureCols="list",
    categoricalFeatureMapping="list"
  )
)

#' @title RF-Constructor
#' @name RF-RF
#' @rdname RF-RF
#' @description Initialize a `RF` object.
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
#' @param nodesize The minimum observations contained in terminal nodes. The
#' default value is 5.
#' @param nthread The number of threads to use in parallel computing. The
#' default value is 1.
#' @param splitrule A string to specify how to find the best split among all
#' candidate feature values. The current version only supports `variance` which
#' minimizes the overall MSE after splitting. The default value is `variance`.
#' @param avgfunc An averaging function to average observations in the node. The
#' function is used for prediction. The input of this function should be a
#' dataframe of predictors `x` and a vector of outcomes `y`. The output is a
#' scalar. The default function is to take the mean of vector `y`.
#' @export RF
setGeneric(
  name="RF",
  def=function(
    x,
    y,
    ntree,
    replace,
    sampsize,
    mtry,
    nodesize,
    nthread,
    splitrule,
    avgfunc
    ){
    standardGeneric("RF")
  }
)

#' @title RF-Constructor
#' @rdname RF-RF
#' @aliases RF, RF-method
#' @return A `RF` object.
RF <- function(
  x,
  y,
  ntree=500,
  replace=TRUE,
  sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
  mtry=max(floor(ncol(x)/3), 1),
  nodesize=5,
  nthread=1,
  splitrule="variance",
  avgfunc=avgMean
  ){

  # Preprocess the data
  preprocessedData <- preprocess_training(x, y)
  processed_x <- preprocessedData$x
  categoricalFeatureCols <- preprocessedData$categoricalFeatureCols
  categoricalFeatureMapping <- preprocessedData$categoricalFeatureMapping

  # Total number of obervations
  nObservations <- length(y)

  #' @import foreach
  #' @import doParallel
  # Set number of threads for parallelism
  registerDoParallel(nthread)

  # nodesize is forest object is actually a list contains both
  # `averagingNodeSize` and `splittingNodeSize`. In naive random forest
  # implementation, those two will be the same value. In extended version, it
  # can be different to serve for different purposes.
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesize,
    "splittingNodeSize"=nodesize
  )

  # Create trees
  trees <- foreach(i = 1:ntree) %dopar% {

    # Bootstrap sample
    sampleIndex <- sample(
      1:nObservations,
      sampsize,
      replace=replace
      )

    # Create tree object
    return(
      RFTree(
        x=processed_x,
        y=y,
        mtry=mtry,
        nodesize=nodesize,
        sampleIndex=sampleIndex,
        splitrule=splitrule,
        categoricalFeatureCol=categoricalFeatureCols
      )
    )
  }

  # Create a forest object
  forest <- new(
    "RF",
    x=processed_x,
    y=y,
    ntree=ntree,
    replace=replace,
    sampsize=sampsize,
    mtry=mtry,
    nodesize=aggregateNodeSize,
    splitrule=splitrule,
    avgfunc=avgfunc,
    forest=trees,
    categoricalFeatureCols=categoricalFeatureCols,
    categoricalFeatureMapping=categoricalFeatureMapping
    )

  return(forest)
}

######################
### Predict Method ###
######################
#' predict-RF
#' @name predict-RF
#' @rdname predict-RF
#' @description Return the prediction from the forest.
#' @param object A `RF` object.
#' @param feature.new A data frame of testing predictors.
#' @return A vector of predicted responses.
#' @aliases predict, RF-method
#' @exportMethod predict
setMethod(
  f="predict",
  signature="RF",
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

    # Make prediction from each tree
    predForEachTree <- foreach(i = 1:object@ntree, .combine="rbind") %dopar%{
      predict(
        object@forest[[i]],
        processed_x,
        object@x,
        object@y,
        object@avgfunc,
        object@categoricalFeatureCols
        )
    }

    # Aggregate responses from each tree
    if (length(predForEachTree) == nrow(feature.new)) {
      return(predForEachTree)
    }
    return(apply(predForEachTree, 2, mean))
  }
)

