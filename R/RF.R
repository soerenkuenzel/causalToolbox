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
#' @slot encodingLabels A list contains all encodings that are used to encode
#' categorical columns in the training data.
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
    encodingLabels="list"
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

#' @title Preprocess Data
#' @name preprocessing_data
#' @description A simple preprocessing function to convert `x` to dataframe,
#' and also convert categorical data into one-hot encoding.
#' @param x A data frame of all predictors.
#' @param labels A list contains all encodings that are used to encode
#' categorical columns in the training data. When the function is called for
#' training dataset, the labels should be set to NULL. When the function is
#' called for testing dataset, the labels must be provided in order to
#' successfully convert the data into the format that is consistent to training
#' data.
#' @return A list of the transformed object `x`, and encoding information
#' `labels`.
preprocessing_data <- function(x, labels=NULL) {
  # Convert data into dataframe
  x <- as.data.frame(x)

  if (is.null(labels)){
    # Extract all categorical column index
    catColumns <- which(sapply(x, is.factor) == TRUE)
    if (length(catColumns) == 0){
      # No need to encode
      return(
        list(
          x=x,
          labels=list()
        )
      )
    }
    # Apply one-hot encoding on training dataset
    x_ <- model.matrix(
      ~ . + 0,
      data=x[catColumns],
      contrasts.arg=lapply(x[catColumns], contrasts, contrasts=FALSE)
    )
    # Delete old columns and add dummy features inside
    x <- cbind(x[-catColumns], as.data.frame(x_))
    # Create labels as a list to store the name of the order of feature
    # encoding that is used
    labels <- attr(x_,"contrasts")
    for (category in names(labels)){
      labels[[category]] <- colnames(labels[[category]])
    }
  } else {
    # Testing dataset, apply labels
    catColumns <- which(sapply(x, is.factor) == TRUE)
    if (length(catColumns) == 0){
      # No need to encode
      return(
        list(
          x=x,
          labels=list()
        )
      )
    }
    # Intiailize a dataframe to hold dummy features
    x_ <- data.frame(matrix(data=NA, nrow=nrow(x), ncol=0))
    # Go through all categorical features
    for (category in names(labels)){
      categoryNames <- labels[[category]]
      # Create one-hot encoding for each categorical value
      for (categoryName in categoryNames){
        dummyCol <- as.integer(x[category] == categoryName)
        dummyColName <- paste(category, categoryName, sep="")
        x_ <- cbind(x_, dummyCol)
        colnames(x_)[length(colnames(x_))] <- dummyColName
      }
    }
    x <- cbind(x[-catColumns], as.data.frame(x_))
  }

  return(
    list(
      x=x,
      labels=labels
    )
  )
}

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
  preprocessed <- preprocessing_data(x)
  x <- preprocessed$x
  encodingLabels <- preprocessed$labels

  # Total number of obervations
  nObservations <- length(y)

  #' @import foreach
  #' @import doParallel
  # Set number of threads for parallelism
  registerDoParallel(nthread)

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
          x=x,
          y=y,
          mtry=mtry,
          nodesize=nodesize,
          sampleIndex=sampleIndex,
          splitrule=splitrule
        )
      )
    }

  # nodesize is forest object is actually a list contains both
  # `averagingNodeSize` and `splittingNodeSize`. In naive random forest
  # implementation, those two will be the same value. In extended version, it
  # can be different to serve for different purposes.
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesize,
    "splittingNodeSize"=nodesize
  )

  # Create a forest object
  forest <- new(
    "RF",
    x=x,
    y=y,
    ntree=ntree,
    replace=replace,
    sampsize=sampsize,
    mtry=mtry,
    nodesize=aggregateNodeSize,
    splitrule=splitrule,
    avgfunc=avgfunc,
    forest=trees,
    encodingLabels=encodingLabels
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
    preprocessed <- preprocessing_data(feature.new, object@encodingLabels)
    x <- preprocessed$x

    # Make prediction from each tree
    predForEachTree <- foreach(i = 1:object@ntree, .combine="rbind") %dopar%{
      predict(
        object@forest[[i]],
        x,
        object@x,
        object@y,
        object@avgfunc
        )
    }

    # Aggregate responses from each tree
    if (length(predForEachTree) == nrow(feature.new)) {
      return(predForEachTree)
    }
    return(apply(predForEachTree, 2, mean))
  }
)

