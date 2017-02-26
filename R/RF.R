#################################
### Random Forest Constructor ###
#################################
#' @name RF-class
#' @rdname RF-class
#' @exportClass RF
#' @description `RF` object is an object implementing the most basic version of
#' a random forest.
#' @slot x A data frame or a matrix of all training predictors.
#' @slot y A vector of all training responses.
#' @slot ntree Number of trees to grow. This should not be set to too small to
#' ensure that every input row gets predicted at least a few times. The default
#' value is 500.
#' @slot replace Indicator of whether sampling of cases be done with or without
#' replacement.
#' @slot sampsize Size(s) of sample to draw.
#' @slot mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @slot nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). The default value
#' is 5.
#' @slot splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @slot avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @slot forest A list of `RFTree` in the forest. If the class is extended, the
#' list may contain the corresponding extended `RFTree` object.
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
    forest="list"
  )
)

#' `RF` Constructor
#' @name RF
#' @rdname RF-class
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param ntree Number of trees to grow. This should not be set to too small to
#' ensure that every input row gets predicted at least a few times. The default
#' value is 500.
#' @param replace Indicator of whether sampling of cases be done with or without
#' replacement.
#' @param sampsize Size(s) of sample to draw.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). The default value
#' is 5.
#' @param nthread Number of threads to use in parallel
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @param avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @return A `RF` object.
RF <- function(x, y, ntree=500, replace=TRUE,
               sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
               mtry=max(floor(ncol(x)/3), 1), nodesize=5,
               nthread=1, splitrule="variance", avgfunc=avgMean){

  # Convert data into dataframe
  x <- as.data.frame(x)

  # Total number of obervations
  nObservations <- length(y)

  #' @import foreach
  #' @import doParallel
  # Set number of threads for parallelism
  registerDoParallel(nthread)

  # Create trees
  trees <- foreach(i = 1:ntree) %dopar% {
      # Bootstrap sample
      sampleIndex <- sample(1:nObservations, sampsize, replace=replace)

      return(RFTree(
        x=x, y=y, mtry=mtry, nodesize=nodesize,
        sampleIndex=sampleIndex,
        splitrule=splitrule)
      )
    }

  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesize,
    "splittingNodeSize"=nodesize
  )

  forest <- new(
    "RF", x=x, y=y, ntree=ntree, replace=replace, sampsize=sampsize,
    mtry=mtry, nodesize=aggregateNodeSize,
    splitrule=splitrule, avgfunc=avgfunc, forest=trees)

  return(forest)
}

######################
### Generic Method ###
######################
#' predict-RF
#' @name predict-RF
#' @rdname predict-RF
#' @description Return the prediction from the current forest.
#' @param object RF object.
#' @param feature.new A data frame or a matrix of all testing predictors.
#' @return A vector of predicted responses.
#' @aliases predict, RF-method
setMethod(
  f="predict",
  signature="RF",
  definition=function(object, feature.new){
    predForEachTree <- foreach(i = 1:object@ntree, .combine="rbind") %dopar%{
      predict(object@forest[[i]], feature.new,
              object@x, object@y, object@avgfunc)
    }
    if (length(predForEachTree) == nrow(feature.new)) {
      return(predForEachTree)
    }
    return(apply(predForEachTree, 2, mean))
  }
)

