#################################
### Random Forest Constructor ###
#################################
setClassUnion("listORnumeric",members=c("list", "numeric"))

#' @name RF-class
#' @rdname RF-class
#' @exportClass RF
#' @description `RF` object is an object implementing the most basic version of
#' a random forest.
#' @slot x A data frame or a matrix of all predictors.
#' @slot y A response vector.
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
#' @slot maxnodes Maximum number of terminal nodes trees in the forest can have.
#' If not given, trees are grown to the maximum possible (subject to limits by
#' nodesize).
#' @slot nthread Number of threads to use in parallel
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
    nodesize="listORnumeric",
    maxnodes="numeric",
    nthread="numeric",
    splitrule="character",
    avgfunc="function",
    forest="list"
  )
)

#' `RF`` Constructor
#' @name RF
#' @rdname RF-class
RF <- function(x, y, ntree=500, replace=TRUE,
               sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
               mtry=max(floor(ncol(x)/3), 1), nodesize=5,
               maxnodes=Inf, nthread=1, splitrule="variance", avgfunc=avgMean){
  # Convert data into dataframe
  x <- as.data.frame(x)
  # Total number of obervations
  nObservations <- length(y)

  # Set number of threads for parallelism
  registerDoParallel(nthread)

  # Create trees
  trees <- foreach(i = 1:ntree) %dopar% {
      # Sample without replacement (bootstrap sample)
      sampleIndex <- sample(1:nObservations, sampsize, replace=replace)
      return(RFTree(
        x=x, y=y, mtry=mtry, nodesize=nodesize,
        maxnodes=maxnodes, sampleIndex=sampleIndex,
        splitrule=splitrule, avgfunc=avgfunc)
      )
    }

  forest <- new("RF", x=x, y=y, ntree=ntree, replace=replace, sampsize=sampsize,
                mtry=mtry, nodesize=nodesize, maxnodes=maxnodes,
                splitrule=splitrule, avgfunc=avgfunc, forest=trees)
  return(forest)
}

######################
### Generic Method ###
######################
#' predict-RF
#' @name predict-RF
#' @rdname predict-RF
#' @description Call predict for all trees and average the predictions among all
#' the trees.
#' @param theObject RFTree object
#' @param feature.new A data frame or a matrix of all testing predictors
#' @exportMethod predict
if (!isGeneric("predict")) {
  setGeneric(
    name="predict",
    def=function(theObject, feature.new){
      standardGeneric("predict")
    }
  )
}

#' @rdname predict-RF
#' @aliases predict, RF-method
setMethod(
  f="predict",
  signature="RF",
  definition=function(theObject, feature.new){
    predForEachTree <- foreach(i = 1:theObject@ntree, .combine="rbind") %dopar%{
      predict(theObject@forest[[i]], feature.new)
    }
    if (length(predForEachTree) == nrow(feature.new)) {
      return(predForEachTree)
    }
    return(apply(predForEachTree, 2, mean))
  }
)

