########################################
### Honest Random Forest Constructor ###
########################################
#' @name honstRF-class
#' @rdname honestRF-class
#' @description `honestRF` inherits `RF`, which serves as a modified version of
#' `RF`. The major change is that it trains honest trees instead of adaptive
#' trees. Adaptive trees are the original trees as they were used in the
#' original implementation and honest trees differ in that they require two
#' data sets to do tree estimation. One data set is used to create the trees
#' and the other one is used to receive the leaf estimates.
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
    splitratio="numeric"
  ),
  contains="RF"
)

#' @export honestRF
setGeneric(
  name="honestRF",
  def=function(x, y, ntree, replace,
               sampsize,
               mtry, nodesize,
               nthread, splitrule,
               avgfunc, splitratio, nodesizeAvg){
    standardGeneric("honestRF")
  }
)
#' honestRF Constructor
#' @name honestRF
#' @rdname honestRF-class
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
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
#' @return A `honestRF` object.
honestRF <- function(x, y, ntree=500, replace=TRUE,
                     sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
                     mtry=max(floor(ncol(x)/3), 1), nodesize=5,
                     nthread=1, splitrule="variance",
                     avgfunc=avgMean, splitratio=1, nodesizeAvg=5){

  # Convert data into dataframe
  x <- as.data.frame(x)

  # Total number of obervations
  nObservations <- length(y)

  # Set number of threads for parallelism
  registerDoParallel(nthread)

  # Create a list of minimum node size
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesizeAvg,
    "splittingNodeSize"=nodesize
  )

  trees <- foreach(i = 1:ntree) %dopar%{
    # Bootstrap sample
    sampledIndex <- sample(1:nObservations, sampsize, replace=replace)
    splittingSamples <- sample(
      1:length(sampledIndex),
      as.integer(sampsize * splitratio)
    )
    splittingSampledIndex <- sampledIndex[splittingSamples]
    # If splitratio is 1, set averagingSampledIndex to be the same as
    # splittingSampledIndex
    if (splitratio == 1) {
      averagingSampledIndex <- splittingSampledIndex
    } else {
      averagingSampledIndex <- sampledIndex[-splittingSamples]
    }

    return(honestRFTree(
        x=x,
        y=y,
        mtry=mtry,
        nodesize=aggregateNodeSize,
        sampleIndex=list(
          "averagingSampleIndex"=averagingSampledIndex,
          "splittingSampleIndex"=splittingSampledIndex
        ),
        splitrule=splitrule)
      )
    }

  forest <- new("honestRF", x=x, y=y, ntree=ntree, replace=replace,
                sampsize=sampsize,
                mtry=mtry, nodesize=aggregateNodeSize,
                splitrule=splitrule, avgfunc=avgfunc, forest=trees,
                splitratio=splitratio)
  return(forest)
}

