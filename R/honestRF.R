########################################
### Honest Random Forest Constructor ###
########################################
#' @name honstRF-class
#' @rdname honestRF-class
#' @exportClass honestRF
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
#' @slot nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
setClass(
  Class="honestRF",
  slots=list(
    splitratio="numeric"
  ),
  contains="RF"
)

#' honestRF Constructor
#' @name honestRF
#' @rdname honestRF-class
honestRF <- function(x, y, ntree=500, replace=TRUE,
                     sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
                     mtry=max(floor(ncol(x)/3), 1), nodesize=5,
                     maxnodes=Inf, nthread=1, splitrule="variance",
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
      # Sample without replacement (bootstrap sample)
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
        maxnodes=maxnodes,
        sampleIndex=list(
          "averagingSampleIndex"=averagingSampledIndex,
          "splittingSampleIndex"=splittingSampledIndex
        ),
        splitrule=splitrule,
        avgfunc=avgfunc)
      )
    }

  forest <- new("honestRF", x=x, y=y, ntree=ntree, replace=replace,
                sampsize=sampsize,
                mtry=mtry, nodesize=aggregateNodeSize,
                maxnodes=maxnodes,
                splitrule=splitrule, avgfunc=avgfunc, forest=trees,
                splitratio=splitratio)
  return(forest)
}

