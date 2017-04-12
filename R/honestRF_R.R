########################################
### Honest Random Forest Constructor ###
########################################
#' @title honestRF_R Constructor
#' @name honestRF_R-class
#' @rdname honestRF_R-class
#' @description `honestRF_R` inherits `RF`, which serves as a modified version of
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
#' @exportClass honestRF_R
setClass(
  Class="honestRF_R",
  slots=list(
    splitratio="numeric"
  ),
  contains="RF"
)


#' @title honestRF_R Constructor
#' @name honestRF_R-honestRF_R
#' @rdname honestRF_R-honestRF_R
#' @description Initialize a `honestRF_R` object.
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
#' @param nthread The number of threads to use in parallel computing. The
#' default value is 1.
#' @param splitrule A string to specify how to find the best split among all
#' candidate feature values. The current version only supports `variance` which
#' minimizes the overall MSE after splitting. The default value is `variance`.
#' @param avgfunc An averaging function to average observations in the node. The
#' function is used for prediction. The input of this function should be a
#' dataframe of predictors `x` and a vector of outcomes `y`. The output is a
#' scalar. The default function is to take the mean of vector `y`.
#' @param splitratio Proportion of the training data used as the splitting
#' dataset. It is a ratio between 0 and 1. If the ratio is 1, then essentially
#' splitting dataset becomes the total entire sampled set and the averaging
#' dataset is empty. If the ratio is 0, then the splitting data set is empty
#' and all the data is used for the averaging data set (This is not a good
#' usage however since there will be no data available for splitting).
#' @param nodesizeAvg Minimum size of terminal nodes for averaging dataset.
#' The default value is 5.
#' @export honestRF_R
setGeneric(
  name="honestRF_R",
  def=function(
    x,
    y,
    ntree,
    replace,
    sampsize,
    mtry,
    nodesizeSpl,
    nthread,
    splitrule,
    avgfunc,
    splitratio,
    nodesizeAvg
    ){
    standardGeneric("honestRF_R")
  }
)

#' @title honestRF_R Constructor
#' @rdname honestRF_R-honestRF_R
#' @aliases honestRF_R, honestRF_R-method
#' @return A `honestRF_R` object.
honestRF_R <- function(
  x,
  y,
  ntree=500,
  replace=TRUE,
  sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x)),
  mtry=max(floor(ncol(x)/3), 1),
  nodesizeSpl=5,
  nthread=1,
  splitrule="variance",
  avgfunc=avgMean,
  splitratio=1,
  nodesizeAvg=5
  ){

  # Preprocess the data
  preprocessedData <- preprocess_training(x, y)
  x <- preprocessedData$x
  categoricalFeatureCols <- preprocessedData$categoricalFeatureCols
  categoricalFeatureMapping <- preprocessedData$categoricalFeatureMapping

  # Total number of obervations
  nObservations <- length(y)

  # @import foreach
  # @import doParallel
  # Set number of threads for parallelism
  registerDoParallel(nthread)

  # Create a list of minimum node size
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesizeAvg,
    "splittingNodeSize"=nodesizeSpl
  )

  # Create trees
  trees <- foreach(i = 1:ntree) %dopar%{

    # Bootstrap sample observation index
    sampledIndex <- sample(
      1:nObservations,
      sampsize,
      replace=replace
      )

    # Sample splitting index from the sampled observation index
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

    return(
      honestRFTree(
        x=x,
        y=y,
        mtry=mtry,
        nodesize=aggregateNodeSize,
        sampleIndex=list(
          "averagingSampleIndex"=averagingSampledIndex,
          "splittingSampleIndex"=splittingSampledIndex
        ),
        splitrule=splitrule,
        categoricalFeatureCol=categoricalFeatureCols
      )
    )
  }

  # Create a forest object
  forest <- new(
    "honestRF",
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
    categoricalFeatureCols=categoricalFeatureCols,
    categoricalFeatureMapping=categoricalFeatureMapping,
    splitratio=splitratio
  )

  return(forest)
}
