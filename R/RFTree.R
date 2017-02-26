######################################
### Random Forest Tree Constructor ###
######################################
#' @name RFTree-class
#' @rdname RFTree-class
#' @exportClass RFTree
#' @description `RFTree` is the unit component in the `RF` which composes
#' `RFNode`. The tree uses recursively partitioning to determine the best
#' `splitFeature` and `splitValue` for each level, and recursively split the
#' dataset until it reaches the limitation according to `nodesize`.
#' @slot sampleIndex A list of the index of observations that are used as
#' averaging dataset. The index are based on the original dataset `x` and `y`
#' from forest. Essentially, `x[sampleIndex]` generates the whole splitting
#' dataset.
#' @slot root A `RFNode` object which is the root of the tree. If the class is
#' extended, the list may contain the corresponding extended `RFNode` object.
setClass(
  Class="RFTree",
  slots=list(
    sampleIndex="list",
    root="list"
  )
)

#' `RFTree` Constructor
#' @name RFTree
#' @rdname RFTree-class
#' @description The actual storation of `RFTree` is different from its input.
#' `sampleIndex` and `nodesize` are actually two lists composing of those in
#' splitting dataset and averaging dataset. In the original random forest,
#' those are the same value. For other variant of random forest, such as honest
#' random forest, they can be set differently. The constructor can be
#' overwritten to reflect the change.
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). The default value
#' is 5.
#' @param sampleIndex A list of the index of observations that are used as
#' averaging dataset. The index are based on the original dataset `x` and `y`
#' from forest. Essentially, `x[sampleIndex]` generates the whole splitting
#' dataset.
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @return a `RFTree` object
RFTree <- function(x, y,
                   mtry=max(floor(ncol(x)/3), 1),
                   nodesize=5,
                   sampleIndex=1:length(y),
                   splitrule="variance"){

  # Create the list of nodeSize and sampleIndex
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesize,
    "splittingNodeSize"=nodesize
  )

  aggregateSampleIndex <- list(
    "averagingSampleIndex"=sampleIndex,
    "splittingSampleIndex"=sampleIndex
  )

  # Create a original random forest tree.
  tree <- new("RFTree",
              sampleIndex=aggregateSampleIndex,
              root=list()
              )

  # Grow the tree.
  root <- recursivePartition(
    x=x,
    y=y,
    mtry=mtry,
    nodesize=aggregateNodeSize,
    sampleIndex=aggregateSampleIndex,
    splitrule=splitrule
  )

  tree@root <- list("node"=root)
  return(tree)
}


#######################
### Internal Method ###
#######################
#' selectBestFeature-RFTree
#' @name selectBestFeature-RFTree
#' @rdname selectBestFeature-RFTree
#' @description Find the best `splitfeature`, `splitValue` pair where
#' `splitfeature` is one of the features specified by `featureList`. The
#' `splitFeature` and its corresponding `splitValue` minimizes the specified
#' `splitrule`. The implementation is slightly different from the original
#' implementation as the tree contains both averaging and splitting dataset.
#' To check the minimum split, the method checks for both datasets according to
#' `sampleIndex` and `nodesize`.
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param featureList A list of candidate variables at the current split.
#' @param sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). This parameter is
#' actually a list containing the values for both `splittingNodeSize` and
#' `averagingNodeSize`.
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @return A list of two outputs: "splitFeature" is the best feature to split
#' in order to minimize the split loss, "splitValue" is its corresponding split
#' value.
selectBestFeature <- function(x, y, featureList,
                              sampleIndex=list(
                                "averagingSampleIndex"=1:length(y),
                                "splittingSampleIndex"=1:length(y)
                              ),
                              nodesize=list(
                                "splittingNodeSize"=5,
                                "averagingNodeSize"=5
                              ),
                              splitrule="variance"){

  # Get the number of total features
  mtry <- length(featureList)

  # Initialize the minimum loss for each feature
  bestSplitLossAll <- rep(-Inf, mtry)
  bestSplitValueAll <- rep(NA, mtry)
  bestSplitFeatureAll <- rep(NA, mtry)
  bestSplitCountAll <- rep(0, mtry)

  # Iterate each selected features
  for (i in 1:mtry) {

    currentFeature <- featureList[i]
    allUniqueValues <- sort(unique(
      c(
        x[sampleIndex$splittingSampleIndex, currentFeature],
        x[sampleIndex$averagingSampleIndex, currentFeature]
      )))

    # Test if the all the values for the feature are the same, then proceed
    if (length(allUniqueValues) == 1) next()

    # Keep track of previous feature
    oldFeatureValue <- allUniqueValues[1]

    # Go through all the values in the selected feature
    for (featureValue in allUniqueValues[-1]) {
      # Make partitions on the current feature and value in both splitting
      # and averaging dataset.
      leftPartitionAveraging <-
        x[sampleIndex$averagingSampleIndex, currentFeature] < featureValue
      leftPartitionSplitting <-
        x[sampleIndex$splittingSampleIndex, currentFeature] < featureValue

      leftPartitionCountSplitting <- sum(leftPartitionSplitting)
      rightPartitionCountSplitting <- sum(!leftPartitionSplitting)
      leftPartitionCountAveraging <- sum(leftPartitionAveraging)
      rightPartitionCountAveraging <- sum(!leftPartitionAveraging)

      # Check leaf size at least nodesize
      if (
        min(leftPartitionCountSplitting,
            rightPartitionCountSplitting) < nodesize$splittingNodeSize |
        min(leftPartitionCountAveraging,
            rightPartitionCountAveraging) < nodesize$averagingNodeSize){
        # Update the oldFeature value before proceeding
        oldFeatureValue <- featureValue
        next()
      }

      # Calculate sample mean in both splitting partitions
      leftPartitionMean <-
        mean(y[sampleIndex$splittingSampleIndex][leftPartitionSplitting])
      rightPartitionMean <-
        mean(y[sampleIndex$splittingSampleIndex][!leftPartitionSplitting])

      # Calculate the variance of the splitting
      muBarSquareSum <- leftPartitionCountSplitting * leftPartitionMean ^ 2 +
        rightPartitionCountSplitting * rightPartitionMean ^ 2

      # Update the value if a higher value has been seen
      if (muBarSquareSum > bestSplitLossAll[i]) {
        bestSplitLossAll[i] <- muBarSquareSum
        bestSplitFeatureAll[i] <- currentFeature
        bestSplitValueAll[i] <- runif(1, oldFeatureValue, featureValue)
        bestSplitCountAll[i] <- 1
      } else {
        # If we are as good as the best split
        if (muBarSquareSum == bestSplitLossAll[i]) {
          bestSplitCountAll[i] <- bestSplitCountAll[i] + 1
          # Only update with probability 1/nseen
          if (runif(1, 0, bestSplitCountAll[i]) <= 1){
            bestSplitLossAll[i] <- muBarSquareSum
            bestSplitFeatureAll[i] <- currentFeature
            bestSplitValueAll[i] <- runif(1, oldFeatureValue, featureValue)
          }
        }
      }
      # Update the old feature value
      oldFeatureValue <- featureValue
    }
  }

  # Get the best split values among all features
  bestSplitLoss <- max(bestSplitLossAll)
  bestFeatureIndex <- which(bestSplitLossAll == bestSplitLoss)

  # If we found a feasible splitting point
  if (sum(bestSplitCountAll) > 0) {
    # If there are multiple best features, sample one according to their
    # frequency of occurence
    elements_to_sample <- rep(bestFeatureIndex,
                              bestSplitCountAll[bestFeatureIndex])
    bestFeatureIndex <- ifelse(length(elements_to_sample) == 1,
                               elements_to_sample,
                               sample(elements_to_sample, 1))
  }else{
    # If none of the features are possible, return NA
    return(list("bestSplitFeature"=NA, "bestSplitValue"=NA))
  }

  # Return the best splitFeature and splitValue
  return(list("bestSplitFeature"=bestSplitFeatureAll[bestFeatureIndex],
              "bestSplitValue"=bestSplitValueAll[bestFeatureIndex]))
}

#' recursivePartition-RFTree
#' @name recursivePartition-RFTree
#' @rdname recursivePartition-RFTree
#' @description Grow the decision tree by recursively finding the best split
#' feature and value.
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @param sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). This parameter is
#' actually a list containing the values for both `splittingNodeSize` and
#' `averagingNodeSize`.
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @return root node.
recursivePartition <- function(x, y,
                               mtry=max(floor(ncol(x)/3), 1),
                               sampleIndex=list(
                                 "averagingSampleIndex"=1:length(y),
                                 "splittingSampleIndex"=1:length(y)
                               ),
                               nodesize=list(
                                 "splittingNodeSize"=5,
                                 "averagingNodeSize"=5
                               ),
                               splitrule="variance"){

  # Sample features for the split
  selectedFeatureIndex <- sample(1:ncol(x), mtry)

  # Load bestSplitFeature, bestSplitValue to the current environment
  list2env(
    selectBestFeature(
      x=x,
      y=y,
      featureList=selectedFeatureIndex,
      sampleIndex=sampleIndex,
      nodesize=nodesize,
      splitrule=splitrule),
    environment())

  # Create a leaf node if the current bestSplitValue is NA
  if (is.na(bestSplitValue)) {
    return(RFNode(sampleIndex=sampleIndex))

  } else {

    # Create a tree node
    # Update sample index for both left and right partitions
    leftPartitionSplitting <-
      x[sampleIndex$splittingSampleIndex,
        bestSplitFeature] < bestSplitValue
    leftPartitionAveraging <-
      x[sampleIndex$averagingSampleIndex,
        bestSplitFeature] < bestSplitValue

    sampleIndex_left <- list(
      "splittingSampleIndex"=
        sampleIndex$splittingSampleIndex[leftPartitionSplitting],
      "averagingSampleIndex"=
        sampleIndex$averagingSampleIndex[leftPartitionAveraging]
    )
    sampleIndex_right <- list(
      "splittingSampleIndex"=
        sampleIndex$splittingSampleIndex[!leftPartitionSplitting],
      "averagingSampleIndex"=
        sampleIndex$averagingSampleIndex[!leftPartitionAveraging]
    )

    # Recursively grow the tree
    leftChild <- recursivePartition(
      x, y, mtry=mtry, sampleIndex=sampleIndex_left, nodesize=nodesize,
      splitrule=splitrule
      )

    rightChild <- recursivePartition(
      x, y, mtry=mtry, sampleIndex=sampleIndex_right, nodesize=nodesize,
      splitrule=splitrule
      )

    # Create the leaf node the connects to both children
    node <- RFNode(
      splitFeature=bestSplitFeature,
      splitValue=bestSplitValue,
      child=list(
        "leftChild"=leftChild,
        "rightChild"=rightChild
      )
    )
    return(node)
  }
}


######################
### Generic Method ###
######################
#' predict-RFTree
#' @name predict-RFTree
#' @rdname predict-RFTree
#' @description Return the prediction from the current tree.
#' @param object RFTree object.
#' @param feature.new A data frame or a matrix of all testing predictors.
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @return A vector of predicted responses.
#' @aliases predict, RFTree-method
setMethod(
  f="predict",
  signature="RFTree",
  definition=function(object, feature.new, x, y, avgfunc){
    return(predict(object@root$node, feature.new, x, y, avgfunc))
  }
)

#' showTree-RFTree
#' @name showTree-RFTree
#' @rdname showTree-RFTree
#' @description Print the entire tree starting from the root node
#' @param object RFTree object
#' @exportMethod showTree
setGeneric(
  name="showTree",
  def=function(object){
    standardGeneric("showTree")
  }
)

#' @rdname showTree-RFTree
#' @aliases showTree, RFTree-method
setMethod(
  f="showTree",
  signature="RFTree",
  definition = function(object){
    printnode(object@root$node)
  }
)
